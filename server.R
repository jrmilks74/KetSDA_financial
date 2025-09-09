# ---- server.R ----

# Packages
library(shiny)
library(googlesheets4)
library(tidyverse)
library(plotly)
library(scales)
library(fpp3)
library(urca)
library(kableExtra)
library(rvest)
library(lubridate)
library(tsibble)
library(htmltools)

options(scipen = 999)
options(
        gargle_oauth_email = TRUE,
        gargle_oauth_cache = "auth/.secrets/"
)
gs4_auth(email = "office@ketsda.org")

# ========== Load Data ==========
# Financials (Google Sheet)
Finances <- read_sheet(
        "https://docs.google.com/spreadsheets/d/1DPq4lM36_CcjEGN2v8UE1w1pr-t0C8lenYx56jniNRo/edit#gid=0",
        col_types = "Dnnn"
) |>
        mutate(Deficit = Income - Expenses)

# Ministry funds (Google Sheet)
Ministry_funds <- read_sheet(
        "https://docs.google.com/spreadsheets/d/1d2g7NRipbarvq78LUxuEObWsTru4t4w_TY9uQgQOvZQ/edit#gid=0"
)

# Midwest CPI-U (CUUR0200SA0) from FRED
fred_page   <- read_html("https://fred.stlouisfed.org/data/CUUR0200SA0.txt")
fred_tables <- html_nodes(fred_page, "table")
stopifnot(length(fred_tables) >= 2)
table_content <- html_table(fred_tables)[[2]]

MidwestCPI <- table_content |>
        mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) |>
        mutate(across(where(is.character), as.numeric)) |>
        filter(DATE >= as.Date("2000-01-01")) |>
        rename(Date = DATE, CPI = VALUE)

# Join CPI and compute real dollars
Finances <- Finances |>
        inner_join(MidwestCPI, by = "Date") |>
        arrange(Date) |>
        mutate(
                Year        = year(Date),
                Month       = month(Date),
                Inflation   = CPI / first(CPI),
                AdjTithe    = Tithe    / Inflation,
                AdjIncome   = Income   / Inflation,
                AdjExpenses = Expenses / Inflation,
                AdjDeficit  = Deficit  / Inflation
        )

# Long/Yearly forms
Finances_long <- Finances |>
        select(Date, Year, Month,
               Tithe, AdjTithe, Income, AdjIncome, Expenses, AdjExpenses, Deficit, AdjDeficit) |>
        pivot_longer(-c(Date, Year, Month), names_to = "Category", values_to = "Amount")

Yearly_finances <- Finances_long |>
        group_by(Year, Category) |>
        summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop") |>
        pivot_wider(id_cols = Year, names_from = Category, values_from = Total)

Finances_wide <- Finances |>
        select(Date, Year, Month,
               Tithe, Income, Expenses, Deficit,
               AdjTithe, AdjIncome, AdjExpenses, AdjDeficit)

# tsibbles for fpp3
Finances_tsbl <- Finances_wide |>
        transmute(Year, Month, Date = yearmonth(Date),
                  Tithe, Income, Expenses) |>
        as_tsibble(index = Date)

current_year <- year(last(Finances_wide$Date))

Finances_tsbl_past <- Finances_wide |>
        filter(Year < current_year) |>
        transmute(Year, Month, Date = yearmonth(Date),
                  Tithe, Income, Expenses) |>
        as_tsibble(index = Date)

# ========== Helpers ==========
nz_divide <- function(num, den) {
        if (is.na(den) || den == 0) return(NA_real_)
        num / den
}
or0 <- function(x) if (length(x) == 0 || all(is.na(x))) 0 else x
fmt_money <- function(x) if (length(x) == 0) dollar(NA_real_) else dollar(x)

build_overview <- function(fin_wide) {
        last_row <- fin_wide |> arrange(Date) |> slice_tail(n = 1)
        last_ym  <- yearmonth(last_row$Date)
        prev_ym  <- last_ym - 12
        
        last_month_name <- month(as.Date(last_ym), label = TRUE, abbr = FALSE) |> as.character()
        prev_row <- fin_wide |> filter(yearmonth(Date) == prev_ym) |> slice_tail(n = 1)
        
        # Latest month vs same month last year
        mtd_df <- tibble::tibble(
                Metric  = c("Tithe", "Income", "Expenses", "Surplus/Deficit"),
                Current = c(last_row$Tithe, last_row$Income, last_row$Expenses, last_row$Deficit),
                Prior   = c(prev_row$Tithe, prev_row$Income, prev_row$Expenses, prev_row$Deficit)
        ) |>
                mutate(
                        Change    = Current - Prior,
                        PctChange = map2_dbl(Change, Prior, nz_divide),
                        across(c(Current, Prior, Change), fmt_money),
                        PctChange = percent(PctChange, accuracy = 0.1)
                )
        
        # YTD through same month (CY vs LY)
        last_month_num <- month(as.Date(last_ym))
        ytd_sums <- fin_wide |>
                filter(Year %in% c(current_year - 1, current_year), Month <= last_month_num) |>
                summarise(
                        Tithe_ly    = sum(Tithe[Year == current_year - 1], na.rm = TRUE),
                        Income_ly   = sum(Income[Year == current_year - 1], na.rm = TRUE),
                        Expenses_ly = sum(Expenses[Year == current_year - 1], na.rm = TRUE),
                        Deficit_ly  = sum(Deficit[Year == current_year - 1], na.rm = TRUE),
                        Tithe_cy    = sum(Tithe[Year == current_year], na.rm = TRUE),
                        Income_cy   = sum(Income[Year == current_year], na.rm = TRUE),
                        Expenses_cy = sum(Expenses[Year == current_year], na.rm = TRUE),
                        Deficit_cy  = sum(Deficit[Year == current_year], na.rm = TRUE)
                )
        
        ytd_df <- tibble::tibble(
                Metric  = c("Tithe", "Income", "Expenses", "Surplus/Deficit"),
                Current = c(ytd_sums$Tithe_cy, ytd_sums$Income_cy, ytd_sums$Expenses_cy, ytd_sums$Deficit_cy),
                Prior   = c(ytd_sums$Tithe_ly, ytd_sums$Income_ly, ytd_sums$Expenses_ly, ytd_sums$Deficit_ly)
        ) |>
                mutate(
                        Change    = Current - Prior,
                        PctChange = map2_dbl(Change, Prior, nz_divide),
                        across(c(Current, Prior, Change), fmt_money),
                        PctChange = percent(PctChange, accuracy = 0.1)
                )
        
        list(mtd = mtd_df, ytd = ytd_df, last_month_name = last_month_name)
}

ov <- build_overview(Finances_wide)

# ========== Server ==========
function(input, output, session) {
        
        # --- Overview tables (HTML so kableExtra styles render)
        output$mtdFinances <- renderUI({
                html <- kbl(
                        ov$mtd,
                        col.names = c(
                                "Metric",
                                paste0(ov$last_month_name, " (CY)"),
                                paste0(ov$last_month_name, " (LY)"),
                                "Change",
                                "Percent change"
                        )
                ) |>
                        kable_styling(bootstrap_options = c("striped"), full_width = FALSE) |>
                        column_spec(1, bold = TRUE)
                HTML(html)
        })
        
        output$ytdFinances <- renderUI({
                html <- kbl(
                        ov$ytd,
                        col.names = c(
                                "Metric",
                                paste0(current_year, " to date"),
                                paste0(current_year - 1, " to date"),
                                "Change",
                                "Percent change"
                        )
                ) |>
                        kable_styling(bootstrap_options = c("striped"), full_width = FALSE) |>
                        column_spec(1, bold = TRUE)
                HTML(html)
        })
        
        # --- In-Depth reactives
        Finances_monthly <- reactive({
                req(input$Category)
                nm <- switch(input$Category,
                             "Tithe"    = "Tithe",
                             "Income"   = "Income",
                             "Expenses" = "Expenses"
                )
                Finances_wide |> select(Date, Current = all_of(nm))
        })
        
        Finances_yearly <- reactive({
                req(input$Category)
                cur <- switch(input$Category,
                              "Tithe"    = c("Tithe", "AdjTithe"),
                              "Income"   = c("Income", "AdjIncome"),
                              "Expenses" = c("Expenses", "AdjExpenses")
                )
                Yearly_finances |>
                        select(Year, Current = all_of(cur[1]), Adjusted = all_of(cur[2])) |>
                        filter(Year < current_year)
        })
        
        Seasonal_dependent <- reactive({
                req(input$Category)
                nm <- switch(input$Category,
                             "Tithe"    = "Tithe",
                             "Income"   = "Income",
                             "Expenses" = "Expenses"
                )
                Finances_wide |> select(Date, Amount = all_of(nm))
        })
        
        Seasonal_inflation_dependent <- reactive({
                req(input$Category)
                nm <- switch(input$Category,
                             "Tithe"    = "AdjTithe",
                             "Income"   = "AdjIncome",
                             "Expenses" = "AdjExpenses"
                )
                Finances_wide |> select(Date, Amount = all_of(nm))
        })
        
        selected_tsibble <- reactive({
                req(input$Category)
                nm <- switch(input$Category,
                             "Tithe"    = "Tithe",
                             "Income"   = "Income",
                             "Expenses" = "Expenses"
                )
                Finances_tsbl |> select(Date, Current = all_of(nm))
        })
        
        selected_tsibble2 <- reactive({
                req(input$Category)
                nm <- switch(input$Category,
                             "Tithe"    = "Tithe",
                             "Income"   = "Income",
                             "Expenses" = "Expenses"
                )
                Finances_tsbl_past |> select(Date, Current = all_of(nm))
        })
        
        # --- FIXED: YTD (used in the totals table) — sum only current-year rows
        current_year_cat <- reactive({
                req(input$Category)
                nm <- switch(input$Category,
                             "Tithe"    = "Tithe",
                             "Income"   = "Income",
                             "Expenses" = "Expenses"
                )
                Finances_wide |>
                        dplyr::filter(Year == current_year) |>
                        dplyr::summarise(val = sum(.data[[nm]], na.rm = TRUE)) |>
                        dplyr::pull(val) %||% 0
        })
        
        last_month_cat <- reactive({
                req(input$Category)
                nm <- switch(input$Category,
                             "Tithe"    = "Tithe",
                             "Income"   = "Income",
                             "Expenses" = "Expenses"
                )
                Finances_wide |>
                        arrange(Date) |>
                        slice_tail(n = 1) |>
                        pull(all_of(nm))
        })
        
        # --- Plots
        output$monthly_time_graph <- renderPlotly({
                df <- Finances_monthly()
                p <- ggplot(df, aes(x = Date, y = Current)) +
                        theme_classic() +
                        geom_line() +
                        geom_smooth(method = "loess", formula = y ~ x) +
                        labs(y = "Dollars", x = "Year")
                ggplotly(p)
        })
        
        output$annual_time_graph <- renderPlotly({
                df <- Finances_yearly()
                p <- ggplot(df, aes(x = Year)) +
                        theme_classic() +
                        geom_line(aes(y = Current, colour = "Current dollars")) +
                        geom_smooth(aes(y = Current, colour = "Current dollars"),
                                    method = "loess", formula = y ~ x) +
                        geom_line(aes(y = Adjusted, colour = "Inflation-adjusted")) +
                        geom_smooth(aes(y = Adjusted, colour = "Inflation-adjusted"),
                                    method = "loess", formula = y ~ x) +
                        labs(y = "Dollars", x = "Year")
                ggplotly(p)
        })
        
        output$seasonal_graph <- renderPlotly({
                df <- Seasonal_dependent()
                this_year     <- current_year
                previous_year <- current_year - 1
                p <- df |>
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) |>
                        ggplot(aes(x = Date, y = Amount, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = ~ dplyr::filter(.x, Year == previous_year), linewidth = 0.5) +
                        geom_line(data = ~ dplyr::filter(.x, Year == this_year), linewidth = 0.75) +
                        theme_bw() +
                        labs(y = "Current Dollars", x = "Month")
                ggplotly(p)
        })
        
        output$seasonal_inflation_graph <- renderPlotly({
                df <- Seasonal_inflation_dependent()
                this_year     <- current_year
                previous_year <- current_year - 1
                p <- df |>
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) |>
                        ggplot(aes(x = Date, y = Amount, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = ~ dplyr::filter(.x, Year == previous_year), linewidth = 0.5) +
                        geom_line(data = ~ dplyr::filter(.x, Year == this_year), linewidth = 0.75) +
                        theme_bw() +
                        labs(y = "Real Dollars (Jan 2000 index = 1.00)", x = "Month")
                ggplotly(p)
        })
        
        # Static forecast (UI uses plotOutput here)
        output$forecast_graph <- renderPlot({
                tsb <- selected_tsibble()
                fit <- tsb |> model(ARIMA(Current))
                fc  <- forecast(fit, h = 12)
                autoplot(fc, tsb) +
                        theme_bw() +
                        labs(y = "Dollars", title = "Forecast (next 12 months)")
        })
        
        # "January Forecast vs Actual" (train through prior year end)
        output$forcast_vs_actual <- renderPlotly({
                tsb2 <- selected_tsibble2()
                fit2 <- tsb2 |> model(ARIMA(Current))
                fc2  <- forecast(fit2, h = 12)
                
                annual_forecast <- fc2 |>
                        as_tibble() |>
                        transmute(Date = as.Date(Date), forecast = .mean)
                
                actual <- Finances_monthly() |> filter(year(Date) >= current_year)
                
                p <- ggplot() +
                        theme_bw() +
                        geom_line(data = annual_forecast, aes(x = Date, y = forecast, colour = "forecast")) +
                        geom_point(data = annual_forecast, aes(x = Date, y = forecast, colour = "forecast")) +
                        geom_line(data = actual, aes(x = Date, y = Current, colour = "actual")) +
                        geom_point(data = actual, aes(x = Date, y = Current, colour = "actual")) +
                        labs(title = "January Forecast vs Actual", x = "Date", y = "Dollars")
                ggplotly(p)
        })
        
        # --- Tables
        output$total_amount_table <- renderTable({
                tsb <- selected_tsibble()
                fit <- tsb |> model(ARIMA(Current))
                fc  <- forecast(fit, h = 12)
                
                # Last observed monthly date from the tsibble
                last_obs_date <- as.Date(last(tsb$Date))
                cutoff        <- as.Date(paste0(year(last_obs_date), "-12-31"))
                
                # Forecasted amount for the rest of the current calendar year
                forecasted_amount <- fc |>
                        as_tibble() |>
                        transmute(Date = as.Date(Date), forecast = .mean) |>
                        filter(Date > last_obs_date, Date <= cutoff) |>
                        summarise(total = sum(forecast, na.rm = TRUE)) |>
                        pull(total) %||% 0
                
                # ✅ Correct YTD: sum only the current year from the selected monthly series
                ytd_amount <- Finances_monthly() |>
                        dplyr::filter(lubridate::year(Date) == current_year) |>
                        dplyr::summarise(val = sum(Current, na.rm = TRUE)) |>
                        dplyr::pull(val) %||% 0
                
                predicted_total <- ytd_amount + forecasted_amount
                
                tibble::tibble(
                        Metric = c(
                                "Last month",
                                "Year-to-date",
                                "Predicted rest of year",
                                "Predicted total for current year"
                        ),
                        Amount = c(
                                fmt_money(or0(last_month_cat())),
                                fmt_money(ytd_amount),
                                fmt_money(forecasted_amount),
                                fmt_money(predicted_total)
                        )
                )
        }, rownames = FALSE)
        
        output$forecast_table <- renderTable({
                tsb <- selected_tsibble()
                fit <- tsb |> model(ARIMA(Current))
                fc  <- forecast(fit, h = 12)
                
                hilo(fc) |>
                        as_tibble() |>
                        transmute(
                                Date = as.Date(Date),
                                Year  = year(Date),
                                Month = month(Date, label = TRUE),
                                Forecast = fmt_money(.mean),
                                `80%` = as.character(`80%`),
                                `95%` = as.character(`95%`)
                        ) |>
                        select(Year, Month, Forecast, `80%`, `95%`)
        })
        
        output$ministry_funds <- renderTable({
                mf <- Ministry_funds
                num_cols <- names(mf)[sapply(mf, is.numeric)]
                mf[num_cols] <- lapply(mf[num_cols], fmt_money)
                mf
        }, rownames = FALSE)
}
