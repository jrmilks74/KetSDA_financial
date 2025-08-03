#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load the packages
library(shiny)
library(googlesheets4)
library(tidyverse)
library(plotly)
library(scales)
library(fpp3)
library(urca)
library(kableExtra)
library(rvest)

# Set options and authenticate access
options(scipen = 999)

options(
        gargle_oauth_email = TRUE,
        gargle_oauth_cache = "auth/.secrets/"
)
gs4_auth(email = "office@ketsda.org")

#Load the data
Finances <- read_sheet("https://docs.google.com/spreadsheets/d/1DPq4lM36_CcjEGN2v8UE1w1pr-t0C8lenYx56jniNRo/edit#gid=0",
                       col_types = "Dnnn") %>%
        mutate(Deficit = Income - Expenses)

Ministry_funds <- read_sheet("https://docs.google.com/spreadsheets/d/1d2g7NRipbarvq78LUxuEObWsTru4t4w_TY9uQgQOvZQ/edit#gid=0")
Ministry_funds[-1] <- lapply(Ministry_funds[-1], dollar)

URL <- read_html("https://fred.stlouisfed.org/data/CUUR0200SA0.txt")
table <- html_nodes(URL, "table")
table_content <- html_table(table)[[2]]

MidwestCPI <- table_content %>%
        mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>%
        mutate_if(is.character, as.numeric) %>%
        filter(DATE >= "2000-01-01") %>%
        rename(Date = DATE, CPI = VALUE)

# Wrangle data
Finances <- inner_join(Finances, MidwestCPI, by = "Date") %>%
        mutate(Year = year(Date),
               Month = month(Date),
               Inflation = CPI / first(CPI),
               AdjTithe = Tithe / Inflation,
               AdjIncome = Income / Inflation,
               AdjExpenses = Expenses / Inflation,
               AdjDeficit = Deficit / Inflation) %>%
        select(Date,
               Year,
               Month,
               Tithe, 
               AdjTithe, 
               Income, 
               AdjIncome, 
               Expenses, 
               AdjExpenses,
               Deficit,
               AdjDeficit) %>%
        pivot_longer(cols = -c("Date",
                                 "Year", 
                                 "Month"), 
                     names_to = "Category", 
                     values_to = "Amount")

Yearly_finances <- Finances %>%
        select(Year, Category, Amount) %>%
        group_by(Year, Category) %>%
        summarise(Total = sum(Amount, na.rm = TRUE)) %>%
        pivot_wider(id_cols = Year, 
                    names_from = Category, 
                    values_from = Total)

Finances.tsibble <- Finances %>%
        select(!Date) %>%
        pivot_wider(id_cols = c(Year, Month), 
                    names_from = Category, 
                    values_from = Amount) %>%
        mutate(Date = paste(Year, Month, sep = " ")) %>%
        mutate(Date = yearmonth(Date)) %>%
        as_tsibble(index = Date)

Finances <- Finances %>%
        pivot_wider(id_cols = c(Date, Year, Month), 
                    names_from = Category, 
                    values_from = Amount)

end <- tail(Yearly_finances$Year)[6]

# Overview Table
current_year <- tail(Finances)[6,] %>%
        select(Tithe, Income, Expenses, Deficit)
last_month <- tail(Finances)[6,] %>%
        select(Tithe, Income, Expenses, Deficit)
last_month_name <- month.name[as.numeric(format(as.Date(tail(Finances$Date, 13)[13]), "%m"))]
year_over_year_month <- tail(Finances, 13)[1,] %>%
        select(Tithe, Income, Expenses, Deficit)
yoy_change <- data.frame(tithe = current_year$Tithe - year_over_year_month$Tithe,
                         income = current_year$Income - year_over_year_month$Income,
                         expenses = current_year$Expenses - year_over_year_month$Expenses,
                         deficit = current_year$Deficit - year_over_year_month$Deficit)
yoy_percent_change <- data.frame(tithe = (yoy_change$tithe / year_over_year_month$Tithe),
                                 income = (yoy_change$income / year_over_year_month$Income),
                                 expenses = (yoy_change$expenses / year_over_year_month$Expenses),
                                 deficit = (yoy_change$deficit / year_over_year_month$Deficit))

mtd <- matrix(c(dollar(last_month$Tithe),
                " ",
                dollar(last_month$Income),
                dollar(last_month$Expenses),
                dollar(last_month$Deficit),
                dollar(year_over_year_month$Tithe),
                " ",
                dollar(year_over_year_month$Income),
                dollar(year_over_year_month$Expenses),
                dollar(year_over_year_month$Deficit),
                dollar(yoy_change$tithe),
                " ",
                dollar(yoy_change$income),
                dollar(yoy_change$expenses),
                dollar(yoy_change$deficit),
                percent(yoy_percent_change$tithe),
                " ",
                percent(yoy_percent_change$income),
                percent(yoy_percent_change$expenses),
                percent(yoy_percent_change$deficit)
),
ncol = 4,
byrow = FALSE
)

colnames(mtd) <- c(paste(last_month_name, end),
                   paste(last_month_name, (end - 1)),
                   "Change",
                   "Percent change")

rownames(mtd) <- c("Tithe",
                   "Church budget",
                   "Income",
                   "Expenses",
                   "Surplus/Deficit")

month_end <- tail(Finances$Month)[6]
year_to_date <- Finances %>%
        filter(Year >= (end - 1),
               Month <= month_end) %>%
        select(Year, Tithe, Income, Expenses, Deficit) %>%
        group_by(Year) %>%
        summarize(across(c(Tithe, Income, Expenses, Deficit), ~sum(.x, na.rm = TRUE)))

ytd_yoy_change <- data.frame(tithe = (year_to_date$Tithe[2]) - (year_to_date$Tithe[1]),
                             income = (year_to_date$Income[2]) - (year_to_date$Income[1]),
                             expenses = (year_to_date$Expenses[2]) - (year_to_date$Expenses[1]),
                             deficit = (year_to_date$Deficit[2]) - (year_to_date$Deficit[1]))

ytd_yoy_percent_change <- data.frame(tithe = ytd_yoy_change$tithe / year_to_date$Tithe[1],
                                     income = ytd_yoy_change$income / year_to_date$Income[1],
                                     expenses = ytd_yoy_change$expenses / year_to_date$Expenses[1],
                                     deficit = ytd_yoy_change$deficit / year_to_date$Deficit[1])

ytd <- matrix(c(dollar(year_to_date$Tithe[2]),
                " ",
                dollar(year_to_date$Income[2]),
                dollar(year_to_date$Expenses[2]),
                dollar(year_to_date$Deficit[2]),
                dollar(year_to_date$Tithe[1]),
                " ",
                dollar(year_to_date$Income[1]),
                dollar(year_to_date$Expenses[1]),
                dollar(year_to_date$Deficit[1]),
                dollar(ytd_yoy_change$tithe),
                " ",
                dollar(ytd_yoy_change$income),
                dollar(ytd_yoy_change$expenses),
                dollar(ytd_yoy_change$deficit),
                percent(ytd_yoy_percent_change$tithe),
                " ",
                percent(ytd_yoy_percent_change$income),
                percent(ytd_yoy_percent_change$expenses),
                percent(ytd_yoy_percent_change$deficit)
),
ncol = 4,
byrow = FALSE
)

colnames(ytd) <- c(paste(end, "to date"),
                   paste((end - 1), "to date"),
                   "Change",
                   "Percent change")

rownames(ytd) <- c("Tithe",
                   "Church budget",
                   "Income",
                   "Expenses",
                   "Surplus/Deficit")

end2 <- paste(end, "-01-01", sep = "")
Finances.tsibble2 <- Finances %>%
        filter(Date < end2) %>%
        select(!Date) %>%
        mutate(Date = paste(Year, Month, sep = " ")) %>%
        mutate(Date = yearmonth(Date)) %>%
        as_tsibble(index = Date)

# Define server logic required each tab
function(input, output, session) {
        
        # Main tab 
        ## Summary table using kableExtra to specify background, indentation, and formating
        output$mtdFinances <- function() ({
                mtd <- kbl(mtd) |>
                        add_header_above(c(" ", " ", " ", "Year-over-year" = 2)) |>
                        kable_styling(bootstrap_options = c("striped"), full_width = F) |>
                        add_indent(c(3,4,5)) |>
                        column_spec(1, bold = T)
                mtd
        })
        
        output$ytdFinances <- function() ({
                ytd <- kbl(ytd) |>
                        add_header_above(c(" ", " ", " ", "Year-over-year" = 2)) |>
                        kable_styling(bootstrap_options = c("striped"), full_width = F) |>
                        add_indent(c(3,4,5)) |>
                        column_spec(1, bold = T)
                ytd
        })
        
        # In Depth tab
        
        ## Data sets
        sub_end <- tail(Finances$Date)[6] %m+% years(1) %>%
                year() %>%
                paste("-01-01", sep = "") %>%
                as.Date()
        
        Tithe_month <- Finances %>%
                select(Date, Tithe) %>%
                rename(Current = Tithe)
        
        Income_month <- Finances %>%
                select(Date, Income) %>%
                rename(Current = Income)
        
        Expenses_month <- Finances %>%
                select(Date, Expenses) %>%
                rename(Current = Expenses)
        
        Tithe_year <- Yearly_finances %>%
                select(Year, Tithe, AdjTithe) %>%
                rename(Current = Tithe, 
                       Adjusted = AdjTithe) %>%
                subset(Year < end)
        
        Income_year <- Yearly_finances %>%
                select(Year, Income, AdjIncome) %>%
                rename(Current = Income, 
                       Adjusted = AdjIncome) %>%
                subset(Year < end)
        
        Expenses_year <- Yearly_finances %>%
                select(Year, Expenses, AdjExpenses) %>%
                rename(Current = Expenses, 
                       Adjusted = AdjExpenses) %>%
                subset(Year < end)
        
        Tithe.tsibble <- Finances.tsibble %>%
                select(Date, Tithe) %>%
                rename(Current = Tithe)
        
        Income.tsibble <- Finances.tsibble %>%
                select(Date, Income) %>%
                rename(Current = Income)
        
        Expenses.tsibble <- Finances.tsibble %>%
                select(Date, Expenses) %>%
                rename(Current = Expenses)
        
        Tithe.tsibble2 <- Finances.tsibble2 %>%
                select(Date, Tithe) %>%
                rename(Current = Tithe)
        
        Income.tsibble2 <- Finances.tsibble2 %>%
                select(Date, Income) %>%
                rename(Current = Income)
        
        Expenses.tsibble2 <- Finances.tsibble2 %>%
                select(Date, Expenses) %>%
                rename(Current = Expenses)
        
        ## Subset end point
        ## Reactive elements
        Finances_monthly <- reactive({
                if (input$Category == "Tithe")
                        Tithe_month
                else if (input$Category == "Income")
                        Income_month
                else
                        Expenses_month
        })
        
        Finances_yearly <- reactive({
                if (input$Category == "Tithe")
                        Tithe_year
                else if (input$Category == "Income")
                        Income_year
                else
                        Expenses_year
        })
        
        Seasonal_dependent <- reactive({
                if (input$Category == "Tithe")
                        Finances %>%
                        select(Date, Tithe) %>%
                        rename(Amount = Tithe)
                else if (input$Category == "Income")
                        Finances %>%
                        select(Date, Income) %>%
                        rename(Amount = Income)
                else
                        Finances %>%
                        select(Date, Expenses) %>%
                        rename(Amount = Expenses)
        })
        
        Seasonal_inflation_dependent <- reactive({
                if (input$Category == "Tithe")
                        Finances %>%
                        select(Date, AdjTithe) %>%
                        rename(Amount = AdjTithe)
                else if (input$Category == "Income")
                        Finances %>%
                        select(Date, AdjIncome) %>%
                        rename(Amount = AdjIncome)
                else
                        Finances %>%
                        select(Date, AdjExpenses) %>%
                        rename(Amount = AdjExpenses)
        })
        
        selected_tsibble <- reactive({
                if (input$Category == "Tithe")
                   Tithe.tsibble
                else if (input$Category == "Income")
                        Income.tsibble
                else
                        Expenses.tsibble
        })
        
        selected_tsibble2 <- reactive({
                if (input$Category == "Tithe")
                        Tithe.tsibble2
                else if (input$Category == "Income")
                        Income.tsibble2
                else
                        Expenses.tsibble2
        })
        
        current_year_cat <- reactive({
                if (input$Category == "Tithe")
                        year_to_date$Tithe[2]
                else if (input$Category == "Income")
                        year_to_date$Income[2]
                else
                        year_to_date$Expenses[2]
        })
        
        last_month_cat <- reactive({
                if (input$Category == "Tithe")
                        last_month$Tithe
                else if (input$Category == "Income")
                        last_month$Income
                else
                        last_month$Expenses
        })
        
        ## Monthly time series graph
        output$monthly_time_graph <- renderPlotly({
                
                p2 <- ggplot(data = Finances_monthly(), aes(x = Date)) +
                        theme_classic() +
                        geom_line(aes(y = Current)) +
                        geom_smooth(aes(y = Current), method = "loess", formula = y ~ x) +
                        labs(y = "Dollars",
                             x = "Year")
                ggplotly(p2)
        })
        
        ## Annual time series graph
        output$annual_time_graph <- renderPlotly({
                
                p3 <- ggplot(data = Finances_yearly(), aes(x = Year)) +
                        theme_classic() +
                        geom_line(aes(y = Current, colour = "Current dollars")) +
                        geom_smooth(aes(y = Current, colour = "Current dollars"), 
                                    method = "loess", formula = y ~ x) +
                        geom_line(aes(y = Adjusted, colour = "Inflation-adjusted")) +
                        geom_smooth(aes(y = Adjusted, colour = "Inflation-adjusted"), 
                                    method = "loess", formula = y ~ x) +
                        labs(y = "Dollars",
                             x = "Year")
                ggplotly(p3)
        })
        
        ## Seasonal graph current dollars
        output$seasonal_graph <- renderPlotly({
                this_year <- tail(Yearly_finances$Year)[6]
                previous_year <- tail(Yearly_finances$Year)[5]
                s1 <- Seasonal_dependent() %>%
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) %>%
                        ggplot(aes(x = Date, y = Amount, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = function(x) filter(x, Year == previous_year), lwd = 0.5) +
                        geom_line(data = function(x) filter(x, Year == this_year), lwd = 0.75) +
                        theme_bw() +
                        labs(y = "Current Dollars",
                             x = "Month")
                ggplotly(s1)
        })
        
        ## Seasonal graph inflation-adjusted dollars
        output$seasonal_inflation_graph <- renderPlotly({
                this_year <- tail(Yearly_finances$Year)[6]
                previous_year <- tail(Yearly_finances$Year)[5]
                s2 <- Seasonal_inflation_dependent() %>%
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) %>%
                        ggplot(aes(x = Date, y = Amount, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = function(x) filter(x, Year == previous_year), lwd = 0.5) +
                        geom_line(data = function(x) filter(x, Year == this_year), lwd = 0.75) +
                        theme_bw() +
                        labs(y = "Real Dollars indexed to Jan 2000",
                             x = "Month")
                ggplotly(s2)
        })
        
        ## Forecast
        output$forecast_graph <- renderPlot({
                fit <- selected_tsibble() %>%
                        model(ARIMA(Current))
                fc <- forecast(fit, h = 12)
                
                fc %>%
                        autoplot(selected_tsibble()) +
                        theme_bw() +
                        labs(y = "Dollars",
                             title = "Forecast")
        })
        
        output$forcast_vs_actual <- renderPlotly({
                fit2 <- selected_tsibble2() %>%
                        model(ARIMA(Current))
                fc2 <- forecast(fit2, h = 12)
                
                annual_forecast <- fc2 %>%
                        as_tibble() %>%
                        transmute(Date = format(Date),
                                  forecast = .mean) %>%
                        mutate(Date = as.Date(paste0(Date, '-01'), '%Y %b-%d'))
                
                actual <- Finances_monthly() %>%
                        filter(Date >= end2)
                
                avf <- ggplot() +
                        theme_bw() +
                        geom_line(data = annual_forecast, aes(x = Date, y = forecast, colour = "forecast")) +
                        geom_point(data = annual_forecast, aes(x = Date, y = forecast, colour = "forecast")) +
                        geom_line(data = actual, aes(x = Date, y = Current, colour = "actual")) +
                        geom_point(data = actual, aes(x = Date, y = Current, colour = "actual")) +
                        labs(title = "January Forecast vs Actual",
                             x = "Date",
                             y = "Amount in dollars")
                ggplotly(avf)
        })
        
        ## Total annual prediction table
        output$total_amount_table <- renderTable({
                ### Model and forecast
                fit <- selected_tsibble() %>%
                        model(ARIMA(Current))
                fc <- forecast(fit, h = 12)
                
                ### Add up prediction for rest of calendar year
                forecasted_amount <- fc %>%
                        as_tibble() %>%
                        transmute(Date = format(Date),
                                  forecast = .mean) %>%
                        mutate(Date = as.Date(paste0(Date, '-01'), '%Y %b-%d')) %>%
                        subset(Date < sub_end) %>%
                        summarize(total = sum(forecast))
                
                ### Add predicted amount with year-to-date amount
                predicted_total <- current_year_cat() + forecasted_amount$total
                
                ### Create table for display
                total_table <- matrix(c(dollar(last_month_cat()),
                                           dollar(current_year_cat()),
                                           dollar(forecasted_amount$total),
                                           dollar(predicted_total)),
                                         ncol = 4,
                                         byrow = FALSE
                )
                
                colnames(total_table) <- c(last_month_name,
                                              "Year-to-date",
                                              "Predicted rest of year",
                                              "Predicted total for current year")
                
                ### Final product
                total_table
        }, rownames = TRUE)
        
        ## Forecast table
        output$forecast_table <- renderTable({
                ### Model and forecast
                fit <- selected_tsibble() %>%
                        model(ARIMA(Current))
                fc <- forecast(fit, h = 12)
                
                ### Table
                forecast_table <- hilo(fc) %>%
                        as_tibble() %>%
                        transmute(Date = format(Date),
                                  Forecast = dollar(.mean),
                                  "80%" = format(`80%`),
                                  "95%" = format(`95%`)) %>%
                        mutate(Date = as.Date(paste0(Date, '-01'), '%Y %b-%d')) %>%
                        mutate(Year = year(Date),
                               Month = month(Date, label = TRUE)) %>%
                        select(Year, Month, Forecast, "80%", "95%")
        })
        
        # Ministry current funds
        output$ministry_funds <- renderTable({
                Ministry_funds
        }, rownames = TRUE)

}
