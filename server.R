#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googlesheets4)
library(tidyverse)
library(blscrapeR)
library(scales)
library(fpp3)
library(urca)

options(scipen = 999)

options(
        gargle_oauth_email = TRUE,
        gargle_oauth_cache = "auth/.secrets/"
)
gs4_auth(email = "office@ketsda.org")

Finances <- read_sheet("https://docs.google.com/spreadsheets/d/1DPq4lM36_CcjEGN2v8UE1w1pr-t0C8lenYx56jniNRo/edit#gid=0",
                       col_types = "Dnnn")

Ministry_funds <- read_sheet("https://docs.google.com/spreadsheets/d/1d2g7NRipbarvq78LUxuEObWsTru4t4w_TY9uQgQOvZQ/edit#gid=0")
Ministry_funds[-1] <- lapply(Ministry_funds[-1], dollar)

cpi_2020 <- bls_api("CUUR0200SA0", startyear = 2020)
cpi_2010 <- bls_api("CUUR0200SA0", startyear = 2010, endyear = 2019)
cpi_2000 <- bls_api("CUUR0200SA0", startyear = 2000, endyear = 2009)

cpi <- bind_rows(cpi_2020, cpi_2010, cpi_2000) %>%
        mutate(periodName = match(periodName, month.name))
cpi$Date <- as.Date(with(cpi, paste(year, periodName, "01", sep = "-")), "%Y-%m-%d")
cpi <- cpi %>%
        arrange(Date) %>%
        select(Date, value) %>%
        rename(CPI = value)

Finances <- inner_join(Finances, cpi, by = "Date")

Finances <- Finances %>%
        mutate(Adj_tithe = Tithe * (first(CPI) / CPI),
               Adj_income = Income * (first(CPI) / CPI),
               Adj_expenses = Expenses * (first(CPI) / CPI),
               Deficit = Income - Expenses,
               Adj_deficit = Adj_income - Adj_expenses)

Yearly_finances <- Finances %>%
        group_by(Year = floor_date(Date, "year")) %>%
        summarize(Tithe = sum(Tithe),
                  Income = sum(Income),
                  Expenses = sum(Expenses),
                  Adj_tithe = sum(Adj_tithe),
                  Adj_income = sum(Adj_income),
                  Adj_expenses = sum(Adj_expenses),
                  Deficit = sum(Deficit),
                  Adj_deficit = sum(Adj_deficit))

Finances.tsibble <- Finances %>%
        mutate(Year = year(Date),
               Month = month(Date)) %>%
        select(Year, 
               Month, 
               Tithe, 
               Income, 
               Expenses, 
               Adj_tithe, 
               Adj_income, 
               Adj_expenses,
               Deficit,
               Adj_deficit) %>%
        mutate(Date = paste(Year, Month, sep = " ")) %>%
        mutate(Date = yearmonth(Date)) %>%
        select(Date,
               Tithe,
               Income,
               Expenses,
               Adj_tithe,
               Adj_income,
               Adj_expenses,
               Deficit,
               Adj_deficit) %>%
        as_tsibble(index = Date)

end <- tail(Yearly_finances$Year)[6]

# Overview Table
current_year <- tail(Yearly_finances)[6,] %>%
        select(Tithe, Income, Expenses, Deficit)
last_month <- tail(Finances)[6,] %>%
        select(Tithe, Income, Expenses, Deficit)
last_month_name <- month.name[as.numeric(format(as.Date(tail(Finances$Date, 13)[13]), "%m"))]

ytd <- matrix(c(dollar(last_month$Tithe),
                " ",
                dollar(last_month$Income),
                dollar(last_month$Expenses),
                dollar(last_month$Deficit),
                dollar(current_year$Tithe),
                " ",
                dollar(current_year$Income),
                dollar(current_year$Expenses),
                dollar(current_year$Deficit)
),
ncol = 2,
byrow = FALSE
)

colnames(ytd) <- c(last_month_name,
                   "Year-to-date")

rownames(ytd) <- c("Tithe",
                   "Church budget",
                   "     Income",
                   "     Expenses",
                   "     Surplus/Deficit")


# Define server logic required each tab
function(input, output, session) {
        
        # Main tab 
        ## Summary table
        output$ytdFinances <- renderTable({
                ytd
        }, rownames = TRUE)
        
        ## Tithe overview
        output$seasonal_tithe <- renderPlotly({
                this_year <- year(tail(Yearly_finances$Year)[6])
                previous_year <- year(tail(Yearly_finances$Year)[5])
                t1 <- Finances %>%
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) %>%
                        ggplot(aes(x = Date, y = Tithe, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = function(x) filter(x, Year == previous_year), lwd = 0.5) +
                        geom_line(data = function(x) filter(x, Year == this_year), lwd = 1) +
                        theme_bw() +
                        labs(y = "Dollars",
                             x = "Month",
                             title = "Tithe")
                ggplotly(t1)
        })
        
        ## Income overview
        output$seasonal_income <- renderPlotly({
                this_year <- year(tail(Yearly_finances$Year)[6])
                previous_year <- year(tail(Yearly_finances$Year)[5])
                i1 <- Finances %>%
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) %>%
                        ggplot(aes(x = Date, y = Income, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = function(x) filter(x, Year == previous_year), lwd = 0.5) +
                        geom_line(data = function(x) filter(x, Year == this_year), lwd = 1) +
                        theme_bw() +
                        labs(y = "Dollars",
                             x = "Month",
                             title = "Operating income")
                ggplotly(i1)
        })
        
        ## Expenses overview
        output$seasonal_expenses <- renderPlotly({
                this_year <- year(tail(Yearly_finances$Year)[6])
                previous_year <- year(tail(Yearly_finances$Year)[5])
                e1 <- Finances %>%
                        mutate(Year = factor(year(Date)),
                               Date = update(Date, year = 1)) %>%
                        ggplot(aes(x = Date, y = Expenses, colour = Year)) +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        geom_line(aes(group = Year), colour = "black", alpha = 0.1) +
                        geom_line(data = function(x) filter(x, Year == previous_year), lwd = 0.5) +
                        geom_line(data = function(x) filter(x, Year == this_year), lwd = 1) +
                        theme_bw() +
                        labs(y = "Dollars",
                             x = "Month",
                             title = "Operating expenses")
                ggplotly(e1)
        })
        
        # Categories tab
        
        ## Data sets
        sub_end <- end %m+% years(1)
        
        Tithe <- Yearly_finances %>%
                select(Year, Tithe, Adj_tithe) %>%
                rename(Nominal = Tithe,
                       Real = Adj_tithe) %>%
                subset(Year < end)
        
        Income <- Yearly_finances %>%
                select(Year, Income, Adj_income) %>%
                rename(Nominal = Income,
                       Real = Adj_income) %>%
                subset(Year < end)
        
        Expenses <- Finances %>%
                select(Date, Expenses, Adj_expenses) %>%
                rename(Year = Date,
                       Nominal = Expenses,
                       Real = Adj_expenses)
        
        Tithe.tsibble <- Finances.tsibble %>%
                select(Date, Tithe) %>%
                rename(Nominal = Tithe)
        
        Income.tsibble <- Finances.tsibble %>%
                select(Date, Income) %>%
                rename(Nominal = Income)
        
        Expenses.tsibble <- Finances.tsibble %>%
                select(Date, Expenses) %>%
                rename(Nominal = Expenses)
        
        ## Subset end point
        ## Reactive elements
        selected_data <- reactive({
                if (input$Category == "Tithe")
                        Tithe
                else if (input$Category == "Income")
                        Income
                else
                        Expenses
        })
        
        selected_tsibble <- reactive({
                if (input$Category == "Tithe")
                   Tithe.tsibble
                else if (input$Category == "Income")
                        Income.tsibble
                else
                        Expenses.tsibble
        })
        
        current_year_cat <- reactive({
                if (input$Category == "Tithe")
                        current_year$Tithe
                else if (input$Category == "Income")
                        current_year$Income
                else
                        current_year$Expenses
        })
        
        last_month_cat <- reactive({
                if (input$Category == "Tithe")
                        last_month$Tithe
                else if (input$Category == "Income")
                        last_month$Income
                else
                        last_month$Expenses
        })
        
        ## Time series graph
        output$time_graph <- renderPlotly({
                
                p2 <- ggplot(data = selected_data(), aes(x = Year)) +
                        theme_classic() +
                        geom_line(aes(y = Nominal, colour = "Nominal")) +
                        geom_smooth(aes(y = Nominal), method = "loess", formula = y ~ x, colour = "pink") +
                        geom_line(aes(y = Real, colour = "Real")) +
                        geom_smooth(aes(y = Real), method = "loess", formula = y ~ x, colour = "blue") +
                        labs(y = "Dollars",
                             x = "Year",
                             title = "Change over time")
                ggplotly(p2)
        })
        
        ## Forecast
        output$forecast_graph <- renderPlot({
                fit <- selected_tsibble() %>%
                        model(ARIMA(Nominal))
                fc <- forecast(fit, h = 12)
                
                fc %>%
                        autoplot(selected_tsibble()) +
                        theme_bw() +
                        labs(y = "Dollars",
                             title = "Forecast")
        })
        
        ## Total annual prediction table
        output$total_amount_table <- renderTable({
                ### Model and forecast
                fit <- selected_tsibble() %>%
                        model(ARIMA(Nominal))
                fc <- forecast(fit, h = 12)
                
                ### Add up prediction for rest of calendar year
                forecasted_amount <- fc %>%
                        as_tibble() %>%
                        transmute(Date = format(Date),
                                  forecast = .mean) %>%
                        mutate(Date = as.Date(paste0(Date, '-01'), '%Y %b-%d')) %>%
                        subset(Date < as.Date(sub_end, "%Y-%b-%d")) %>%
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
                        model(ARIMA(Nominal))
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
