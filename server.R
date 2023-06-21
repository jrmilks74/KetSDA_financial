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
                       col_types = "Dnnn") %>%
        mutate(Deficit = Income - Expenses)

Ministry_funds <- read_sheet("https://docs.google.com/spreadsheets/d/1d2g7NRipbarvq78LUxuEObWsTru4t4w_TY9uQgQOvZQ/edit#gid=0")
Ministry_funds[-1] <- lapply(Ministry_funds[-1], dollar)

Yearly_finances <- Finances %>%
        group_by(Year = floor_date(Date, "year")) %>%
        summarize(Tithe = sum(Tithe),
                  Income = sum(Income),
                  Expenses = sum(Expenses),
                  Deficit = sum(Deficit))

Finances.tsibble <- Finances %>%
        mutate(Year = year(Date),
               Month = month(Date)) %>%
        select(Year, 
               Month, 
               Tithe, 
               Income, 
               Expenses,
               Deficit) %>%
        mutate(Date = paste(Year, Month, sep = " ")) %>%
        mutate(Date = yearmonth(Date)) %>%
        select(Date,
               Tithe,
               Income,
               Expenses,
               Deficit) %>%
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
        
        Tithe_month <- Finances %>%
                select(Date, Tithe) %>%
                rename(Nominal = Tithe)
        
        Income_month <- Finances %>%
                select(Date, Income) %>%
                rename(Nominal = Income)
        
        Expenses_month <- Finances %>%
                select(Date, Expenses) %>%
                rename(Nominal = Expenses)
        
        Tithe_year <- Yearly_finances %>%
                select(Year, Tithe) %>%
                rename(Nominal = Tithe) %>%
                subset(Year < end)
        
        Income_year <- Yearly_finances %>%
                select(Year, Income) %>%
                rename(Nominal = Income) %>%
                subset(Year < end)
        
        Expenses_year <- Yearly_finances %>%
                select(Year, Expenses) %>%
                rename(Nominal = Expenses) %>%
                subset(Year < end)
        
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
        
        ## Monthly time series graph
        output$monthly_time_graph <- renderPlotly({
                
                p2 <- ggplot(data = Finances_monthly(), aes(x = Date)) +
                        theme_classic() +
                        geom_line(aes(y = Nominal)) +
                        geom_smooth(aes(y = Nominal), method = "loess", formula = y ~ x) +
                        labs(y = "Dollars",
                             x = "Year",
                             title = "Change over time")
                ggplotly(p2)
        })
        
        ## Annual time series graph
        output$annual_time_graph <- renderPlotly({
                
                p3 <- ggplot(data = Finances_yearly(), aes(x = Year)) +
                        theme_classic() +
                        geom_line(aes(y = Nominal)) +
                        geom_smooth(aes(y = Nominal), method = "loess", formula = y ~ x) +
                        labs(y = "Dollars",
                             x = "Year",
                             title = "Change over time")
                ggplotly(p3)
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
