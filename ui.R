#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This app developed for:
# Kettering Adventist Church
# Kettering, Ohio, USA
#
# Author: James Ryan Milks, Dayton, Ohio, USA
# Date: 2023 June 6
# Update: 2024 Feb 25

library(shiny)
library(mailtoR)
library(plotly)
library(kableExtra)

# Define user interface
ui <- fluidPage(
        # Title
        titlePanel("Kettering Adventist Church Financial Overview"),
        
        # Set tabs
        tabsetPanel(
                # Overview page
                tabPanel(
                        "Financial Overview",
                        sidebarLayout(
                                sidebarPanel(width = 0),
                                mainPanel(
                                        h3("Month-to-date"),
                                        tableOutput("mtdFinances"),
                                        h3("Year-to-date"),
                                        tableOutput("ytdFinances")
                                        )
                                )
                        ),
                
                # In-depth Focus
                tabPanel(
                        "In-Depth",
                        sidebarLayout(
                                sidebarPanel(
                                        selectInput(inputId = "Category",
                                                    label = "Select a category",
                                                    choices = list("Tithe" = "Tithe",
                                                                "Church Budget Income" = "Income",
                                                                "Church Budget Expenses" = "Expenses"),
                                                    selected = "Tithe")
                                ),
                                mainPanel(
                                        h3("Current and predicted dollar amounts for the year"),
                                        tableOutput("total_amount_table"),
                                        h3("Monthly change over time"),
                                        plotlyOutput("monthly_time_graph"),
                                        h3("Seasonal graph"),
                                        plotlyOutput("seasonal_graph"),
                                        h3("Seasonal graph adjusted for inflation"),
                                        plotlyOutput("seasonal_inflation_graph"),
                                        h3("Annual change over time"),
                                        plotlyOutput("annual_time_graph"),
                                        h3("Forecast"),
                                        plotlyOutput("forcast_vs_actual"),
                                        plotOutput("forecast_graph"),
                                        tableOutput("forecast_table")
                                )
                        )
                ),
                
                # Pastoral budgets
                tabPanel(
                        "Ministry Current Funds",
                        sidebarLayout(
                                sidebarPanel(width = 0),
                                mainPanel(
                                        h3("Current funds"),
                                        tableOutput("ministry_funds"),
                                        width = 12
                                )
                        )
                )
        ),
        hr(),
        h5("Created by: Jim Milks"),
        br(),
        "Initialized on 16 June 2023",
        br(),
        "Data updated on the 15th of each month",
        br(),
        a(actionButton(inputId = "email1", label = "Contact Admin", 
                       icon = icon("envelope", lib = "font-awesome")),
          href = "mailto: jrmilks@gmail.com")
)