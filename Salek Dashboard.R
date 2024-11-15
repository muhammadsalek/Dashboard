# Load Required Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(forecast)
library(DT)
library(dplyr)
library(ggplot2)
library(hms)

# Load Dataset
coffee_data <- read.csv("C:/Users/Acer/Downloads/Coffee_Shop_Sales.xlsx - Transactions.csv")

# Data Preparation
coffee_data <- coffee_data %>%
  mutate(
    transaction_date = as.Date(transaction_date, format = "%m/%d/%Y"),
    transaction_time = hms::as_hms(transaction_time),
    day_of_week = weekdays(transaction_date),
    month = lubridate::month(transaction_date, label = TRUE)
  )

# UI for Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Coffee Shop Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Sales Trends", tabName = "sales_trends", icon = icon("chart-line")),
      menuItem("Predictive Model", tabName = "predictive_model", icon = icon("chart-bar")),
      menuItem("Store Analysis", tabName = "store_analysis", icon = icon("store"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f9f9f9 !important; }
      .box { border-radius: 5px; }
      .main-header { font-weight: bold; font-size: 18px; color: #333; }
    "))),
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Total Sales by Day of the Week", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("total_sales")),
                box(title = "Average Sales by Day of the Week", width = 6, solidHeader = TRUE, status = "info",
                    plotlyOutput("daily_avg_sales"))
              ),
              fluidRow(
                box(title = "Data Summary", width = 12, status = "primary", solidHeader = TRUE,
                    dataTableOutput("data_summary"))
              )
      ),
      
      # Sales Trends Tab
      tabItem(tabName = "sales_trends",
              fluidRow(
                box(title = "Sales Over Time", width = 12, solidHeader = TRUE, status = "warning",
                    dateRangeInput("dateRange", "Select Date Range:", start = min(coffee_data$transaction_date),
                                   end = max(coffee_data$transaction_date), format = "yyyy-mm-dd"),
                    plotlyOutput("sales_over_time"))
              )
      ),
      
      # Predictive Model Tab
      tabItem(tabName = "predictive_model",
              fluidRow(
                box(title = "Sales Forecast (Next 6 Months)", width = 12, solidHeader = TRUE, status = "success",
                    plotlyOutput("sales_forecast")),
                box(title = "Model Summary", width = 12, solidHeader = TRUE, status = "info",
                    verbatimTextOutput("model_summary"))
              )
      ),
      
      # Store Analysis Tab
      tabItem(tabName = "store_analysis",
              fluidRow(
                box(title = "Store Location Analysis", width = 12, solidHeader = TRUE, status = "primary",
                    selectInput("store_select", "Select Store Location:", choices = unique(coffee_data$store_location)),
                    plotlyOutput("store_sales"))
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Data Summary Table
  output$data_summary <- renderDataTable({
    coffee_data %>%
      group_by(product_category) %>%
      summarise(
        Total_Sales = sum(transaction_qty * unit_price),
        Avg_Price = mean(unit_price),
        Total_Transactions = n()
      ) %>%
      datatable(options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Total Sales Plot
  output$total_sales <- renderPlotly({
    total_sales <- coffee_data %>%
      group_by(day_of_week) %>%
      summarise(total_sales = sum(transaction_qty * unit_price))
    
    plot_ly(total_sales, x = ~day_of_week, y = ~total_sales, type = 'bar', name = 'Total Sales', marker = list(color = 'rgb(58, 71, 80)')) %>%
      layout(title = 'Total Sales by Day of Week', xaxis = list(title = 'Day of Week'), yaxis = list(title = 'Total Sales'))
  })
  
  # Daily Average Sales Plot
  output$daily_avg_sales <- renderPlotly({
    avg_sales <- coffee_data %>%
      group_by(day_of_week) %>%
      summarise(avg_sales = mean(transaction_qty * unit_price))
    
    plot_ly(avg_sales, x = ~day_of_week, y = ~avg_sales, type = 'bar', name = 'Average Sales', marker = list(color = 'rgb(135, 206, 235)')) %>%
      layout(title = 'Average Sales by Day of Week', xaxis = list(title = 'Day of Week'), yaxis = list(title = 'Average Sales'))
  })
  
  # Sales Over Time Plot
  output$sales_over_time <- renderPlotly({
    sales_over_time <- coffee_data %>%
      filter(transaction_date >= input$dateRange[1] & transaction_date <= input$dateRange[2]) %>%
      group_by(transaction_date) %>%
      summarise(total_sales = sum(transaction_qty * unit_price))
    
    plot_ly(sales_over_time, x = ~transaction_date, y = ~total_sales, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = 'Sales Over Time', xaxis = list(title = 'Date'), yaxis = list(title = 'Total Sales'))
  })
  
  # Store Sales Analysis
  output$store_sales <- renderPlotly({
    store_sales <- coffee_data %>%
      filter(store_location == input$store_select) %>%
      group_by(transaction_date) %>%
      summarise(total_sales = sum(transaction_qty * unit_price))
    
    plot_ly(store_sales, x = ~transaction_date, y = ~total_sales, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = paste("Sales at", input$store_select), xaxis = list(title = 'Date'), yaxis = list(title = 'Total Sales'))
  })
  
  # Forecast Plot
  output$sales_forecast <- renderPlotly({
    monthly_sales <- coffee_data %>%
      mutate(month = floor_date(transaction_date, "month")) %>%
      group_by(month) %>%
      summarise(total_sales = sum(transaction_qty * unit_price)) %>%
      arrange(month)
    
    ts_sales <- ts(monthly_sales$total_sales, frequency = 12)
    
    model <- forecast::auto.arima(ts_sales)
    forecast_sales <- forecast::forecast(model, h = 6)
    
    forecast_data <- data.frame(
      month = seq(max(monthly_sales$month) + months(1), by = "month", length.out = 6),
      forecast = as.numeric(forecast_sales$mean)
    )
    
    plot_ly() %>%
      add_trace(x = monthly_sales$month, y = monthly_sales$total_sales, type = 'scatter', mode = 'lines', name = 'Historical Sales') %>%
      add_trace(x = forecast_data$month, y = forecast_data$forecast, type = 'scatter', mode = 'lines', name = 'Forecasted Sales') %>%
      layout(title = 'Sales Forecast (Next 6 Months)', xaxis = list(title = 'Month'), yaxis = list(title = 'Sales'))
  })
  
  # Model Summary
  output$model_summary <- renderPrint({
    monthly_sales <- coffee_data %>%
      mutate(month = floor_date(transaction_date, "month")) %>%
      group_by(month) %>%
      summarise(total_sales = sum(transaction_qty * unit_price)) %>%
      arrange(month)
    
    ts_sales <- ts(monthly_sales$total_sales, frequency = 12)
    model <- forecast::auto.arima(ts_sales)
    summary(model)
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
