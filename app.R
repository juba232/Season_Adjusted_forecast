library(shiny)
library(shinydashboard)
library(forecast)
library(seasonal)
library(tseries)
library(plotly)
library(readr)
library(DT)


# Load data
data <- read.csv("Max_temp.csv")
MaxTemp <- ts(data[,3], frequency = 12, start = c(1953,1))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Max Temperature Forecast"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forecast Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("model_type", "Select Model:", choices = c("ARIMA", "ETS")),
      selectInput("decomp", "Seasonal Decomposition:", choices = c("None", "Classical", "X11", "STL")),
      checkboxInput("season_adj", "Seasonal Adjustment", value = TRUE),
      numericInput("horizon", "Forecast Horizon (Months):", 24, min = 1, max = 60),
      downloadButton("downloadData", "Download Forecast")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Forecast Plot", width = 12, plotlyOutput("forecastPlot")),
                box(title = "Forecast Table", width = 12, DTOutput("forecastTable")),
                box(title = "Error Metrics", width = 12, verbatimTextOutput("metrics"))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  forecastReactive <- reactive({
    ts_data <- MaxTemp
    decomp_obj <- NULL
    seasonal_comp <- NULL
    
    if (input$decomp == "Classical") {
      decomp_obj <- decompose(ts_data)
      seasonal_comp <- decomp_obj$seasonal
    } else if (input$decomp == "X11") {
      decomp_obj <- seas(ts_data, x11 = "")
      seasonal_comp <- seasonal(decomp_obj)
    } else if (input$decomp == "STL") {
      decomp_obj <- stl(ts_data, s.window = 5)
      seasonal_comp <- seasonal(decomp_obj)
    }
    
    adjusted <- if (!is.null(seasonal_comp) && input$season_adj) ts_data - seasonal_comp else ts_data
    
    fit <- if (input$model_type == "ARIMA") auto.arima(adjusted) else ets(adjusted)
    
    fcast <- forecast(fit, h = input$horizon)
    
    final_fcast <- if (!is.null(seasonal_comp) && input$season_adj) {
      last_season <- tail(seasonal_comp, 12)
      rep_season <- rep(last_season, length.out = input$horizon)
      fcast$mean + rep_season
    } else {
      fcast$mean
    }
    
    list(forecast = final_fcast, model = fit, fitted = fcast)
  })
  
  output$forecastPlot <- renderPlotly({
    f <- forecastReactive()
    forecast_ts <- ts(f$forecast, start = c(2022,1), frequency = 12)
    p <- autoplot(MaxTemp) +
      autolayer(forecast_ts, series = "Forecast") +
      ggtitle("Forecast of MaxTemp") +
      xlab("Year") + ylab("Max Temperature")
    ggplotly(p)
  })
  
  output$forecastTable <- renderDT({
    f <- forecastReactive()
    data.frame(
      Month = as.character(time(f$forecast)),
      Forecast = as.numeric(f$forecast)
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("forecast_data.csv") },
    content = function(file) {
      f <- forecastReactive()
      write.csv(data.frame(Month = as.character(time(f$forecast)),
                           Forecast = as.numeric(f$forecast)), file, row.names = FALSE)
    }
  )
  
  output$metrics <- renderPrint({
    f <- forecastReactive()
    accuracy(f$fitted)
  })
}

shinyApp(ui, server)
