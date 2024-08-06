library(shiny)
library(lubridate)
library(httr)
library(rvest)
library(xts)
library(data.table)
library(dplyr)
library(prophet)
library(plotly)

# 유틸리티 함수
t2n <- function(x) { as.numeric( as.POSIXct(strptime(x, "%Y-%m-%d %H:%M:%OS")) )  }
n2t <- function(x) { format(as.POSIXct(x, origin = "1970-01-01 09:00:00", tz="UTC"), "%Y-%m-%d %H:%M:%OS") }

# UI 정의
ui <- fluidPage(
  titlePanel("KOSPI 지수 예측"),
  sidebarLayout(
    sidebarPanel(
      numericInput("days_to_predict", "예측 일수:", 90, min = 1, max = 365),
      actionButton("update", "데이터 업데이트 및 예측")
    ),
    mainPanel(
      plotlyOutput("predictionPlot"),
      verbatimTextOutput("summary")
    )
  )
)

# 서버 로직
server <- function(input, output, session) {
  
  getData <- reactive({
    input$update
    
    key = "KOSPI"
    url = paste0("https://fchart.stock.naver.com/sise.nhn?symbol=", key, "&timeframe=day&count=1000&requestType=0")
    data = GET(url) %>% read_html %>% html_nodes("item") %>% html_attr("data") %>% strsplit("\\|")
    
    df = data.table( ds = sapply(data, function(x) { x[1] } ), y = sapply(data, function(x) { x[4] } ) )
    df$y = df$y %>% as.double()
    df$ds = paste(paste0(substr(df$ds, 1, 4), "-", substr(df$ds, 5, 6), "-", substr(df$ds, 7, 8)), "00:00:00")
    df$ds_value = t2n(df$ds)
    df = data.table( df %>% filter(y != 0) %>% filter( ds >= "2023-08-01" ) )
    
    n_pred = input$days_to_predict
    df = rbind(df, data.table( ds = n2t( (seq( t2n(tail(df, 1)$ds) + 60*60*24*1, t2n(tail(df, 1)$ds) + 60*60*24*(n_pred), by = 60*60*24)) ), y= NA, ds_value = NA ))
    df$ds_value = t2n(df$ds)
    
    return(df)
  })
  
  modelData <- reactive({
    df <- getData()
    m = prophet(na.omit(df), yearly.seasonality=T, weekly.seasonality = TRUE, daily.seasonality = TRUE)
    m_pred = predict(m, df)
    df$yhat = m_pred$yhat
    df$yhat_lower = m_pred$yhat_lower
    df$yhat_upper = m_pred$yhat_upper
    return(list(df = df, m = m, m_pred = m_pred))
  })
  
  output$predictionPlot <- renderPlotly({
    model_data <- modelData()
    df <- model_data$df
    
    p <- plot_ly() %>%
      add_trace(x = ~ds, y = ~y, data = df, type = 'scatter', mode = 'markers+lines', name = '실제 데이터',
                marker = list(color = ~y, colorscale = 'Viridis')) %>%
      add_trace(x = ~ds, y = ~yhat, data = df, type = 'scatter', mode = 'lines', name = '예측', line = list(color = 'black', dash = 'dash')) %>%
      add_trace(x = ~ds, y = ~yhat_lower, data = df, type = 'scatter', mode = 'lines', name = '하한', line = list(color = 'gold')) %>%
      add_trace(x = ~ds, y = ~yhat_upper, data = df, type = 'scatter', mode = 'lines', name = '상한', line = list(color = 'coral')) %>%
      layout(title = "KOSPI 지수 예측",
             xaxis = list(title = "날짜"),
             yaxis = list(title = "KOSPI 지수"),
             shapes = list(
               list(
                 type = "rect",
                 fillcolor = "rgba(0, 255, 0, 0.2)",
                 line = list(color = "rgba(0, 255, 0, 0)"),
                 x0 = df$ds[which(is.na(df$y))[1]],
                 x1 = tail(df$ds, 1),
                 y0 = min(df$y, na.rm = TRUE),
                 y1 = max(df$y, na.rm = TRUE)
               )
             ))
    
    return(p)
  })
  
  output$summary <- renderPrint({
    model_data <- modelData()
    df <- model_data$df
    
    cat("예측 요약:\n")
    cat("어제 종가:", round(tail(df$y[!is.na(df$y)], 2)[1], 2), "\n")
    cat("오늘 예측 종가:", round(tail(df$yhat, 2)[1], 2), "\n")
    cat("오늘 예측 변동폭:", round(tail(df$yhat_upper - df$yhat_lower, 2)[1], 2), "\n")
    cat("내일 예측 종가:", round(tail(df$yhat, 1), 2), "\n")
    cat("내일 예측 변동폭:", round(tail(df$yhat_upper - df$yhat_lower, 1), 2), "\n")
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
