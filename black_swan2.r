
library(shiny)
library(deSolve)
library(ggplot2)
library(plotly)
library(scales)


# 수정된 모델 함수
market_model <- function(t, state, params) {
  with(as.list(c(state, params)), {
    # 블랙 스완 이벤트 체크
    black_swan <- ifelse(t >= black_swan_time && t < black_swan_time + black_swan_duration, 1, 0)
    
    # 외부 뉴스 영향
    news_impact <- rnorm(1, mean = 0, sd = news_volatility)
    
    # 수정된 반응 속도 계산
    k1_eff <- k1 * exp(-E_a / (R * T)) * (1 + black_swan * black_swan_intensity) * (1 + VIX/100)
    k2_eff <- k2 * exp(-E_a / (R * T)) * (1 + black_swan * black_swan_intensity) * (1 + VIX/100)
    
    # 주가 지수 변화율
    dS <- -k1_eff * S - k2_eff * P * S * (1 + leverage * P) * (2 - Sentiment) + 
      noise * rnorm(1) - black_swan * S * black_swan_impact + news_impact * S/10
    
    # 프로그램 매매 압력 변화율
    dP <- ifelse(S < threshold * S0 || black_swan == 1, 
                 k1_eff * (1 + panic_factor * (1 - Sentiment)), 
                 -k1_eff * P)
    
    # 거래량 변화율
    dV <- k2_eff * P * S * (1 + black_swan * trading_volume_spike) * (2 - Sentiment)
    
    # 투자자 심리 변화율
    dSentiment <- -a * (Sentiment - S0/S) + news_impact
    
    # VIX 변화율
    dVIX <- b * (1/Sentiment - 1) - c * VIX + abs(news_impact)
    
    return(list(c(dS, dP, dV, dSentiment, dVIX)))
  })
}

# UI 정의
ui <- fluidPage(
  titlePanel("인터랙티브 주식 시장 시뮬레이션"),
  sidebarLayout(
    sidebarPanel(
      numericInput("S0", "초기 주가 지수 (S0):", 2900, min = 1000, max = 5000),
      sliderInput("k1", "초기 반응 속도 (k1):", min = 0, max = 0.1, value = 0.05, step = 0.01),
      sliderInput("k2", "연쇄 반응 속도 (k2):", min = 0, max = 0.05, value = 0.025, step = 0.005),
      sliderInput("threshold", "프로그램 매매 임계값:", min = 0.9, max = 0.99, value = 0.95, step = 0.01),
      sliderInput("E_a", "활성화 에너지 (E_a):", min = 10, max = 100, value = 50, step = 5),
      sliderInput("T", "시장 온도 (T):", min = 250, max = 350, value = 300, step = 10),
      sliderInput("noise", "시장 노이즈:", min = 0, max = 0.1, value = 0.05, step = 0.01),
      sliderInput("leverage", "레버리지 효과:", min = 0, max = 2, value = 1, step = 0.1),
      sliderInput("panic_factor", "공황 요인:", min = 0, max = 2, value = 1, step = 0.1),
      numericInput("black_swan_time", "블랙 스완 발생 시점:", 50, min = 1, max = 100),
      numericInput("black_swan_duration", "블랙 스완 지속 기간:", 10, min = 1, max = 50),
      sliderInput("black_swan_intensity", "블랙 스완 강도:", min = 0, max = 2, value = 1, step = 0.1),
      sliderInput("black_swan_impact", "블랙 스완 충격:", min = 0, max = 0.1, value = 0.05, step = 0.01),
      sliderInput("trading_volume_spike", "거래량 급증 요인:", min = 0, max = 5, value = 2, step = 0.5),
      sliderInput("news_volatility", "뉴스 변동성:", min = 0, max = 0.1, value = 0.02, step = 0.01),
      sliderInput("a", "투자자 심리 조정 속도:", min = 0, max = 1, value = 0.1, step = 0.05),
      sliderInput("b", "VIX 상승 계수:", min = 0, max = 1, value = 0.2, step = 0.05),
      sliderInput("c", "VIX 하락 계수:", min = 0, max = 1, value = 0.1, step = 0.05),
      sliderInput("circuit_breaker_threshold", "서킷 브레이커 발동 임계값:", min = 0.8, max = 0.95, value = 0.9, step = 0.01),
      sliderInput("circuit_breaker_effect", "서킷 브레이커 효과:", min = 0, max = 1, value = 0.5, step = 0.1),
      numericInput("sim_time", "시뮬레이션 기간:", 100, min = 50, max = 200),
      actionButton("run", "시뮬레이션 실행")
    ),
    mainPanel(
      plotlyOutput("marketPlot"),
      plotlyOutput("sentimentPlot"),
      verbatimTextOutput("summary")
    )
  )
)



# 수정된 서버 로직
server <- function(input, output) {
  sim_data <- eventReactive(input$run, {
    params <- c(
      k1 = input$k1, k2 = input$k2, threshold = input$threshold, E_a = input$E_a,
      R = 8.314, T = input$T, noise = input$noise, S0 = input$S0,
      leverage = input$leverage, panic_factor = input$panic_factor,
      black_swan_time = input$black_swan_time, black_swan_duration = input$black_swan_duration,
      black_swan_intensity = input$black_swan_intensity, black_swan_impact = input$black_swan_impact,
      trading_volume_spike = input$trading_volume_spike, news_volatility = input$news_volatility,
      a = input$a, b = input$b, c = input$c,
      circuit_breaker_threshold = input$circuit_breaker_threshold,
      circuit_breaker_effect = input$circuit_breaker_effect
    )
    
    initial_state <- c(S = input$S0, P = 0, V = 0, Sentiment = 1, VIX = 20)
    times <- seq(0, input$sim_time, by = 1)
    out <- ode(y = initial_state, times = times, func = market_model, parms = params)
    as.data.frame(out)
  })
  
  output$marketPlot <- renderPlotly({
    data <- sim_data()
    data$cumulative_volume <- cumsum(data$V)
    
    p <- plot_ly() %>%
      add_trace(x = ~time, y = ~S, data = data, type = 'scatter', mode = 'lines', 
                name = '주가 지수', yaxis = "y1", line = list(color = 'blue')) %>%
      add_bars(x = ~time, y = ~cumulative_volume, data = data, name = '누적 거래량', 
               yaxis = "y2", marker = list(color = 'rgba(0, 255, 0, 0.3)')) %>%
      layout(
        title = "주가 지수 및 누적 거래량 변화",
        xaxis = list(title = "시간"),
        yaxis = list(title = "주가 지수", side = "left", showgrid = FALSE),
        yaxis2 = list(title = "누적 거래량", side = "right", overlaying = "y", showgrid = FALSE),
        legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center"),
        hovermode = "x unified"
      )
    
    p
  })
  
  output$sentimentPlot <- renderPlotly({
    data <- sim_data()
    
    p <- plot_ly() %>%
      add_trace(x = ~time, y = ~Sentiment, data = data, type = 'scatter', mode = 'lines', 
                name = '투자자 심리', yaxis = "y1", line = list(color = 'purple')) %>%
      add_trace(x = ~time, y = ~VIX, data = data, type = 'scatter', mode = 'lines', 
                name = 'VIX', yaxis = "y2", line = list(color = 'orange')) %>%
      layout(
        title = "투자자 심리 및 VIX 변화",
        xaxis = list(title = "시간"),
        yaxis = list(title = "투자자 심리", side = "left", showgrid = FALSE),
        yaxis2 = list(title = "VIX", side = "right", overlaying = "y", showgrid = FALSE),
        legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center"),
        hovermode = "x unified"
      )
    
    p
  })
  
  output$summary <- renderPrint({
    data <- sim_data()
    cat("시뮬레이션 요약:\n")
    cat("초기 주가 지수:", round(data$S[1], 2), "\n")
    cat("최종 주가 지수:", round(tail(data$S, 1), 2), "\n")
    cat("최대 하락률:", round((min(data$S) - data$S[1]) / data$S[1] * 100, 2), "%\n")
    cat("최대 VIX:", round(max(data$VIX), 2), "\n")
    cat("최소 투자자 심리:", round(min(data$Sentiment), 4), "\n")
    cat("총 거래량:", round(sum(data$V), 2), "\n")
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
