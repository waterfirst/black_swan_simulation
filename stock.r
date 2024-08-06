
library(shiny)
library(deSolve)
library(ggplot2)
library(scales)



# 수정된 모델 함수
market_model <- function(t, state, params) {
  with(as.list(c(state, params)), {
    # 블랙 스완 이벤트 체크
    black_swan <- ifelse(t >= black_swan_time && t < black_swan_time + black_swan_duration, 1, 0)
    
    # 수정된 반응 속도 계산
    k1_eff <- k1 * exp(-E_a / (R * T)) * (1 + black_swan * black_swan_intensity)
    k2_eff <- k2 * exp(-E_a / (R * T)) * (1 + black_swan * black_swan_intensity)
    
    # 수정된 주가 지수 변화율
    dS <- -k1_eff * S - k2_eff * P * S * (1 + leverage * P) + noise * rnorm(1) - black_swan * S * black_swan_impact
    
    # 수정된 프로그램 매매 압력 변화율
    dP <- ifelse(S < threshold * S0 || black_swan == 1, k1_eff * (1 + panic_factor * (1 - S/S0)), -k1_eff * P)
    
    # 거래량 변화율
    dV <- k2_eff * P * S * (1 + black_swan * trading_volume_spike)
    
    return(list(c(dS, dP, dV)))
  })
}

# UI 정의
ui <- fluidPage(
  titlePanel("급격한 주가 하락 시뮬레이션"),
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
      numericInput("sim_time", "시뮬레이션 기간:", 100, min = 50, max = 200),
      actionButton("run", "시뮬레이션 실행")
    ),
    mainPanel(
      plotOutput("marketPlot"),
      verbatimTextOutput("summary")
    )
  )
)


# 모델 함수는 이전과 동일

# UI 정의 (이전과 동일)

# 서버 로직
server <- function(input, output) {
  sim_data <- eventReactive(input$run, {
    params <- c(
      k1 = input$k1,
      k2 = input$k2,
      threshold = input$threshold,
      E_a = input$E_a,
      R = 8.314,
      T = input$T,
      noise = input$noise,
      S0 = input$S0,
      leverage = input$leverage,
      panic_factor = input$panic_factor,
      black_swan_time = input$black_swan_time,
      black_swan_duration = input$black_swan_duration,
      black_swan_intensity = input$black_swan_intensity,
      black_swan_impact = input$black_swan_impact,
      trading_volume_spike = input$trading_volume_spike
    )
    
    initial_state <- c(S = input$S0, P = 0, V = 0)
    times <- seq(0, input$sim_time, by = 1)
    out <- ode(y = initial_state, times = times, func = market_model, parms = params)
    as.data.frame(out)
  })
  output$marketPlot <- renderPlot({
    data <- sim_data()
    data$cumulative_volume <- cumsum(data$V)
    
    # 주가 지수의 y축 범위 계산
    y_range_index <- range(data$S)
    y_range_index_adjusted <- y_range_index + c(-0.1, 0.1) * diff(y_range_index)
    
    # 누적 거래량의 y축 범위 계산
    y_range_volume <- c(0, max(data$cumulative_volume))
    
    # 그래프 생성
    ggplot(data, aes(x = time)) +
      # 주가 지수 선 그래프
      geom_line(aes(y = S, color = "주가 지수")) +
      # 누적 거래량 막대 그래프
      geom_col(aes(y = cumulative_volume / max(cumulative_volume) * diff(y_range_index_adjusted) + y_range_index_adjusted[1], 
                   fill = "누적 거래량"), alpha = 0.3) +
      # 주가 지수용 y축 (왼쪽)
      scale_y_continuous(
        name = "주가 지수",
        limits = y_range_index_adjusted,
        sec.axis = sec_axis(~ (. - y_range_index_adjusted[1]) / diff(y_range_index_adjusted) * diff(y_range_volume),
                            name = "누적 거래량")
      ) +
      labs(x = "시간", title = "주가 지수 및 누적 거래량 변화") +
      theme_minimal() +
      theme(
        axis.title.y.left = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "green"),
        legend.position = "bottom"
      ) +
      scale_color_manual(values = c("주가 지수" = "blue")) +
      scale_fill_manual(values = c("누적 거래량" = "green")) +
      guides(color = guide_legend(title = NULL),
             fill = guide_legend(title = NULL))
  })
  
  output$sentimentPlot <- renderPlot({
    data <- sim_data()
    ggplot(data, aes(x = time)) +
      geom_line(aes(y = Sentiment, color = "투자자 심리")) +
      geom_line(aes(y = VIX / 100, color = "VIX (scaled)")) +
      scale_y_continuous(name = "투자자 심리", sec.axis = sec_axis(~.*100, name = "VIX")) +
      labs(x = "시간", title = "투자자 심리 및 VIX 변화") +
      theme_minimal() +
      scale_color_manual(values = c("투자자 심리" = "purple", "VIX (scaled)" = "orange")) +
      guides(color = guide_legend(title = NULL))
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

