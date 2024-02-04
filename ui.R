fluidPage(
  
  titlePanel("生存時間シミュレーター"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist",
        "分布:",
        c("指数分布" = "exp", "ワイブル分布" = "weibull", "ガンマ分布" = "gamma")),
      sliderInput("rate",
        "rate(1/scale):",
        min = 0.001,
        max = 2,
        value = 0.01,
        step = 0.001),
      sliderInput("shape",
        "shape:",
        min = 0.5,
        max = 1.5,
        value = 0.8,
        step = 0.01),
      sliderInput("n",
        "N:",
        min = 10,
        max = 5000,
        value = 200,
        step = 10),
      sliderInput("ratio_censor",
        "打ち切りの割合:",
        min = 0,
        max = 1,
        value = 0.2,
        step = 0.01)
    ),
    
    mainPanel(
      plotOutput("survplot", height="800px"),
      verbatimTextOutput("param_survreg"),
      verbatimTextOutput("param_survreg2"),
    )
  )
)
