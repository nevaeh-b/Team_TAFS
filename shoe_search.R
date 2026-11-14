shoe_search_ui <- tabPanel("Shoe Search",
         sidebarLayout(
           sidebarPanel(
             radioButtons("src", "Photo", c("Catalogue", "Self-check", "Upload")),
             conditionalPanel("input.src == 'Catalogue'",
                              numericInput("cat_id", "Catalogue id", 1, 1, 2000)),
             conditionalPanel("input.src == 'Self-check'",
                              selectInput("sc_file", "Self-check photo", sprintf("selfcheck_%02d.png", 1:4))),
             conditionalPanel("input.src == 'Upload'",
                              fileInput("photo", "Upload", accept = c(".jpg", ".jpeg", ".png")),
                              checkboxInput("flip", "좌우 반전", FALSE)),
             sliderInput("K_slider", "K (PC 개수)", min = 1, max = 300, value = 50),
             numericInput("K_num", "K 직접 입력", value = 50, min = 1, max = 300)
           ),
           mainPanel(
             h4("Best match"),
             fluidRow(
               column(3, plotOutput("p_query", height = "220px")),
               column(3, plotOutput("p_match", height = "220px")),
               column(3, plotOutput("p_mean",  height = "220px")),
               column(3, plotOutput("p_recon", height = "220px"))
             ),
             h4("Top 5"),
             tableOutput("top5_table"),
             h4("Prediction"),
             textOutput("pred_text")
           )
         )
)







shoe_search_server <- function(input, output, session) {
  
  # K: 슬라이더와 숫자칸 동기화 (값이 다를 때만 업데이트)
  observeEvent(input$K_slider, {
    if (!isTRUE(input$K_num == input$K_slider)) {
      updateNumericInput(session, "K_num", value = input$K_slider)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$K_num, {
    req(input$K_num)
    k <- min(max(round(input$K_num), 1), 300)
    if (!isTRUE(input$K_slider == k)) {
      updateSliderInput(session, "K_slider", value = k)
    }
    if (input$K_num != k) {
      updateNumericInput(session, "K_num", value = k)
    }
  }, ignoreInit = TRUE)
  
  K_now <- reactive({
    req(input$K_slider)
    input$K_slider
  })
  
  # 선택한 사진 -> 4096 벡터
  query_vec <- reactive({
    if (input$src == "Catalogue") {
      req(input$cat_id)
      X[input$cat_id, ]
    } else if (input$src == "Self-check") {
      req(input$sc_file)
      as.vector(readPNG(file.path("selfcheck", input$sc_file)))
    } else {
      req(input$photo)
      prep_upload(input$photo$datapath, tools::file_ext(input$photo$name),
                  flip = isTRUE(input$flip))
    }
  })
  
  # Top 5 (카탈로그 선택일 때만 자기 자신 제외)
  result <- reactive({
    ex <- if (input$src == "Catalogue") input$cat_id else NULL
    find_match(query_vec(), K_now(), exclude_id = ex)
  })
  
  # 선택한 사진을 K개 PC로 복원
  recon_vec <- reactive({
    K <- K_now()
    s <- get_score(query_vec(), K)
    as.vector(x_bar + pca$rotation[, 1:K, drop = FALSE] %*% s)
  })
  
  output$p_query <- renderPlot(show_shoe(query_vec(), "선택한 사진"))
  
  output$p_match <- renderPlot({
    best <- result()$id[1]
    show_shoe(X[best, ], paste("Best match: id", best))
  })
  
  output$p_mean <- renderPlot(show_shoe(x_bar, "평균 신발"))
  
  output$p_recon <- renderPlot({
    v <- pmin(pmax(recon_vec(), 0), 1)
    show_shoe(v, paste("K =", K_now(), "복원"))
  })
  
  output$top5_table <- renderTable(result())
  
  output$pred_text <- renderText({
    pred <- predict_cat(result())
    msg  <- paste("Prediction:", pred)
    if (input$src == "Self-check") {
      truth <- labels$category[labels$photo == input$sc_file]
      msg <- paste0(msg, " / 정답: ", truth, if (pred == truth) " (맞음)" else " (틀림)")
    }
    if (input$src == "Catalogue") {
      truth <- meta$category[input$cat_id]
      msg <- paste0(msg, " / 정답: ", truth, if (pred == truth) " (맞음)" else " (틀림)")
    }
    msg
  })
  
  # 다른 탭에서 사용할 신발 정보와 K
  return(list(
    query_vec = query_vec,
    K_now = K_now,
    
    catalogue_id = reactive({
      if (input$src == "Catalogue") {
        input$cat_id
      } else {
        NULL
      }
    })
  ))
}