our_shoes_ui <- tabPanel("Our Shoes",
         sidebarLayout(
           sidebarPanel(
             selectInput("our_pick", "Choose a shoe",
                         choices = setNames(seq_len(nrow(our_df)), our_df$label)),
             p("K는 Shoe Search의 값을 사용해.")
           ),
           mainPanel(
             h4("All shoes"),
             plotOutput("our_gallery", height = "400px"),
             h4("Best match"),
             fluidRow(
               column(6, plotOutput("our_query", height = "250px")),
               column(6, plotOutput("our_match", height = "250px"))
             ),
             h4("Top 5"),
             tableOutput("our_top5"),
             h4("Prediction"),
             textOutput("our_pred")
           )
         )
)








our_shoes_server <- function(input, output, session, K_now){
  our_idx <- reactive({ req(input$our_pick); as.integer(input$our_pick) })
  our_result <- reactive({
    find_match(our_X[our_idx(), ], K_now())
  })
  
  output$our_gallery <- renderPlot({
    n <- nrow(our_df)
    par(mfrow = c(3, ceiling(n / 3)), mar = c(1, 1, 2, 1))
    for (i in seq_len(n)) {
      m <- matrix(our_X[i, ], 64, 64)
      image(t(m[64:1, ]), col = gray.colors(256, 0, 1), axes = FALSE, asp = 1,
            main = our_df$label[i], cex.main = 0.8)
    }
  })
  
  output$our_query <- renderPlot(show_shoe(our_X[our_idx(), ], our_df$label[our_idx()]))
  
  output$our_match <- renderPlot({
    best <- our_result()$id[1]
    show_shoe(X[best, ], paste("Best match: id", best))
  })
  
  output$our_top5 <- renderTable(our_result())
  
  output$our_pred <- renderText({
    pred  <- predict_cat(our_result())
    truth <- our_df$category[our_idx()]
    paste0("Prediction: ", pred, " / 정답: ", truth,
           if (pred == truth) " (맞음)" else " (틀림)")
  })
}
