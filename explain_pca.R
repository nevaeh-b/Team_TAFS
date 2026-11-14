library(png)
library(ggplot2)
library(shiny)
library(bslib)

shoe_pca <- pca

explained_var <- summary(shoe_pca)$importance[2, ]
cumulative_var <- summary(shoe_pca)$importance[3,]



shoes_meta <- read.csv("shoes_meta.csv")
score_df <- data.frame(
  PC1 = shoe_pca$x[, 1],
  PC2 = shoe_pca$x[, 2],
  Category = shoes_meta$category)


explain_pca_ui <- tabPanel(
  'Explain PCA',
  
  br(),
  br(),
  
  
  layout_columns(
    col_widths = c(6, 6),
    
    div(
      h3('Step 1. Average Shoe'),
      plotOutput('average_plot', height = '350px')
    ),
    
    div(
      h3('Step 2. Photo - Average'),
      p('with the shoe selected in the Shoe Search tab', style = 'font-size: 12px; color: gray;'),
      
      plotOutput('selected_plot', height = '350px'),
      p('White: Brighter than average | Gray: Similar to average | Black: Darker than average ', style = 'font-size: 12px; color: gray;')
    )
  ),
  
  br(),
  br(),
  
  
  h3('Step 3. Score map'),
  plotOutput('score_plot'),
  
  br(),
  hr(),
  
  sliderInput(
    inputId = 'K',
    label = 'Select K (Step 4-6)',
    min = 1, max=300,
    value = 3,
    step=1
  ),
  
  br(),
  br(),
  
  
  h3('Step4. Eigenshoes'),
  plotOutput('eigenshoe_plot'),
  br(),
  plotOutput('individual_variance_plot'),
  textOutput('cumulative_variance_text'),
  
  br(),
  br(),
  
  
  h3('Step 5. PC Scores'),
  tableOutput('scores_table'),
  
  br(), 
  br(),
  

  h3('Step6. Rebuild'),
  plotOutput("rebuild_plot", height = '700px'),
  
  br(), 
  br(),
  hr(),
  
  sliderInput(
    inputId = 'K_more',
    label = 'Select K (Step 7)',
    min = 1, max=300,
    value = 3,
    step=1
  ),
  
  h3('Step7. More About PCA'),
  
  h5('7-1. Why PCA'),
  p('PCA can reduce the dimensionality of shoe images while preserving as much variance as possible.'),
  
  
  h4('7-2. Why Subtract the Average?'),
  p('Subtracting the average shoe makes the data centered around zero, allowing PCA to focus on variations between shoes.'),
  
  
  h4('7-3. Why Maximum Variance?'),
  p('Compare how the 2,000 shoes spread along PC1 and the selected PC.'),
  
  plotOutput('projection_plot'),
  br(),
  h6(' -> PCA selects directions with the greatest variance because they can preserve the most information about differences between data points, minimizing information loss during dimensionality reduction.'),
  
  
  h4('7-4. How Does PCA Find Similar Shoes?'),
  p('PCA represents each shoe using PC scores. Similar shoes can be found by comparing distances between their score vectors.'),


layout_columns(
  col_widths = c(6, 6),
  
  div(
    h4('Selected Shoe'),
    plotOutput('match_selected_plot', height = '300px')
  ),
  
  div(
    h4('Closest Match Using Selected K'),
    textOutput('match_closest_id'),
    plotOutput('match_closest_plot')
  )
  ),
  
  textOutput('match_distance_text')
)





# Shoe Search에서 선택한 사진을 사용
# selected_shoe() = 4096개 픽셀값
explain_pca_server <- function(input, output,  session,
                   selected_shoe, catalogue_id){
  
  
  # step1
  output$average_plot <- renderPlot({
    average_shoe <- shoe_pca$center
    average_image <- matrix(average_shoe, 64, 64)
    
    plot(as.raster(average_image))
  })
  
  
  
  # step2
  output$selected_plot <- renderPlot({
    difference <- selected_shoe() - shoe_pca$center
    difference_image <- matrix(difference, 64, 64)
    
    plot(as.raster((difference_image + 1)/2))
    })
  
  
  
  # step3
  output$score_plot <- renderPlot({
    scores <- get_score(selected_shoe(), 2) # 선택한 사진의 PC1, PC2 점수 직접 계산
    selected_point <- data.frame(
      PC1 = scores[1],
      PC2 = scores[2]
    )
    
    ggplot(score_df, aes(x = PC1, y=PC2, color = Category)) + 
      geom_point(size = 1.5, alpha = 0.65) + 
      geom_point(
        data = selected_point,
        aes(x = PC1, y = PC2),
        shape = 23,
        fill = '#FFD43B',
        color = 'black',
        stroke = 1.2,
        size = 6
      ) +
      scale_color_manual(
        values = c(
          'Boots' = '#E76F51',
          'Sandals' = '#2A9D8F',
          'Shoes' = '#457B9D',
          'Slippers' = '#9B5DE5'
        )
      ) +
      theme_minimal()
    })
  
  
  
  # step4
  output$eigenshoe_plot <- renderPlot({
    selected_pcs <- c(1, 2, as.integer(input$K))
    
    par(mfrow = c(1, 3), mar = c(3, 1, 5, 1))
    
    for (j in seq_along(selected_pcs)) {
      i <- selected_pcs[j]
      
      pc <- shoe_pca$rotation[, i]
      pc_image <- matrix(pc, 64, 64)
      
      pc_minmax <- (pc_image - min(pc_image)) /
        (max(pc_image) - min(pc_image))
      
      plot(as.raster(pc_minmax))
      
      if (j == 3) {
        mtext(paste0('PC', i, ' (Selected)'),
              side = 3, line = 2, cex = 1.2)
      } else {
        mtext(paste0('PC', i),
              side = 3, line = 2, cex = 1.2)
      }
    }
  })
  
  output$individual_variance_plot <- renderPlot({
    selected_pcs <- c(1, 2, as.integer(input$K))
    selected_var <- explained_var[selected_pcs] * 100
    
    bar_positions <- barplot(
      selected_var,
      names.arg = c('PC1', 'PC2',
                    paste0('PC', input$K, ' (Selected)')),
      main = 'Individual Explained Variance',
      xlab = 'Principal Component',
      ylab = 'Explained Variance (%)',
      ylim = c(0, max(selected_var) * 1.2),
      cex.names = 0.8
    )
    
    text(
      bar_positions,
      selected_var,
      labels = paste0(round(selected_var, 4), '%'),
      pos = 3
    )
    
  })
  
  output$cumulative_variance_text <- renderText({
    K <- as.integer(input$K)
    
    paste0(
      'Cumulative Explained Variance (First ', K, ' PC(s)): ', round(cumulative_var[K]*100, 2), '%'
    )
  })
  
  
  
  #step5
  output$scores_table <- renderTable({
    K <- as.integer(input$K)
    scores <- get_score(selected_shoe(), K)
    
    
    data.frame(
      PC = paste0('PC', 1:K),
      Scores = round(scores, 4)
    )
  })
  
  
  
  # step6
  output$rebuild_plot <-renderPlot({
    selected_K <- as.integer(input$K)
    
    K_values <- c(1, 20, 100, 300, selected_K)
    par(mfrow = c(2, 3), mar = c(2, 1, 3, 1))
    
    original <- selected_shoe()
    plot(as.raster(matrix(original, 64, 64)))
    title(main ='Original')
    
    for (j in seq_along(K_values)) {
      K <- K_values[j]
      scores <- get_score(original, K) #
      eigenshoes <- shoe_pca$rotation[, 1:K, drop=FALSE]
      rebuilt <- shoe_pca$center + as.vector(eigenshoes %*%scores)
      rebuilt <- pmin(pmax(rebuilt, 0), 1)
      
      plot(as.raster(matrix(rebuilt, 64, 64)))
      if (j == 5) {
        title(main = paste0("First ", K, " PCs (Selected)"))
      } else if (j == 1) {
        title(main =  paste0("First 1 PC"))
      } else {
        title(main = paste0("First ", K, " PCs"))
      }
    }
  })
  
  
  # step 7-3
  output$projection_plot <- renderPlot({
    K <- as.integer(input$K_more)
    set.seed(123)
    
    pc1_scores <- shoe_pca$x[, 1]
    selected_scores <- shoe_pca$x[, K]
    
    x_range <- range(c(pc1_scores, selected_scores))
    par(mfrow = c(1, 2), mar = c(4, 3, 3, 1))
    
    stripchart(
      pc1_scores,
      method = 'jitter',
      jitter = 0.15, 
      pch = 16,
      cex = 0.5,
      xlim = x_range,
      main = 'PC1 Projection',
      xlab = 'PC Score'
    )
    
    stripchart(
      selected_scores,
      method = 'jitter',
      jitter = 0.15,
      pch = 16,
      cex = 0.5,
      xlim = x_range,
      main = paste0('PC', K, ' Projection (Selected)'),
      xlab = 'PC Score'
    )
  })
  
  
  #step. 7-4
  closest_match <- reactive({
    K <- as.integer(input$K_more)
    
    scores <- shoe_pca$x[, 1:K, drop=FALSE]
    selected_scores <- get_score(selected_shoe(), K)
    differences <- sweep(scores, 2, selected_scores,'-')
    distances <- sqrt(rowSums(differences^2))
    
    shoe_id <- catalogue_id()
    if (!is.null(shoe_id)) {
      distances[shoe_id] <- Inf
    }
    closest_id <- which.min(distances)
    
    list(
      id = closest_id, distance = distances[closest_id]
    )
  })
  
  output$match_selected_plot <- renderPlot({
    plot(as.raster(matrix(selected_shoe(), 64, 64)))
  })
  
  output$match_closest_id <- renderText({
    closest_id <- closest_match()$id
    
    paste0('Shoe #', sprintf('%04d', closest_id))
  })
  
  output$match_closest_plot <- renderPlot({
    closest_id <- closest_match()$id
    shoe_file <- sprintf('shoes/%04d.png', closest_id)
    closest_image <- as.vector(readPNG(shoe_file))
    
    plot(as.raster(matrix(closest_image, 64, 64)))
    
  })

  output$match_distance_text <- renderText({
    K <- as.integer(input$K_more)
    distance <- closest_match()$distance
    
    paste0(
      'Distance in PC Space (First ', K, ' PCs): ', round(distance, 4)
    )
  })
}

