# ===== 1. 패키지 =====
library(shiny)
library(png)
library(jpeg)
library(ggplot2)

# ===== 2. 엔진: 앱 시작 때 딱 한 번 계산 =====

meta  <- read.csv("data/shoes_meta.csv")
labels <- read.csv("data/selfcheck/labels.csv")
files <- sprintf("data/shoes/%04d.png", 1:2000)
X     <- t(sapply(files, function(f) as.vector(readPNG(f))))  # 2000 x 4096
x_bar <- colMeans(X)                                          # 평균 신발
pca   <- prcomp(X, center = TRUE, scale. = FALSE)

var_ratio <- pca$sdev^2 / sum(pca$sdev^2)
cum_var   <- cumsum(var_ratio)

# ===== 3. 함수들 =====
get_score <- function(v, K) {
  as.vector((v - x_bar) %*% pca$rotation[, 1:K])
}

find_match <- function(v, K, exclude_id = NULL) {
  s <- get_score(v, K)
  d <- sqrt(rowSums((pca$x[, 1:K, drop = FALSE] -
                       matrix(s, nrow = 2000, ncol = K, byrow = TRUE))^2))
  if (!is.null(exclude_id)) d[exclude_id] <- Inf
  ord <- order(d)[1:5]
  data.frame(id = ord, category = meta$category[ord], distance = unname(d[ord]))
}

predict_cat <- function(top5) {
  w <- tapply(1 / top5$distance, top5$category, sum)
  names(which.max(w))
}

# 업로드 사진 전처리: 투명 처리 -> 배경 순백화 -> 신발 crop -> 정사각형 중앙 배치 -> 64x64
prep_upload <- function(path, ext, flip = FALSE, thr = 0.92,
                        fill_w = 0.94, fill_h = 0.75, bottom = 55/64) {
  img <- if (tolower(ext) %in% c("jpg", "jpeg")) readJPEG(path) else readPNG(path)
  if (length(dim(img)) == 3) {
    g <- (img[,,1] + img[,,2] + img[,,3]) / 3
    if (dim(img)[3] == 4) g <- g * img[,,4] + (1 - img[,,4])  # 투명 -> 흰색
    img <- g
  }
  # 1) 흰색에 가까운 배경을 순백으로
  img[img > thr] <- 1
  # 2) 신발 영역만 잘라내기 (bounding box)
  mask <- img < thr
  if (any(mask)) {
    rr <- range(which(rowSums(mask) > 0))
    cc <- range(which(colSums(mask) > 0))
    img <- img[rr[1]:rr[2], cc[1]:cc[2], drop = FALSE]
  }
  # 3) 카탈로그와 같은 규칙으로 배치: 가로 중앙, 바닥은 55/64 행에 정렬
  r <- nrow(img); c <- ncol(img)
  side <- ceiling(max(c / fill_w, r / fill_h))
  sq <- matrix(1, side, side)
  b  <- round(side * bottom)          # 신발 바닥이 놓일 행
  r0 <- b - r                         # 위쪽 여백
  c0 <- floor((side - c) / 2)
  sq[(r0 + 1):(r0 + r), (c0 + 1):(c0 + c)] <- img
  if (flip) sq <- sq[, side:1]        # 좌우 반전
  # 4) 64x64로 블록 평균
  out <- matrix(0, 64, 64)
  for (i in 1:64) for (j in 1:64) {
    rows <- ceiling((i-1)*side/64 + 1):ceiling(i*side/64)
    cols <- ceiling((j-1)*side/64 + 1):ceiling(j*side/64)
    out[i, j] <- mean(sq[rows, cols])
  }
  as.vector(out)
}
show_shoe <- function(v, title = "") {
  df <- data.frame(x = rep(1:64, each = 64), y = rep(64:1, times = 64), value = v)
  ggplot(df, aes(x, y, fill = value)) +
    geom_raster() +
    scale_fill_gradient(low = "black", high = "white", limits = c(0, 1)) +
    coord_fixed() + theme_void() +
    theme(legend.position = "none") + ggtitle(title)
}

# Our Shoes: 팀원 사진 (폴더에 사진 1장 이상 있어야 해)
our_files <- list.files("data/ourshoes", pattern = "\\.(jpg|jpeg|png)$",
                        ignore.case = TRUE, full.names = TRUE)
parts <- strsplit(tools::file_path_sans_ext(basename(our_files)), "_")
cat_raw <- sapply(parts, `[`, 2)
our_df <- data.frame(
  file     = our_files,
  member   = sapply(parts, `[`, 1),
  category = paste0(toupper(substr(cat_raw, 1, 1)), tolower(substring(cat_raw, 2))),
  stringsAsFactors = FALSE
)
our_df$label <- paste(our_df$member, our_df$category, sapply(parts, `[`, 3))
bad <- setdiff(our_df$category, unique(meta$category))
if (length(bad)) warning("카탈로그에 없는 카테고리: ", paste(bad, collapse = ", "))
our_X <- do.call(rbind, lapply(our_df$file, function(f) prep_upload(f, tools::file_ext(f))))

# ===== 4. 화면(UI) =====
ui <- navbarPage("TODO: TeamTAFS",
                 
                 # ---------- 탭 1: Shoe Search ----------
                 tabPanel("Shoe Search",
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
                 ),
                 
                 # ---------- 탭 2: Our Shoes ----------
                 tabPanel("Our Shoes",
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
)

# ===== 5. 서버 =====
server <- function(input, output, session) {
  
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
      as.vector(readPNG(file.path("data/selfcheck", input$sc_file)))
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
  
  # ---- Our Shoes ----
  our_idx <- reactive({ req(input$our_pick); as.integer(input$our_pick) })
  
  output$our_gallery <- renderPlot({
    n <- nrow(our_df)
    par(mfrow = c(2, ceiling(n / 2)), mar = c(1, 1, 2, 1))
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

shinyApp(ui, server)

