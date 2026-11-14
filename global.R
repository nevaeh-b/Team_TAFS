# ===== 1. 패키지 =====
library(shiny)
library(png)
library(jpeg)
library(ggplot2)

# ===== 2. 엔진: 앱 시작 때 딱 한 번 계산 =====

meta  <- read.csv("shoes_meta.csv")
labels <- read.csv("selfcheck/labels.csv")
files <- sprintf("shoes/%04d.png", 1:2000)
X     <- t(sapply(files, function(f) as.vector(readPNG(f))))  # 2000 x 4096
x_bar <- colMeans(X)                                          # 평균 신발
pca <- readRDS("shoe_pca_model.rds")

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
our_files <- list.files("ourshoes", pattern = "\\.(jpg|jpeg|png)$",
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