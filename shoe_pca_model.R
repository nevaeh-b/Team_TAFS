library(png)


shoe_files <- list.files('shoes',pattern ='\\.png$', full.names=TRUE)

img_list <- lapply(shoe_files, function(file) {
  as.vector(readPNG(file))})
shoe_matrix <- do.call(rbind, img_list)

dim(shoe_matrix)


shoe_pca <- prcomp(shoe_matrix, center=TRUE, scale.=FALSE)


length(shoe_pca$center)
dim(shoe_pca$rotation)
dim(shoe_pca$x)

saveRDS(shoe_pca, "shoe_pca_model.rds")
