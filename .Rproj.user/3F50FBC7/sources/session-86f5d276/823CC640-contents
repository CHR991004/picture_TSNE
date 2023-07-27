library(ggplot2)
library(gganimate)
library(png)
library(EBImage)
library(Rtsne)

# 读取图像
img_path <- "Tiananmen_beijing_Panorama.jpg"
img <- EBImage::readImage(img_path)
img <- EBImage::rotate(img, 90)
# 图像大小和块大小
num_rows <- 50
num_cols <- 50

# 计算块的大小
block_size_row <- dim(img)[1] %/% num_rows
block_size_col <- dim(img)[2] %/% num_cols

# 分割图像并计算每块的颜色平均值
average_colors <- array(NA, dim = c(num_rows, num_cols, dim(img)[3]))
for (i in 1:num_rows) {
  for (j in 1:num_cols) {
    block <- img[((i-1)*block_size_row+1):(i*block_size_row), ((j-1)*block_size_col+1):(j*block_size_col), ]
    average_colors[i, j, ] <- colMeans(colMeans(block))
  }
}

# 将三维数组重塑为二维矩阵
average_colors_matrix <- matrix(average_colors, nrow = num_rows * num_cols, ncol = dim(img)[3])

# 添加噪声并运行tsne
average_colors_matrix_noisy <- average_colors_matrix + runif(n = length(average_colors_matrix), min = -0.01, max = 0.01)
tsne_results <- Rtsne(average_colors_matrix_noisy)

# 创建动画数据集
tsne_data <- data.frame(
  frame = c(rep(1:50, each = nrow(tsne_results$Y)), rep(50, each = nrow(tsne_results$Y) * 10)), # 在最后增加10帧相同的帧
  x = rep(tsne_results$Y[, 1], times = 60) + (rep(1:num_cols, each = num_rows) - tsne_results$Y[, 1]) * (rep(c(50:1, rep(0, 10)), each = nrow(tsne_results$Y)) / 50),
  y = rep(tsne_results$Y[, 2], times = 60) + (rep(1:num_rows, times = num_cols) - tsne_results$Y[, 2]) * (rep(c(50:1, rep(0, 10)), each = nrow(tsne_results$Y)) / 50),
  color = rep(rgb(average_colors_matrix, maxColorValue = 1), times = 60)
)

# 创建动画
p <- ggplot(tsne_data, aes(x = x, y = y, color = I(color))) +
  geom_point(size = 5) +
  theme_void() +
  transition_manual(frame) +
  coord_cartesian(xlim = c(min(tsne_data$x), max(tsne_data$x)), ylim = c(min(tsne_data$y), max(tsne_data$y))) 

# 保存动画
anim_save("animation.gif", animate(p, nframes = 60, fps = 10, width = 1600, height = 800, renderer = gifski_renderer()))
