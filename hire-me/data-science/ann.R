library(ggplot2)
library(dplyr)

dat <- data.frame(
  layer = c(rep(0, 11), rep(1, 21),  rep(2, 27),  rep(3, 11),  rep(4, 1)),
  y =     c(45:55,      40:60,       37:63,       45:55,       50),
  x =     c(rep(0, 11), rep(25, 21), rep(50, 27), rep(75, 11), rep(100, 1))
) 

con01 <- tidyr::crossing(
  filter(dat, layer == 0)$y, 
  filter(dat, layer == 1)$y
  ) %>%
  mutate(x = 0, xend = 25) %>%
  rename(y = 1, yend = 2)

con12 <- tidyr::crossing(
  filter(dat, layer == 1)$y, 
  filter(dat, layer == 2)$y
) %>%
  mutate(x = 25, xend = 50) %>%
  rename(y = 1, yend = 2)

con23 <- tidyr::crossing(
  filter(dat, layer == 2)$y, 
  filter(dat, layer == 3)$y
) %>%
  mutate(x = 50, xend = 75) %>%
  rename(y = 1, yend = 2)

con34 <- tidyr::crossing(
  filter(dat, layer == 3)$y, 
  filter(dat, layer == 4)$y
) %>%
  mutate(x = 75, xend = 100) %>%
  rename(y = 1, yend = 2)

pl <- dat %>%
  ggplot(aes(x = x, y = y)) +
    geom_segment(
      data = con01, 
      mapping = aes(x = x, y = y, xend = xend, yend = yend), 
      alpha = 0.6, color = "#e7e7e7") +
    geom_segment(
      data = con12, 
      mapping = aes(x = x, y = y, xend = xend, yend = yend), 
      alpha = 0.6, color = "#e7e7e7") +
    geom_segment(
      data = con23, 
      mapping = aes(x = x, y = y, xend = xend, yend = yend), 
      alpha = 0.6, color = "#e7e7e7") +
    geom_segment(
      data = con34, 
      mapping = aes(x = x, y = y, xend = xend, yend = yend), 
      alpha = 0.6, color = "#e7e7e7") +
  
    geom_point(aes(x = x, y = y), data = filter(dat, layer == 0), pch = 21, size = 4, fill = "#cfb7ae") +
    geom_point(aes(x = x, y = y), data = filter(dat, layer %in% c(1:3)), pch = 21, size = 3, fill = "#aec6cf") +
    geom_point(aes(x = x, y = y), data = filter(dat, layer == 4), pch = 21, size = 5, fill = "#cfb7ae") +
    theme_void()

ggsave(filename = "ann.jpg", plot = pl, width = 8, height = 5, bg = "#232323", dpi = 300)
