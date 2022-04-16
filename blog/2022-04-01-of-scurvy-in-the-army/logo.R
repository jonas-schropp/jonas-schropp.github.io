suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(here))
library(magrittr)
library(ggplot2)
library(ggpattern)
library(geomtextpath)
library(ggtext)
library(jpeg)

# Read in the data
scurvy1 <- readr::read_csv(here("content/blog/2022-04-01-of-scurvy-in-the-army", "TableNo2.csv"), show_col_types = FALSE) %>%
  select(-`Estimated Average Monthly Strength of the Army`) %>%   
  tidyr::pivot_longer(
    cols = c("Zymotic Diseases", "Wounds and Injuries", "All other Causes"), 
    names_to = "Cause of Death"
  ) %>%
  mutate(
    Month = factor(Month, levels = month.name, labels = month.name),
    value = if_else(is.na(value), 0, value),
    `Cause of Death` = factor(
      `Cause of Death`, 
      levels = c("Zymotic Diseases", "Wounds and Injuries", "All other Causes"))
  ) %>% 
  mutate(r = sqrt(2*value / 30)) %>%
  filter(
    Year == 1854 | Year == 1855 & Month %in% c("January", "February", "March")
  ) %>%
  arrange(Year, Month) %>%
  mutate(
    tmp = as.factor(rep(1:12, each = 3)),
    `Cause of Death` = factor(
      `Cause of Death`, 
      levels = c("All other Causes", "Wounds and Injuries", "Zymotic Diseases")
    )
  ) 


scurvy1a <- scurvy1 %>%
  group_by(tmp) %>%
  filter(`Cause of Death` == "Zymotic Diseases") %>% 
  ungroup()
scurvy1b <- scurvy1 %>%
  group_by(tmp) %>%
  filter(`Cause of Death` == "All other Causes" & r > 0) %>% 
  ungroup()
scurvy1c <- scurvy1 %>%
  group_by(tmp) %>%
  filter(`Cause of Death` == "Wounds and Injuries" & r > 0) %>% 
  ungroup()


images <- c(
  here("content/blog/2022-04-01-of-scurvy-in-the-army", "color_death.jpg"), 
  here("content/blog/2022-04-01-of-scurvy-in-the-army", "color_zymotic.jpg"), 
  here("content/blog/2022-04-01-of-scurvy-in-the-army", "color_other.jpg")
)

labels <- scurvy1a %>% 
  mutate(
    label = c(
      "APRIL\n1854", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", 
      "DECEMBER", "JANUARY 1855", "FEBRUARY", "MARCH 1855"
    ),
    y = if_else(
      r < 4, 3.8, r + 0.5
    ),
    `Cause of Death` = NA
  )

plot1 <- scurvy1 %>% 
  filter(r > 0) %>%
  ggplot() + 
  geom_bar_pattern(
    data = scurvy1a,
    aes(tmp, r, color = `Cause of Death`, pattern_filename = `Cause of Death`),
    stat = "identity", 
    position = "identity",  
    alpha = 0.7,
    pattern = "image",    # rather than color we use pattern from image
    pattern_type = "tile",
    width = 1,    # to remove space between bars
    size = 0.4
  ) + 
  geom_bar_pattern(
    data = scurvy1b,
    aes(tmp, r, color = `Cause of Death`, pattern_filename = `Cause of Death`),
    stat = "identity", 
    position = "identity",  
    alpha = 0.7,
    pattern = "image",    # rather than color we use pattern from image
    pattern_type = "tile",
    width = 1,    # to remove space between bars
    size = 0.4
  ) + 
  geom_bar_pattern(
    data = scurvy1c,
    aes(tmp, r, color = `Cause of Death`, pattern_filename = `Cause of Death`),
    stat = "identity", 
    position = "identity", 
    alpha = 0.7,
    pattern = "image",    # rather than color we use pattern from image
    pattern_type = "tile",
    width = 1,    # to remove space between bars
    size = 0.4
  ) +
  scale_pattern_filename_manual(
    values = c(
      "Zymotic Diseases" = "color_zymotic.jpg", 
      "Wounds and Injuries" = "color_death.jpg", 
      "All other Causes" = "color_other.jpg")
  )  +
  scale_color_manual(
    values = c(
      "Zymotic Diseases" = "#42514f", 
      "Wounds and Injuries" = "#ae7e79", 
      "All other Causes" = "black")
  ) +
  scale_y_continuous(limits = c(0, 14.25)) +
  # because ggplot's geom_text will not be curved with coord_polar
  geom_textpath(  
    mapping = aes(x = tmp, y = y, label = label), 
    data = labels, 
    color = "black",
    upright = FALSE,
    fontface = "bold",
    # Turn the letters upside down - 
    # otherwise they'll be upside down once we change coord
    angle = 180
  ) +
  theme_void() +
  # remove all the clutter we don't need
  theme(
    legend.position = "none",
    panel.background = element_rect(fill='transparent', color='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color='transparent'), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank() #remove minor gridlines
  ) +
  # the one black line she describes in the legend
  annotate(geom = "segment", x = 7.5, xend = 8.5, y = 2.66, yend = 2.66) + 
  # change to polar coordinates
  coord_polar(start = 0.7*pi, direction = 1)

ggsave(here::here("static/img/", "logo.png"), plot1, width = 12, height = 12, bg = 'transparent')
ggsave(here::here("content/blog/2022-04-01-of-scurvy-in-the-army/index", "thumb.png"), plot1, width = 12, height = 12, bg = 'black')

#ggsave("logo.png", plot1, width = 12, height = 12, bg = 'transparent')




plot2 <- scurvy1 %>% 
  filter(r > 0) %>%
  ggplot() + 
  geom_bar_pattern(
    data = scurvy1a,
    aes(tmp, r, color = NULL, pattern_filename = `Cause of Death`),
    stat = "identity", 
    position = "identity",  
    alpha = 0.3,
    pattern = "image",    # rather than color we use pattern from image
    pattern_type = "tile",
    width = 1,    # to remove space between bars
    size = 0.4
  ) + 
  geom_bar_pattern(
    data = scurvy1b,
    aes(tmp, r, color = NULL, pattern_filename = `Cause of Death`),
    stat = "identity", 
    position = "identity",  
    alpha = 0.3,
    pattern = "image",    # rather than color we use pattern from image
    pattern_type = "tile",
    width = 1,    # to remove space between bars
    size = 0.4
  ) + 
  geom_bar_pattern(
    data = scurvy1c,
    aes(tmp, r, color = NULL, pattern_filename = `Cause of Death`),
    stat = "identity", 
    position = "identity", 
    alpha = 0.3,
    pattern = "image",    # rather than color we use pattern from image
    pattern_type = "tile",
    width = 1,    # to remove space between bars
    size = 0.4
  ) +
  scale_pattern_filename_manual(
    values = c(
      "Zymotic Diseases" = "color_zymotic.jpg", 
      "Wounds and Injuries" = "color_death.jpg", 
      "All other Causes" = "color_other.jpg")
  )  +
  scale_color_manual(
    values = c(
      "Zymotic Diseases" = "#42514f", 
      "Wounds and Injuries" = "#ae7e79", 
      "All other Causes" = "black")
  ) +
  scale_y_continuous(limits = c(0, 14.25)) +
  # because ggplot's geom_text will not be curved with coord_polar
  geom_textpath(  
    mapping = aes(x = tmp, y = y, label = label), 
    data = labels, 
    color = "lightgrey",
    upright = FALSE,
    fontface = "bold",
    alpha = 0.4,
    # Turn the letters upside down - 
    # otherwise they'll be upside down once we change coord
    angle = 180
  ) +
  theme_void() +
  # remove all the clutter we don't need
  theme(
    legend.position = "none",
    panel.background = element_rect(fill='transparent', color='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color='transparent'), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank() #remove minor gridlines
  ) +
  # the one black line she describes in the legend
  annotate(geom = "segment", x = 7.5, xend = 8.5, y = 2.66, yend = 2.66) + 
  # change to polar coordinates
  coord_polar(start = 0.7*pi, direction = 1)

ggsave(here::here("static/img/", "logo2.png"), plot2, width = 12, height = 12, bg = 'transparent')



