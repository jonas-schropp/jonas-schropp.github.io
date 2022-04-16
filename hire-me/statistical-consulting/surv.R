library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
library(gridExtra)

theme_custom = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "#e7e7e7", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "#e7e7e7", lineheight = 0.9),  
      axis.ticks = element_line(color = "#e7e7e7", size  =  0.2),  
      axis.title.x = element_text(face = "italic", size = base_size, color = "#e7e7e7", margin = margin(10, 0, 0, 0)),  
      axis.title.y = element_text(face = "italic", size = base_size, color = "#e7e7e7", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify panel options
      panel.background = element_rect(fill = "#232323", color  =  NA),  
      panel.border = element_rect(fill = NA, color = NA),  
      panel.grid.major = element_line(color = "grey20"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.spacing = unit(0.5, "lines"),   
      # Specify plot options
      plot.background = element_rect(color = "#232323", fill = "#232323"),  
      plot.title = element_text(face = "italic", size = base_size*1.2, color = "#e7e7e7", margin = margin(0, 0, 10, 0)),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

lung <- survival::lung %>%
  filter(
    ph.ecog %in% 0:2,
    time <= 805
    ) %>%
  mutate(group = factor(ph.ecog))

pl <- ggsurvplot(
  fit = survfit(Surv(time, status) ~ group, data = lung), 
  xlab = "Days", 
  ylab = "Overall survival probability",
  risk.table = TRUE,
  conf.int = TRUE
  ) 

pl$plot <- pl$plot +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
    labels = c("0%", "20%", "40%", "60%", "80%", "100%")
    ) +
  xlab("") +
  #scale_fill_manual(values = c("#EDFFC9", "#CC759A", "#B8F4FF")) +
  #scale_color_manual(values = c("#EDFFC9", "#CC759A", "#B8F4FF")) +
  #guides(color = "none", fill = "none") +
  theme_custom() +
  theme(
    legend.position = c(0.9,0.9),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#232323", color = "grey20"),
    legend.text = element_text(colour = "#e7e7e7"))

# solve an odd problem with the labeling of strata
tmp <- pl$table$data %>%
  mutate(
    strata = forcats::fct_recode(
      strata, 
      'group=0' = 'group=2',
      'group=1' = 'group=1',
      'group=2' = 'group=0'
      )
  )

pl$table <- pl$table$data %>%
  ggplot(aes(x = time, y = strata)) +
  theme_custom() +
  theme(
    panel.border = element_rect(fill = NA, color = "grey20"),
    panel.grid.major = element_line(color = NA),  
    panel.grid.minor = element_line(color = NA),
    text = element_text(colour = "pink")
    ) +
  geom_text(mapping = aes(label = n.risk), color = "#e7e7e7") +
  xlab("time (days)") +
  ggtitle("number at risk")

ggsave(filename = "surv.jpg", plot = print(pl), width = 10, height = 7, bg = "#232323", dpi = 300)

