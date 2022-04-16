library(ggseg)
library(ggplot2)

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

pl <- ggseg(atlas = dk, 
      mapping = aes(fill = region),
      show.legend = FALSE, 
      color = "black",
      position = "stacked") +
  theme_custom() 


ggsave("brain.jpg", pl, width = 8, height = 5)
