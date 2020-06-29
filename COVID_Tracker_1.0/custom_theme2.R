theme_Custom <- function(base_size=12, base_family="Helvetica") {
  library(grid)
  library(ggthemes)
  library(extrafont)
  library(extrafontdb)
  (theme_foundation(base_size=base_size, base_family="Lato") + 
      theme(plot.title = element_text(family = 'Lato',
                                      face = "bold",
                                      size = rel(1.5),
                                      hjust = 0.5),
            plot.subtitle = element_text(size = rel(0.8), hjust = 0.5),
            text = element_text(family = 'Lato', size = 12),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(size = rel(0.8)),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_text(colour="#6B6B6B", size = rel(0.8)), 
            axis.line = element_line(colour="#6B6B6B", size = 0.3),
            axis.ticks = element_line(colour="#6B6B6B", size = 0.3),
            panel.grid.major.y = element_line(colour="#f0f0f0"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size = unit(0.5, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_blank(),
            legend.text = element_text(size = rel(0.7)),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0")
    ))
}

  