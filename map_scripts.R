library(maps)
library(mapproj)
us_states <- map_data("state")



selection.fill <- c(
  "1" = "#69b3a2",
  "0" = "Gray95")

Granularity <- 'State'
State <- 'Utah'
County <- 'N/A'
Metric <- 'New Cases'

us_states$selection <- as.factor(ifelse(us_states$region == tolower(State), 1, 0))

p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = selection))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(axis.title = element_blank(), axis.line = element_blank(), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = 'none', plot.margin = margin(0, 0, 0, 0)) +
  scale_fill_manual(values = selection.fill)
