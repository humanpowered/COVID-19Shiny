library(maps)
library(mapproj)
us_states <- map_data("state")
national <- map_data('state')
county_full <- map_data("county")
# county_ca <- county_full[county_full$region == 'california',]

saveRDS(us_states, 'C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/state_maps.rds')
saveRDS(national, 'C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/nation_map.rds')
saveRDS(county_full, 'C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/county_maps.rds')


selection.fill <- c(
  "1" = "#69b3a2",
  "0" = "Gray95")

granularity <- 'County'
state <- 'California'
county <- 'Alameda'


national$selection <- "1"

us_states$selection <- ifelse(tolower(State) == us_states$region, "1", "0")

county_ca$selection <- ifelse(tolower(County) == county_ca$subregion, "1", "0")


reg <- map_file[map_file$granularity == granularity,]

reg$selection <- ifelse(reg$granularity == 'Nation', "1",
                        ifelse(reg$granularity == 'State' & reg$state == tolower(state), "1",
                               ifelse(reg$granularity == 'County' & reg$county == tolower(county), "1", "0"
                               )
                        )
)

################## National Map -------------------------
ggplot(data = reg,
       aes(
         x = long,
         y = lat,
         group = group,
         fill = selection
       )) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers",
            lat0 = 39,
            lat1 = 45) +
  theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none',
    plot.margin = margin(0, 0, 0, 0)
  ) +
  scale_fill_manual(values = selection.fill)


################## State Map ------------------------------
sp <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = selection))

sp + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(axis.title = element_blank(), axis.line = element_blank(), panel.background = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = 'none', plot.margin = margin(0, 0, 0, 0)) +
  scale_fill_manual(values = selection.fill)

################### County Map ---------------------------
cp <- ggplot(
  data = county_ca,
  aes(
    x = long,
    y = lat,
    group = group,
    fill = selection
  )) +
    
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(
      projection = "albers",
      lat0 = 39,
      lat1 = 45
    ) +
    theme(
      axis.title = element_blank(),
      axis.line = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'none',
      plot.margin = margin(0, 0, 0, 0)
    ) +
    scale_fill_manual(values = selection.fill)


