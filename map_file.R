state_maps <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID_Tracker_1.0/state_maps.rds')
nation_map <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID_Tracker_1.0/nation_map.rds')
ca_counties_map <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID_Tracker_1.0/ca_counties_maps.rds')

nation_map$granularity <- 'Nation'
nation_map$region <- 'N/A'
nation_map$subregion[is.na(nation_map$subregion)] <- 'N/A'
state_maps$granularity <- 'State'
state_maps$subregion[is.na(state_maps$subregion)] <- 'N/A'
ca_counties_map$granularity <- 'County'
ca_counties_map$region <- 'N/A'

map_file <- bind_rows(nation_map, state_maps, ca_counties_map)

names(map_file) <- c('long', 'lat', 'group', 'order', 'state', 'county', 'granularity')

map_file$granularity <- gsub('County', "County (California Only)", map_file$granularity, fixed = T)

saveRDS(map_file, 'C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID_Tracker_1.0/combined_map_files.rds')
