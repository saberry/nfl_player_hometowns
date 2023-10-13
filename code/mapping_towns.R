###############
### Mapping ###
###############

library(leaflet)

load("./data/all_player_df.RData")
load("./data/town_geocodes.RData")

all_player_df$Hometown[all_player_df$Hometown == "Hunstville, AL"] <- "Huntsville, AL"

all_player_df <- merge(all_player_df, town_geocodes, 
                       by.x = "Hometown", by.y = "town", 
                       all.x = TRUE)

all_player_df <- all_player_df[!duplicated(all_player_df$player), ]

all_player_df[, c("lat", "lon")] <- lapply(c("lat", "lon"), function(x) {
  as.numeric(all_player_df[, x])
})

position_cols <- colorFactor("Set3", all_player_df$position)

leaflet(all_player_df) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircles(lng = ~lon, lat = ~lat, color = ~position_cols(position)) %>% 
  addLegend(pal = position_cols, values = ~position)
