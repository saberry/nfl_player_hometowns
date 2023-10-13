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

all_player_df$position_type <- ""

all_player_df$position_type[
  all_player_df$position %in% c("C", "G", "OT", "OL", "LS")
  ] <- "O line"

all_player_df$position_type[
  all_player_df$position %in% c("SAF", "CB", "FS", "DB", "S")
  ] <- "D backs"

all_player_df$position_type[
  all_player_df$position %in% c("DE", "DT", "NT", "DL")
] <- "D line"

all_player_df$position_type[
  all_player_df$position %in% c("ILB", "LB", "MLB", "OLB")
] <- "linebackers"

all_player_df$position_type[
  all_player_df$position %in% c("WR", "TE")
] <- "receivers"

all_player_df$position_type[
  all_player_df$position %in% c("QB", "RB", "FB")
] <- "O backs"

all_player_df$position_type[
  all_player_df$position %in% c("P", "K")
] <- "kickers"

position_cols <- colorFactor("Set3", all_player_df$position_type)

leaflet(all_player_df) %>% 
  setView(lng = -87, lat = 41, zoom = 4) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircles(lng = ~lon, lat = ~lat, 
             color = ~position_cols(position_type), 
             label = ~paste0(player, " ", position)) %>% 
  addLegend(pal = position_cols, values = ~position_type)
