###########
### PPA ###
###########

library(purrr)
library(sf)
library(spatstat)


load("./data/all_player_df.RData")

all_player_df_dropped <- all_player_df[!is.na(all_player_df$lat) & 
                                         !is.na(all_player_df$lon), ]

all_player_df_dropped <- all_player_df_dropped[all_player_df_dropped$lon > -125 & 
                                                 all_player_df_dropped$lon < 20 & 
                                                 all_player_df_dropped$lat > -30 & 
                                                 all_player_df_dropped$lat < 53.5, ]

player_sf <- st_as_sf(all_player_df_dropped, 
                      agr = "identity", 
                      coords = c("lat", "lon"))

ks_results <- map(unique(player_sf$position), ~{
  player_ppp <- as.ppp(player_sf[player_sf$position == .x, ])
  
  # plot(player_ppp)
  # 
  # quadrat.test(player_ppp, 10, 10)
  
  ks <- cdf.test(player_ppp, "y")
  
  data.frame(p_value = ks$p.value, 
             position = .x)
  
})

ks_results <- list_rbind(ks_results)

test_positions <- ks_results[ks_results$p_value > .05, ]


map(test_positions$position, ~{
  
  player_ppp <- as.ppp(player_sf[player_sf$position == .x, ])
  
  # plot(player_ppp)
  # 
  # quadrat.test(player_ppp, 10, 10)
  
  g_test <- Gest(player_ppp)
  
  data.frame(p_value = ks$p.value, 
             position = .x)
})