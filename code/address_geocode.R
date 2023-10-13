#######################
### Geocoding Towns ###
#######################

library(httr)
library(jsonlite)

load("./data/all_player_df.RData")

towns <- unique(all_player_df$Hometown)

towns <- towns[towns != ""]

town_df <- data.frame(towns)

town_df$clean_town <- gsub(" ", "%20", town_df$towns)

base_links <- glue::glue("https://geocode.maps.co/search?q={town_df$clean_town}")

geo_getter <- function(link, town) {
  
  Sys.sleep(runif(1, .5, 1))
  
  geo_request <- GET(link)
  
  geo_result <- fromJSON(content(geo_request, as = "text"))
  
  output <- data.frame(
    lat = geo_result$lat[1], 
    lon = geo_result$lon[1]
  )
  
  output$bbox <- list(geo_result$boundingbox[[1]])
  
  output$town <- town
  
  return(output)
}

town_geocodes <- mapply(
  geo_getter, 
  base_links, 
  town_df$towns, 
  SIMPLIFY = FALSE, 
  USE.NAMES = FALSE
)
