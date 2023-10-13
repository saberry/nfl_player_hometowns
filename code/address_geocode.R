#######################
### Geocoding Towns ###
#######################

library(httr)
library(jsonlite)

load("./data/all_player_df.RData")

towns <- unique(all_player_df$Hometown)

towns <- towns[towns != ""]

towns[towns == "Hunstville, AL"] <- "Huntsville, AL"

town_df <- data.frame(towns)

town_df$clean_town <- gsub(" ", "%20", town_df$towns)

base_links <- glue::glue("https://geocode.maps.co/search?q={town_df$clean_town}")

geo_getter <- function(link, town) {
  
  Sys.sleep(runif(1, .6, 1))
  
  geo_request <- GET(
    link, 
    add_headers(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/117.0", 
      "Host" = "geocode.maps.co"
    )
  )
  
  if(geo_request$status_code == 429) {
    print("waiting on 429")
    Sys.sleep(1)
    geo_request <- GET(
      link, 
      add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/117.0", 
        "Host" = "geocode.maps.co"
      )
    ) 
  } else if(geo_request$status_code == 503) {
    print("waiting on 503")
    print(geo_request$headers)
    Sys.sleep(20)
    geo_request <- GET(
      link, 
      add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/117.0", 
        "Host" = "geocode.maps.co"
      )
    ) 
  }
    
  geo_result <- fromJSON(content(geo_request, as = "text"))
  
  if(length(geo_result) != 0 & geo_request$status_code == 200) {
    
    output <- data.frame(
      lat = geo_result$lat[1], 
      lon = geo_result$lon[1]
    )
    
    if(length(geo_result$boundingbox) != 0) {
      output$bbox <- list(geo_result$boundingbox[[1]])
    } else output$bbox <- NA
    
    output$town <- town
    
    return(output)
  } else {
    data.frame(lat = NA, 
               lon = NA, 
               bbox = NA, 
               town = town 
               )
  }
}

town_geocodes <- mapply(
  geo_getter, 
  base_links, 
  town_df$towns, 
  SIMPLIFY = FALSE, 
  USE.NAMES = FALSE
)

town_geocodes <- do.call(rbind, town_geocodes)

save(town_geocodes, file = "./data/town_geocodes.RData")
