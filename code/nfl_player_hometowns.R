############################
### NFL Player Hometowns ###
############################

library(rvest)
library(stringr)

# First we need to get links for all of
# the teams. 

teams_link <- "https://www.nfl.com/teams/"

team_info_links <- read_html(teams_link) %>%
  html_elements("a[href*='teams']") %>% 
  html_attr('href') %>% 
  grep("https", ., value = TRUE)

team_info_links <- team_info_links[!duplicated(team_info_links)]

team_info_links <- gsub("/$", "", team_info_links)

team_info_links <- grep("-", team_info_links, value = TRUE)

team_roster_links <- paste0(team_info_links, "/roster")

# Now we can links for each player on every roster.

all_player_links <- sapply(team_roster_links, function(x) {
  Sys.sleep(runif(1, 0, 1))
  read_html(x) %>%
    html_elements("a[href*='players/']") %>%
    html_attr('href') %>%
    paste0("https://www.nfl.com", .) %>%
    grep("players/.*/", ., value = TRUE)
})

all_player_links <- unlist(all_player_links)

# Going to make a function to pass into 
# a future statement.

player_info_getter <- function(player, team) {
  
  Sys.sleep(runif(1, .1, 1))

  tryCatch({
    initial_read <- read_html(player) 
    
    player_info <- html_elements(initial_read, ".nfl-c-player-info__key") |> 
      html_text()
    
    player_value <- html_elements(initial_read, ".nfl-c-player-info__value") |> 
      html_text()
    
    player_position <- html_elements(initial_read, ".nfl-c-player-header__position") |> 
      html_text()
    
    data.frame(player_info, 
               player_value, 
               player_position,
               player_link = player,
               team_link = team)
  }, 
  error = function(e) {
    data.frame(player_info = NA, 
               player_value = NA, 
               player_position = NA,
               player_link = player,
               team_link = team)
  })
  
}

# Since there are over 2500 player links, 
# I want to run this in parallel.

library(future)
library(furrr)

plan(multisession, workers = parallel::detectCores() - 1)

player_info_output <- future_map2_dfr(.x = all_player_links, 
                                      .y = names(all_player_links), 
                                      ~player_info_getter(.x, .y))

# If you don't want to use parallelization 
# to do this, you could run the code below. 
# You'll just need to be findful of the variable
# names that come after.

# player_info_output <- mapply(player_info_getter, 
#                              all_player_links, 
#                              names(all_player_links))
# 
# player_info_output <- lapply(1:ncol(player_info_output), function(x) {
#   data.frame(
#     info = player_info_output[, x]$player_info,
#     value = player_info_output[, x]$player_value,
#     player = player_info_output[, x]$player_link,
#     team = player_info_output[, x]$team_link
#   )
# })
# 
# player_info_output <- do.call(rbind, player_info_output)

# No matter what you choose, we need to clean
# up the data a tiny bit:



player_info_output$player_value <- gsub(
  "\n|\\s{2,}", 
  "", 
  player_info_output$player_value
)

player_info_output$player_position <- gsub(
  "\n|\\s{2,}", 
  "", 
  player_info_output$player_position
)

player_info_output$player_link <- str_extract(
  player_info_output$player_link, 
  "(?<=players/).*(?=/)"
)

player_info_output$team_link <- str_extract(
  player_info_output$team_link, 
  "(?<=teams/).*(?=/roster)"
)

colnames(player_info_output) <- c("info", "value", "position", 
                                  "player", "team")

# The data is in long format when it comes in
# from the page, so let's pivot it back out:

all_player_df <- tidyr::pivot_wider(
  player_info_output, id_cols = c("player", "team"), 
  names_from = info, 
  values_from = value
)

all_player_df$feet <- (as.numeric(str_extract(all_player_df$Height, "[56](?=-)")) * 12)
all_player_df$inches <- as.numeric(str_extract(all_player_df$Height, "(?<=-)[56]"))
all_player_df$inches[is.na(all_player_df$inches)] <- 0

all_player_df$Height_inches <- all_player_df$feet + all_player_df$inches

fraction_conversion <- function(frac) {
  # browser()
  decimal <- sapply(str_extract_all(frac, "(?<= ).*"), function(x) eval(parse(text = x)))
  
  decimal <- sapply(decimal, function(x) {
    ifelse(is.null(x), 0, x)
  })
  
  inch_val <- str_extract_all(frac, "^[0-9]{1,2}")
  
  inch_val <- sapply(inch_val, function(x) {
    ifelse(is.null(x), 0, x)
  })
  
  result <- as.numeric(inch_val) + decimal
  return(result)
}

all_player_df$Hands <- fraction_conversion(all_player_df$Hands)

all_player_df$Arms <- fraction_conversion(all_player_df$Arms)

to_numeric_vars <- c("Weight", "Experience", "Age")

all_player_df <- as.data.frame(all_player_df)

all_player_df[, to_numeric_vars] <- lapply(to_numeric_vars, function(x) {
  as.numeric(all_player_df[, x])
  }
)

save(all_player_df, file = "data/all_player_df.RData")
