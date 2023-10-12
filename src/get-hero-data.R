library(tidyverse)
library(httr)

get_hero_data <- function() {
  message("Retrieving hero data")
  
  dir_path <- paste0("data")
  file_path <- paste0(dir_path, "/heroes.csv")
  
  if (file.exists(file_path)) {
    # Read data from disk
    heroes <- read_csv(file_path, progress = FALSE, show_col_types = FALSE)
  } else {
    # Get hero data
    response <- GET(url = "https://api.opendota.com/api/constants/heroes")
    if (http_status(response)$category != "Success") {
      stop("Unsuccessful request")
    }
    
    # Store player data
    heroes <- data.frame(
      hero_id = as.numeric(), 
      hero_name = as.character(),
      balanced = as.logical(),
      brawny = as.logical(),
      dashing = as.logical(),
      canny = as.logical()
    )
    for (hero in content(response)) {
      heroes <- heroes %>%
        add_row(
          hero_id = as.numeric(hero$id), 
          hero_name = as.character(hero$name),
          balanced = as.logical(hero$primary_attr == "all"),
          brawny = as.logical(hero$primary_attr == "str"),
          dashing = as.logical(hero$primary_attr == "agi"),
          canny = as.logical(hero$primary_attr == "int")
        )
    }
    
    # Write the data to disk
    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
    }
    heroes <- heroes %>% arrange(hero_id)
    write_csv(x = heroes, file = file_path)
  }
  
  return(heroes)
}
