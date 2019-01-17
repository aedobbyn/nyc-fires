
suppressPackageStartupMessages({
  library(tidyverse)
  library(twitteR)
  library(ggmap)
  library(maps)
  library(drake)
  library(emojifont)
})

source(here::here("key.R"))
register_google(gmaps_key)
# https://developers.google.com/maps/documentation/geocoding/intro#Geocoding

boroughs <- c("Brooklyn", "Bronx", "Manhattan", "Staten", "Queens")
borough_reg <- boroughs %>%
  str_c(collapse = "|")


get_fires <- function(user = "NYCFireWire",
                      n_tweets = 50) {
  userTimeline(user, n = n_tweets) %>%
    twListToDF() %>%
    as_tibble() 
}


pull_addresses <- function(tbl) {
  tbl %>%
    mutate(
      borough = str_extract(text, "^[^\\s]*\\s") %>%
        str_remove("\\s"),
      street = str_extract(text, "(\\*[^\\.,]*)") %>% # All text after an asterisk and before a comma or period
        str_remove_all("(\\*.+\\*)") %>% # Get rid of stuff in between asterisks
        str_trim()
    ) %>%
    mutate(
      borough =
        case_when(
          str_detect(borough, borough_reg) ~ borough,
          TRUE ~ NA_character_
        ),
      address =
        glue::glue("{street}, {borough}", .na = "") %>%
          str_remove("[, ]?") %>%
          str_trim()
    ) %>%
    mutate(
      address = na_if(address, "")
    ) %>%
    select(borough, street, address, text, created)
}


geo_to_list <- function(inp) {
  geocode(inp) %>%
    rename(long = lon) %>%
    list()
}


truncate_lat_long <- function(tbl, digits = 3) {
  if (!"long" %in% names(tbl) ||
    !"lat" %in% names(tbl)) {
    stop("Input must contain columns lat and long.")
  }

  tbl %>%
    mutate(
      lat_trunc = round(lat, digits = digits),
      long_trunc = round(long, digits = digits)
    )
}


get_lat_long <- function(tbl) {
  tbl %>%
    rowwise() %>%
    mutate(
      l_l = ifelse(is.na(address), tibble(
        lat = NA_real_,
        long = NA_real_
      ) %>% list(),
      geo_to_list(address)
      )
    ) %>%
    unnest() %>%
    truncate_lat_long(digits = 1) %>%
    select(address, lat, long, lat_trunc, long_trunc, created, text)
}

get_city_data <- function(region = "new york") {
  ggplot2::map_data("state", region = region) %>%
    truncate_lat_long(digits = 1) %>%
    as_tibble()
}

join_on_city_data <- function(tbl, city = nyc) {
  tbl %>%
    rename(
      lat_tweet = lat,
      long_tweet = long
    ) %>%
    left_join(city, by = c("lat_trunc", "long_trunc"))
}


plot_fires <- function(tbl, city = nyc) {
  
  fire_emoji <- emoji("fire")
  
  tbl <- tbl %>% 
    mutate(label = fire_emoji)
  
  ggplot() +
    geom_polygon(data = nyc, aes(lat, long)) +
    geom_text(data = nyc, aes(lat, long, label = fire_emoji), family = "OpenSansEmoji", size = 6) +
    geom_point(data = dat, aes(lat, long), color = "red") +
    xlim(NA, 41) +
    ylim(-75, -73) +
    ggtitle("Fires were Started") +
    labs(x = "latitude", y = "longitude") +
    theme_light() 
}

