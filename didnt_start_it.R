
library(tidyverse)
library(twitteR)
library(ggmap)

source(here::here("key.R"))
register_google(gmaps_key)

boroughs <- c("Brooklyn", "Bronx", "Manhattan", "Staten", "Queens")
borough_reg <- boroughs %>% 
  str_c(collapse = "|")

fires_raw <- userTimeline("NYCFireWire")

fires <- 
  fires_raw %>% 
  twListToDF() %>% 
  as_tibble()

addresses <-
  fires %>% 
  mutate(
    borough = str_extract(text, "^[^\\s]*\\s") %>% 
      str_remove("\\s"),
    street = str_extract(text, "(\\*[^\\.,]*)") %>% # All text after an asterisk and before a comma or period
      str_remove_all("(\\*.+\\*)") %>%  # Get rid of stuff in between asterisks
      str_trim()
  ) %>%
  mutate(
    borough = 
      case_when(str_detect(borough, borough_reg) ~ borough,
                TRUE ~ NA_character_),
    address = 
      glue::glue("{street}, {borough}", .na = "") %>% 
      str_remove("[, ]?") %>%  
      str_trim()
  ) %>% 
  mutate(
    address = na_if(address, "")
  ) %>% 
  select(borough, street, address, text, created)

# https://developers.google.com/maps/documentation/geocoding/intro#Geocoding
geo_to_list <- function(inp) {
  geocode(inp) %>% list()
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


# Get lat lon
lat_long <- 
  addresses %>% 
  rowwise() %>% 
  mutate(
    l_l = ifelse(is.na(address), tibble(lat = NA_character_, 
                                        long = NA_character_) %>% list(), 
                 geo_to_list(address))
  ) %>% 
  unnest() %>% 
  truncate_lat_long(digits = 1) %>% 
  select(address, lat, long, lat_trunc, long_trunc, created, text)


nyc <- 
  ggplot2::map_data("state", region = "new york") %>%
  truncate_lat_long(digits = 1) %>% 
  as_tibble() 


dat <- 
  lat_long %>% 
  rename(
    lat_tweet = lat,
    long_tweet = long
  ) %>% 
  left_join(nyc, by = c("lat_trunc", "long_trunc"))

ggplot() +
  geom_polygon(data = nyc, aes(lat, long)) +
  geom_point(data = dat, aes(lat_tweet, long_tweet), color = "red") +
  xlim(NA, 41) +
  ylim(-75, -73)








library(nycgeo) # https://github.com/mfherman/nycgeo
library(sf)


lat_long %>% 
  drop_na(lat, long) %>% 
  st_as_sf(coords = c("long", "lat"), 
           crs = 4326, agr = "constant") %>% 
  st_join(nyc_boundaries(geography = "block"))

nyc_boundaries(geography = "block", add_acs_data = TRUE)$geometry[1]



