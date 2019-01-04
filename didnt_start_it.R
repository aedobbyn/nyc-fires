
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
    street = str_extract(text, "(\\*.+[,\\.])") %>% # All text after an asterisk and before a comma or period
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
  select(borough, street, address, text)

geo_to_list <- function(inp) {
  geocode(inp) %>% list()
}

# Get lat lon
lat_long <- 
  addresses %>% 
  rowwise() %>% 
  mutate(
    l_l = ifelse(is.na(address), tibble(lat = NA_character_, 
                                        lon = NA_character_) %>% list(), 
                 geo_to_list(address))
  ) %>% 
  unnest() %>% 
  select(address, lat, lon)





