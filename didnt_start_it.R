
suppressPackageStartupMessages({
  library(tidyverse)
  library(twitteR)
  library(ggmap)
  library(maps)
  library(drake)
  library(emojifont)
  library(here)
  library(glue)
})

source(here("key.R"))
register_google(gmaps_key)
# https://developers.google.com/maps/documentation/geocoding/intro#Geocoding

boroughs <- c("Brooklyn", "Bronx", "Manhattan", "Staten", "Queens")
borough_reg <- boroughs %>%
  str_c(collapse = "|")


get_seed_fires <- function(user = "NYCFireWire",
                           n_tweets = 50, 
                           max_id = NULL) {
  
  userTimeline(user, n = n_tweets, maxID = max_id) %>%
    twListToDF() %>%
    as_tibble() %>%
    mutate(
      created_at = # UTC by default
      lubridate::as_datetime(created, tz = "America/New_York")
    )
}


get_more_fires <- function(tbl,
                           n_tweets = 20,
                           verbose = TRUE,
                           ...) {
  latest_dt <-
    tbl %>%
    arrange(desc(created_at)) %>%
    slice(1) %>%
    pull(created_at)

  if (verbose) message("Searching for new tweets.")

  new <- get_seed_fires(n_tweets = n_tweets)

  if (max(new$created_at) <= latest_dt) {
    if (verbose) message("No new tweets to pull.")
    return(NULL)
  }

  out <-
    new %>%
    filter(created_at > latest_dt)

  if (verbose) message(glue("{nrow(out)} new tweets pulled."))

  out
}


get_fires <- function(tbl = NULL,
                      user = "NYCFireWire",
                      n_tweets_seed = 50,
                      n_tweets_reup = 20,
                      verbose = TRUE, ...) {
  if (is.null(tbl)) {
    out <- get_seed_fires(n_tweets = n_tweets_seed)
  } else {
    new <- 
      get_more_fires(tbl, n_tweets = n_tweets_reup, verbose = verbose)
    
    out <- 
      tbl %>% 
      bind_rows(new)
  }
  
  out
}

try_get_fires <- possibly(get_fires, otherwise = NULL,
                          quiet = FALSE)


clean_borough <- function(x) {
  if (is.na(x) || !str_detect(x, borough_reg)) {
    return(NA_character_)
  }

  # Return the borough match
  b <- boroughs[which(str_detect(x, boroughs))][1]

  if (b == "Bronx") {
    b <- "The Bronx"
  } else if (b == "Staten") {
    b <- "Staten Island"
  }

  b
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
    rowwise() %>%
    mutate(
      borough =
        case_when(
          str_detect(borough, borough_reg) ~ borough %>% clean_borough(),
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
    select(borough, street, address, text, created_at)
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
    select(address, lat, long, lat_trunc, long_trunc, created_at, text)
}


join_on_city_data <- function(tbl, city = nyc) {
  tbl %>%
    rename(
      lat_tweet = lat,
      long_tweet = long
    ) %>%
    left_join(city, by = c("lat_trunc", "long_trunc"))
}


count_fires <- function(tbl) {
  tbl %>%
    drop_na() %>%
    group_by(lat, long) %>%
    count()
}


graph_fire_times <- function(tbl) {
  ggplot(tbl, aes(created_at)) +
    geom_density() +
    ggtitle("Frequency of Fires in NYC") +
    labs(x = "Time of Tweet", y = "Density") +
    theme_light()
}


nyc <- 
  ggplot2::map_data("state", region = region) %>%
  truncate_lat_long(digits = 1) %>%
  as_tibble()


plot_fires <- function(tbl, city = nyc) {
  fire_emoji <- emoji("fire")

  ggplot() +
    geom_polygon(data = nyc, aes(lat, long)) +
    geom_text(
      data = dat, aes(lat, long, label = fire_emoji),
      family = "EmojiOne", size = 3, color = "red"
    ) +
    # geom_point(data = dat, aes(lat, long), color = "red") +
    xlim(NA, 41) +
    ylim(-75, -73) +
    ggtitle("Fires were Started") +
    labs(x = "latitude", y = "longitude") +
    theme_light()

  ggsave(here("data", "derived", "fire_plot.png"),
    device = "png"
  )
}
