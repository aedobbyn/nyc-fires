
suppressPackageStartupMessages({
  library(drake)
  library(emojifont)
  library(ggmap)
  library(glue)
  library(here)
  library(maps)
  library(rtweet)
  library(testthat)
  library(tidyverse)
})

pkgconfig::set_config("drake::strings_in_dots" = "literals")

source(here("key.R"))
register_google(gmaps_key)
# https://developers.google.com/maps/documentation/geocoding/intro#Geocoding

firewire_token <- create_token(
  app = firewire_app_name,
  consumer_key = firewire_consumer_key,
  consumer_secret = firewire_consumer_secret,
  access_token = firewire_access_token,
  access_secret = firewire_access_secret
)

firewire_handle <- "NYCFireWire"
burner_handle <- "didntstartit"

boroughs <- c("Brooklyn", "Bronx", "Manhattan", "Staten", "Queens")
borough_reg <- boroughs %>%
  str_c(collapse = "|")

old_tweet_id <- "1084619203167031297"

get_seed_tweets <- function(user = firewire_handle,
                            n_tweets = 50,
                            max_id = NULL, # Max ID of the tweet
                            input_path = NULL, # Read from a file or grab from Twitter?
                            output_path = NULL,
                            write_out = FALSE,
                            ...) {
  if (!is.null(input_path) && file.exists(input_path)) {
    out <-
      read_csv(input_path)
  } else {
    out <- get_timeline(user = user, n = n_tweets, max_id = max_id) %>%
      mutate(
        user_id = as.numeric(user_id),
        status_id = as.numeric(status_id),
        created_at = # UTC by default
        lubridate::as_datetime(created_at, tz = "America/New_York")
      ) %>%
      select(text, user_id, status_id, created_at, screen_name) %>%
      arrange(desc(created_at))
  }

  if (!is.null(output_path) && write_out == TRUE) {
    write_csv(out, output_path)
  }

  out
}


there_are_new_tweets <- function(tbl,
                                 user = firewire_handle,
                                 verbose = TRUE, 
                                 ...) {
  latest_dt <- 
    tbl %>%
    arrange(desc(created_at)) %>%
    slice(1) %>%
    pull(created_at)

  if (verbose) message("Searching for new tweets.")

  new <- get_seed_tweets(user = user, n_tweets = 1)

  if (max(new$created_at) <= latest_dt) {
    if (verbose) message("No new tweets to pull.")
    FALSE
  } else {
    TRUE
  }
}

get_latest_dt <- function(user = firewire_handle) {
  
  get_seed_tweets(user) %>%
    arrange(desc(created_at)) %>%
    slice(1) %>%
    pull(created_at)
}


get_more_tweets <- function(tbl,
                            user = firewire_handle,
                            n_tweets = 20,
                            verbose = TRUE,
                            ...) {
  if (!there_are_new_tweets(tbl = tbl, user = user)) {
    return(NULL)
  }

  new <- get_seed_tweets(user = user, n_tweets = n_tweets)

  out <-
    new %>%
    filter(created_at > max(tbl$created_at))

  if (verbose) message(glue("{nrow(out)} new tweet(s) pulled."))

  out
}


get_tweets <- function(tbl = NULL,
                       user = firewire_handle,
                       max_id = NULL,
                       n_tweets_seed = 50,
                       n_tweets_reup = 20,
                       input_path = NULL,
                       output_path = NULL,
                       write_out = FALSE,
                       verbose = TRUE, ...) {
  
  if (is.null(tbl) || is.na(tbl)) {
    out <- get_seed_tweets(user = user, 
                           n_tweets = n_tweets_seed, 
                           input_path = input_path, 
                           output_path = output_path,
                           write_out = write_out,
                           max_id = max_id)
  } else {
    new <-
      get_more_tweets(tbl, user = user, n_tweets = n_tweets_reup, verbose = verbose)

    out <-
      tbl %>%
      bind_rows(new) %>%
      arrange(desc(created_at))
  }

  if (!is.null(output_path)) {
    write_csv(out, output_path)
  }

  out
}

try_get_fires <- possibly(get_tweets,
  otherwise = NULL,
  quiet = FALSE
)


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
  ggplot2::map_data("state", region = "new york") %>%
  truncate_lat_long(digits = 1) %>%
  as_tibble()

fire_emoji <- emoji("fire")

plot_fires <- function(tbl, city = nyc,
                       output_path = here("data", "derived", "fire_plot.png")) {
  
  tbl <- 
    tbl %>% 
    drop_na(lat, long)

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

  if (!is.null(output_path)) {
    ggsave(output_path,
           device = "png"
    )
  }
}


plot_fire_sums <- function(tbl, city = nyc, 
                           output_path = here("data", "derived", "fire_sums_plot.png")) {
  
  tbl <- 
    tbl %>% 
    drop_na(lat, long)
  
  ggplot() +
    geom_polygon(data = nyc, aes(lat, long)) +
    geom_text(
      data = tbl, aes(lat, long, label = fire_emoji, size = n),
      family = "EmojiOne", color = "red", fill = "orange"
    ) +
    xlim(NA, 41) +
    ylim(-75, -73) +
    ggtitle("Fires were Started") +
    labs(x = "latitude", y = "longitude") +
    theme_light()
  
  if (!is.null(output_path)) {
    ggsave(output_path,
           device = "png"
    )
  }
}
