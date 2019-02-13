suppressPackageStartupMessages({
  library(drake)
  library(emojifont)
  library(fs)
  library(ggmap)
  library(glue)
  library(here)
  library(maps)
  library(rtweet)
  library(testthat)
  library(tidyselect)
  library(tidyverse)
})

# Read in gitignored GMaps and Twitter keys
source(here("R", "key.R"))

# Set up Google Maps key
register_google(gmaps_key)

# And Twitter burner account token
firewire_token <- create_token(
  app = firewire_app_name,
  consumer_key = firewire_consumer_key,
  consumer_secret = firewire_consumer_secret,
  access_token = firewire_access_token,
  access_secret = firewire_access_secret
)

firewire_handle <- "NYCFireWire"
burner_handle <- "didntstartit"

# Random old NYCFireWire tweet ID so we can test pulling in new ones
# (By default, the most recent tweets are pulled in first)
old_tweet_id <- "1084948074588487680"


# ------------------------ Getting tweets ------------------------

# Get a batch of tweets, either from file or from Twitter
get_seed_tweets <- function(user = firewire_handle,
                            n_tweets = 50,
                            max_id = NULL, # Max ID of the tweet
                            input_path = NULL, # Read from a file or grab from Twitter?
                            output_path = NULL,
                            write_out = FALSE,
                            verbose = TRUE) {
  if (!is.null(input_path) && file_exists(input_path)) {
    if (verbose) message("Reading in tweets from CSV.")
    out <-
      read_csv(input_path,
        col_types =
          list(
            text = col_character(),
            user_id = col_character(),
            status_id = col_character(),
            created_at = col_datetime(format = ""),
            screen_name = col_character()
          )
      )
  } else {
    out <-
      get_timeline(user = user, n = n_tweets, max_id = max_id) %>%
      mutate(
        user_id = as.character(user_id),
        status_id = as.character(status_id),
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


# Check if there are new tweets at an account
there_are_new_tweets <- function(tbl,
                                 user = firewire_handle,
                                 verbose = TRUE) {
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


# Given a tbl of tweets, reup if there_are_new_tweets()
get_more_tweets <- function(tbl,
                            user = firewire_handle,
                            n_tweets = 20,
                            verbose = TRUE) {
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


# Run get_seed_tweets() if tbl is null, otherwise reup with get_more_tweets()
# and write the result out to file if write_out is true
get_tweets <- function(tbl = NULL,
                       user = firewire_handle,
                       max_id = NULL,
                       n_tweets_seed = 10,
                       n_tweets_reup = 5,
                       input_path = NULL,
                       output_path = NULL,
                       write_out = TRUE,
                       verbose = TRUE) {
  if (is.null(tbl) || is.na(tbl)) {
    out <- get_seed_tweets(
      user = user,
      n_tweets = n_tweets_seed,
      input_path = input_path,
      output_path = output_path,
      write_out = FALSE,
      max_id = max_id
    )
  } else {
    new <-
      get_more_tweets(tbl, user = user, n_tweets = n_tweets_reup, verbose = verbose)

    out <-
      tbl %>%
      bind_rows(new) %>%
      arrange(desc(created_at))
  }

  # Always write to file
  if (!is.null(output_path) && write_out == TRUE) {
    write_csv(out, output_path)
  }

  out
}


# ------------------------ Extracting stuff from tweets ------------------------

# Bronx will become The Bronx and Staten will become Staten Island in clean_borough()
boroughs <- c("Brooklyn", "Bronx", "Manhattan", "Staten", "Queens")
borough_reg <- boroughs %>%
  str_c(collapse = "|")


# Helper used inside pull_addresses()
# If a tweet has a borough anywhere in it, pull it out
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


# From the text of a tweet, pull out the borough, the street, and stick them
# together to make the address
pull_addresses <- function(tbl) {
  tbl %>%
    mutate(
      borough = str_extract(text, "^[^\\s]*\\s") %>%
        str_remove("\\s"),
      # All text after an asterisk and before a comma or period
      street = str_extract(text, "(\\*[^\\.,]*)") %>%
        # Get rid of stuff in between asterisks
        str_remove_all("(\\*.+\\*)") %>%
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

# Given an address extracted from a tweet, if it's not NA send it to Google
# to grab its assocated lat and long. Add truncated versions for good measure.
get_lat_long <- function(tbl) {
  tbl %>%
    rowwise() %>%
    mutate(
      l_l = ifelse(is.na(address),
        tibble(
          lat = NA_real_,
          long = NA_real_
        ) %>% list(),
        geo_to_list(address)
      )
    ) %>%
    unnest() %>%
    select(address, lat, long, created_at, text)
}


# ------------------------ Analyzing tweet content ------------------------

# Count number of fires at each lat/long combo
count_fires <- function(tbl) {
  tbl %>%
    drop_na() %>%
    count(lat, long)
}


# Graph when fires happen
graph_fire_times <- function(tbl) {
  ggplot(tbl, aes(created_at)) +
    geom_density() +
    ggtitle("Timing of Fires in NYC") +
    labs(x = "Datetime of Tweet", y = "Density") +
    theme_light()
}


fire_emoji <- emoji("fire")

nyc_map <- get_map("new york city")

# Plot where fires occurred by lat/long combo
plot_fire_sums <- function(tbl, city = nyc_map,
                           use_emoji = FALSE,
                           output_path = here("plots", "fire_sums_plot.png")) {
  
  tbl <-
    tbl %>%
    drop_na(lat, long)
  
  if (use_emoji) {
    fire_layer <- 
      geom_text(
        data = tbl, aes(long, lat, label = fire_emoji, size = n),
        family = "EmojiOne", color = "red"
      ) 
  } else {
    fire_layer <- 
      geom_point(
        data = tbl, aes(long, lat, size = n),
        color = "red", alpha = 0.5
      ) 
  }

  plt <- ggmap(nyc_map) +
    fire_layer +
    ggtitle("Fires were Started") +
    labs(x = "longitude", y = "latitude") +
    theme_light()

  if (!is.null(output_path)) {
    ggsave(output_path,
      device = "png"
    )
  } else {
    plt
  }
}
