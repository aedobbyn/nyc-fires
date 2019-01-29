
# Not done during live coding for time reasons (mainly the round trip to/from Google)

source(here::here("R", "didnt_start_it.R"))

lots_o_fires <-
  get_seed_tweets(
    n_tweets = 3000,
    output_path = here("data", "raw", "lots_o_fires.csv")
  )
