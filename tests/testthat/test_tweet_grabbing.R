
sample_max_id <- 1085605027140517889
fire_handle <- "NYCFireWire"

seeded <- get_seed_fires(user = fire_handle,
                       n_tweets = 2,
                       max_id = sample_max_id)

reupped <- get_fires(seeded, n_tweets_reup = 3)

# Test that we pulled in the new tweet
expect_equal(
  nrow(reupped),
  5
)
