
test_that("get_fires successfully reups and adds new tweets", {
  
  sample_max_id <- 1085605027140517889
  
  seeded <- get_seed_fires(user = firewire_handle,
                           n_tweets = 2,
                           max_id = sample_max_id)
  
  reupped <- get_fires(seeded, n_tweets_reup = 3)
  
  expect_equal(
    nrow(reupped),
    5
  )
})

