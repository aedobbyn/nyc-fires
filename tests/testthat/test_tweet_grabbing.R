
test_that("get_fires successfully reups and adds new tweets", {
  
  sample_max_id <- sample_ids[1]
  
  seeded <- get_seed_fires(user = firewire_handle,
                           n_tweets = 2,
                           max_id = sample_max_id)
  
  reupped <- get_fires(seeded, n_tweets_reup = 3)
  
  expect_equal(
    nrow(reupped),
    5
  )
})

burner_acct <- "didntstartit"

get_fires(user = burner_acct)

