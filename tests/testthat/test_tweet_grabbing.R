# From oldest to newest
sample_ids <- c(
  "1084034650157264896",
  "1084619203167031297",
  "1085331631299276800",
  "1085603835534630913"
)

test_that("get_fires successfully reups and adds new tweets", {
  sample_max_id <- sample_ids[1]

  seeded <- get_seed_tweets(
    user = firewire_handle,
    n_tweets = 2,
    max_id = sample_max_id
  )

  reupped <- get_tweets(seeded, n_tweets_reup = 3)

  expect_equal(
    nrow(reupped),
    5
  )
})

burner_acct <- "didntstartit"

get_tweets(user = burner_acct)
