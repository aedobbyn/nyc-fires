

burner_path <- "data/derived/burn.csv"

burner_plan <-
  drake_plan(
    seed_burn = get_tweets(
      user = burner_handle,
      input_path = burner_path # Reads in file from burn_path if file exists, otherwise pulls in seed tweets
    ),
    full_burn = target(
      command = get_tweets(
        tbl = seed_burn,
        user = burner_handle,
        output_path = burner_path
      ),
      trigger = trigger(
        condition = TRUE # Always look for new tweets
      )
    )
  )

burner_config <- drake_config(burner_plan)
make(burner_plan)
loadd(seed_burn)
loadd(full_burn)
expect_equal(seed_burn, full_burn)

outdated(burner_config)

# Tweet here
post_tweet(status = digest::digest(sample(100, 1)), token = firewire_token)

make(burner_plan)

loadd(seed_burn)
loadd(full_burn)
burn <- read_csv(here("data", "derived", "burn.csv"))
expect_gt(nrow(full_burn), nrow(seed_burn))
expect_equal(nrow(full_burn), nrow(burn))

clean()

make(burner_plan)
loadd(seed_burn)
loadd(full_burn)
expect_equal(nrow(seed_burn), nrow(full_burn))
