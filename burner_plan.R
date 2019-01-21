

burner_path <- "data/derived/burn.csv"

burner_plan <- 
  drake_plan(
    seed_burn = get_tweets(user = burner_acct, 
                           input_path = burner_path),  # Reads in file from burn_path if it exists
    full_burn = get_tweets(tbl = seed_burn, 
                           user = burner_acct,
                           output_path = burner_path),
    
    strings_in_dots = "literals"
  )

burner_config <- drake_config(burner_plan)
make(burner_plan)
loadd(seed_burn)
loadd(full_burn)
expect_equal(seed_burn, full_burn)

outdated(burner_config)

# Tweet here

make(burner_plan)

loadd(seed_burn)
loadd(full_burn)
expect_gt(nrow(full_burn), nrow(seed_burn))


