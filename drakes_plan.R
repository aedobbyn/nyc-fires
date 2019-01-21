
source(here::here("didnt_start_it.R"))

plan <-
  drake_plan(
    seed_fires = get_tweets(n_tweets_seed = 2, 
                      max_id = sample_max_id), # Grab some seed fires
    fires = get_tweets(seed_fires, n_tweets_reup = 3), # Reups if there are more
    addresses = pull_addresses(fires), # Extract addresses from tweets
    lat_long = get_lat_long(addresses), # Send to Google for lat-longs
    dat = join_on_city_data(lat_long, nyc), # Join on the nyc coords
    fire_sums = count_fires(dat), # Sum up n fires per lat-long combo
    
    time_graph = graph_fire_times(dat),
    plot = plot_fires(dat, nyc),

    strings_in_dots = "literals"
  )


burner_plan <- 
  drake_plan(
    seed_burn = get_tweets(n_tweets_seed = 2,
                           user = burner_acct),
    full_burn = target(
      command = get_tweets(seed_burn),
      trigger = trigger(condition = !is.null(get_more_tweets(seed_burn)))
    ),
    
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





