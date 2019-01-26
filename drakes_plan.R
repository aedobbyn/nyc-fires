
source(here::here("didnt_start_it.R"))

plan <-
  drake_plan(
    seed_fires = get_tweets( # Grab some seed fires
      n_tweets_seed = 2,
      max_id = sample_max_id
    ),
    fires = target(
      command = get_tweets(
        tbl = seed_fires,
        n_tweets_reup = 3
      ),
      trigger = trigger(
        condition = TRUE # Always look for new tweets
      )
    ),
    addresses = pull_addresses(fires), # Extract addresses from tweets
    lat_long = get_lat_long(addresses), # Send to Google for lat-longs
    dat = join_on_city_data(lat_long, nyc), # Join on the nyc coords
    fire_sums = count_fires(dat), # Sum up n fires per lat-long combo

    time_graph = graph_fire_times(dat),
    plot = plot_fires(dat, nyc)
  )
