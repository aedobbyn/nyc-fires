
source(here::here("didnt_start_it.R"))

plan <-
  drake_plan(
    fires = try_get_fires(), # Grab some seed fires
    fires = try_get_fires(fires), # Reups if there are more
    addresses = pull_addresses(fires), # Extract addresses from tweets
    lat_long = get_lat_long(addresses), # Send to Google for lat-longs
    dat = join_on_city_data(lat_long, nyc), # Join on the nyc coords
    fire_sums = count_fires(dat), # Sum up n fires per lat-long combo
    
    time_graph = graph_fire_times(dat),
    plot = plot_fires(dat, nyc),

    strings_in_dots = "literals"
  )


