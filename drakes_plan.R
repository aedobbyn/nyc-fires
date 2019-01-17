
source(here::here("didnt_start_it.R"))

plan <-
  drake_plan(
    seed_fires = get_seed_fires(),
    fires = get_fires(seed_fires),
    addresses = pull_addresses(fires),
    lat_long = get_lat_long(addresses),
    nyc = get_city_data(),
    dat = join_on_city_data(lat_long, nyc),
    plot = plot_fires(dat, nyc),

    strings_in_dots = "literals"
  )

make(plan)
