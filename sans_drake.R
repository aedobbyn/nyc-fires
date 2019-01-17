
source(here::here("didnt_start_it.R"))

fires <-
  get_fires("NYCFireWire")

addresses <-
  pull_addresses(fires)

lat_long <-
  get_lat_long(addresses)

nyc <- get_city_data()

dat <-
  join_on_city_data(lat_long)

plot_fires()