
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

write_csv(fires, here("data", "raw", "fires.csv"))
write_csv(addresses, here("data", "derived", "addresses.csv"))
write_csv(lat_long, here("data", "derived", "lat_long.csv"))
write_csv(nyc, here("data", "raw", "nyc.csv"))
write_csv(dat, here("data", "derived", "dat.csv"))


fires <- read_csv(here::here("data", "raw", "fires.csv"))
addresses <- read_csv(here::here("data", "derived", "addresses.csv"))
lat_long <- read_csv(here::here("data", "derived", "lat_long.csv"))
nyc <- read_csv(here::here("data", "raw", "addresses.csv"))
dat <- read_csv(here::here("data", "derived", "dat.csv"))

plot_fires()
