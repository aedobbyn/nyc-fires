
# What the flow would look like if we didn't use drake
# All of this would need to be re-run every time we wanted to reproduce the analysis :/

source(here::here("R", "didnt_start_it.R"))

fires <-
  get_tweets(
    output_path = here("data", "raw", "fires.csv")
  )

addresses <-
  pull_addresses(fires)

lat_long <-
  get_lat_long(addresses)

dat <-
  join_on_city_data(lat_long)

fire_sums <-
  dat %>%
  count_fires()

write_csv(fires, here("data", "raw", "fires.csv"))
write_csv(addresses, here("data", "derived", "addresses.csv"))
write_csv(lat_long, here("data", "derived", "lat_long.csv"))
write_csv(dat, here("data", "derived", "dat.csv"))
write_csv(fire_sums, here("data", "derived", "fire_sums.csv"))

graph_fire_times(dat)
plot_fire_sums(fire_sums)
