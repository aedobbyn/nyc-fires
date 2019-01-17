library(nycgeo) # https://github.com/mfherman/nycgeo
library(sf)


lat_long %>%
  drop_na(lat, long) %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326, agr = "constant"
  ) %>%
  st_join(nyc_boundaries(geography = "block"))

nyc_boundaries(geography = "block", add_acs_data = TRUE)$geometry[1]
