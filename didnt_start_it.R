
library(tidyverse)
library(twitteR)

source(here::here("key.R"))

fires_raw <- userTimeline("NYCFireWire")

fires <- 
  fires_raw %>% 
  twListToDF() %>% 
  as_tibble()

fires %>% 
  mutate(
    borough = str_extract(text, "^[^\\s]*\\s") %>% 
      str_remove("\\s"),
    street = str_extract(text, "(\\*.+[,\\.])") %>% # All text after an asterisk and before a comma or period
      str_remove_all("(\\*.+\\*)"), # Get rid of stuff in between asterisks
    address = glue::glue("{street}, {borough}")
  ) %>% 
  select(borough, street, address, text)


