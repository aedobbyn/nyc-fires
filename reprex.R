
library(drake)
library(fs)
library(here)
library(tidyverse)

pkgconfig::set_config("drake::strings_in_dots" = "literals")

dir_create("dir")
file_path <- here("dir", "mtcars.csv")

# If input file does exit, read it. If it doesn't exist, return mtcars.
seed_mtcars <- function(input_path = file_path,
                        output_path = file_path) {
  if (!is.null(input_path) && file.exists(input_path)) {
    out <-
      read_csv(input_path)
  } else {
    out <- mtcars %>%
      as_tibble()
  }
  
  write_csv(out, output_path)
  out
}

# If no tbl supplied, run seed_mtcars to return regular mtcars.
# If tbl supplied, log it and write the result to file
log_mtcars <- function(tbl = NULL,
                       input_path = file_path,
                       output_path = file_path) {
  if (is.null(tbl)) {
    out <- seed_mtcars(input_path = input_path)
  } else {
    out <-
      tbl %>%
      map_dfr(log)

    write_csv(out, output_path)
  }
  out
}

# Start with no file
if (file_exists(file_path)) file_delete(file_path)

# Returns regular mtcars
log_mtcars()

# Returns logged mtcars and saves to file
log_mtcars() %>%
  log_mtcars()

# Check that logged mtcars was written out
read_csv(file_path)

# Delete that file
if (file_exists(file_path)) file_delete(file_path)

mt_plan <-
  drake_plan(
    mt_seed = log_mtcars(
      input_path = here(file_in("dir/mtcars.csv")),
      output_path = here(file_in("dir/mtcars.csv"))
    ),
    mt_final = log_mtcars(mt_seed,
      input_path = here(file_in("dir/mtcars.csv")),
      output_path = here(file_in("dir/mtcars.csv"))
    )
  )

# Warning about missing input files
make(mt_plan)

# mt_seed is regular, mt_final is logged
loadd(mt_seed)
loadd(mt_final)
mt_seed
mt_final

# No warning
if (file_exists(file_path)) file_delete(file_path)
write_csv(mtcars, file_path)
make(mt_plan)

# Targets are always outdated and file is only listed as an input, not output
mt_config <- drake_config(mt_plan)
vis_drake_graph(mt_config)

