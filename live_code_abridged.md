    # Grab all funs
    source(here::here("R", "didnt_start_it.R"))

### Burner account time

Our first draft of the plan:

    burner_plan <-
      drake_plan(
        # Reads in file from burner_path if file exists, otherwise pulls in seed tweets
        seed_burn = get_tweets(
          user = burner_handle,
          input_path = burner_path
        ),
        
        full_burn = get_tweets(
            tbl = seed_burn,
            user = burner_handle,
            output_path = burner_path # Outputs to burner_path
          )
      )

Define a file path to dump the tweets we collect into.

    burner_path <- here("data", "raw", "burn.csv") # Same as "<working_dir>/data/raw/burn.csv"

    # Delete the file in burner_path if it exists already so we start from scratch
    if (file_exists(burner_path)) file_delete(burner_path)

### Test out v1 of our plan

Since `seed_burn` hasn’t changed and the code to generate the targets
hasn’t changed, every time after the first time we run this plan `drake`
will let us know that our targets are up to date and not re-run
anything.

    # Take a look at the dataframe that represents our plan
    burner_plan

    # Save the config in an object we can look at
    burner_config <- drake_config(burner_plan)

    # The config knows everything
    sort(names(burner_config))
    burner_config$plan
    burner_config$cache_path
    burner_config$graph

    # Both seed_burn and full_burn should be outdated
    vis_drake_graph(burner_config)

    # Remove targets if they were already built
    clean()

    # Make the plan
    make(burner_plan, verbose = 4)

    # Everything should already be up to date 
    outdated(burner_config)

    # Which is reflected in our graph
    vis_drake_graph(burner_config)

    # Can show just targets if things are too crowded
    vis_drake_graph(burner_config, targets_only = TRUE)

We can now load these targets into our working environment.

    # They won't be found if we don't loadd() them
    seed_burn

    loadd(seed_burn)
    seed_burn

Every subsequent time we re-`make` the plan, `drake` should tell us we
don’t need to do anything.

    # Unloads (removes) the global variable seed burn we've loadd'd to prevent conflicts
    make(burner_plan)
    # Not in our env anymore
    seed_burn

    make(burner_plan)
    make(burner_plan)

If we `clean()`, however, we’ll remake the plan from scratch.

    clean()

    # Targets we built are gone from the cache
    loadd(seed_burn)

    # Targets are outdated now
    vis_drake_graph(burner_config, targets_only = TRUE) 

    # Make the plan again.
    # Since we saved the previous output of full_burn to a file, this time we read that in for our seed_burn
    make(burner_plan) 

    # And now everything is up to date again
    vis_drake_graph(burner_config, targets_only = TRUE) 

### Changing Code

One way we can guarantee that `drake` will re-`make` a target is if some
part of the code used to generate that target changes.

Right now, nothing outdated becuase we just ran `make(burner_plan)`.

    outdated(burner_config)

Let’s modify the code to a function called by `get_tweets`. Note that
`drake` recognizes that targets that incorporate this function at some
point in the pipeline are out of date, even though this function isn’t
called in the plan directly.

Our original function, `there_are_new_tweets` which checks if the
specified user has tweeted anything since the most recent tweet in our
`tbl` argument:

    there_are_new_tweets <- function(tbl,
                                     user = firewire_handle,
                                     verbose = TRUE) {
      latest_dt <-
        tbl %>%
        arrange(desc(created_at)) %>%
        slice(1) %>%
        pull(created_at)

      if (verbose) message("Searching for new tweets.")

      new <- get_seed_tweets(user = user, n_tweets = 1)

      if (max(new$created_at) <= latest_dt) {
        if (verbose) message("No new tweets to pull.")
        FALSE
      } else {
        TRUE
      }
    }

We’ll change the message to include a smiley emoji `emo::ji('smile')`
when `there_are_new_tweets` is called.

    there_are_new_tweets <- function(tbl,
                                     user = firewire_handle,
                                     verbose = TRUE) {
      latest_dt <-
        tbl %>%
        arrange(desc(created_at)) %>%
        slice(1) %>%
        pull(created_at)

      if (verbose) message(glue("Searching for new tweets! {emo::ji('smile')}"))

      new <- get_seed_tweets(user = user, n_tweets = 1)

      if (max(new$created_at) <= latest_dt) {
        if (verbose) message("No new tweets to pull.")
        FALSE
      } else {
        TRUE
      }
    }

Without doing anything else, we can check that both targets which had
been up to date has now been invalidated.

    outdated(burner_config)
    vis_drake_graph(burner_config)

Notice that we didn’t even need to re-define our plan; `drake` can tell
that a function a target in our plan relies on has meaningfully changed.

When we re-`make` our plan we should be messaged an 😄.

    make(burner_plan)

And now `seed_burn` and `full_burn` should both be up to date again.

    outdated(burner_config)
    vis_drake_graph(burner_config)

### Add Triggers

I mentioned that a way to guarantee that targets are re-made even if
code *doesn’t* change is to associate a trigger with a target.

Let’s add a
[trigger](https://ropensci.github.io/drake/articles/debug.html#test-with-triggers-)
so that we always run `get_tweets` to look for new tweets at the burner
handle.

    burner_plan_2 <-
      drake_plan(
        seed_burn = get_tweets(
          user = burner_handle,
          input_path = burner_path
        ),
        
        full_burn = target(
          command = get_tweets(
            tbl = seed_burn,
            user = burner_handle,
            output_path = burner_path 
          ),
          trigger = trigger(
            condition = TRUE
          )
        )
      )

This trigger will make our `full_burn` target look always out of date to
`drake` since it knows we need to re-make `full_burn` every time
`make()` is run.

Our `condition` always evaluates to `TRUE` because it’s just the value
`TRUE`, but you can also sub in any expression that returns a boolean.

    is_even_day <- function(input_date = Sys.Date()) {
      this_day <- 
        input_date %>% lubridate::day()
      
      this_day %% 2 == 0
    }

    is_even_day()
    is_even_day(Sys.Date() + 1)
    is_even_day(Sys.Date() + 2)

### Test out the `burner_plan` 2.0

First we’ll clean out the previous file we saved and start from scratch.

    if (file_exists(burner_path)) file_delete(burner_path)

Our `full_burn` target will always be out of date because the trigger
indicates that it always needs to be rebuilt, but `seed_burn` will now
be up-to-date.

    clean()

    # Notice our new trigger column
    burner_plan_2

    # seed_burn is outdated becuase we haven't made the plan yet.
    # full_burn is always out of date, no matter what, because of the trigger.
    burner_config_2 <- drake_config(burner_plan_2)
    outdated(burner_config_2)
    vis_drake_graph(burner_config_2, targets_only = TRUE)

All tweets are now stored in `burner_path`, which we specified as our
`output_path` when making `full_burn`.

What’s stored in this file is the same as both `seed_burn` and
`full_burn` becuase there were no tweets posted between when we made
`seed_burn` and `full_burn`.

    make(burner_plan_2)

    # Load the results of seed_burn and full_burn into our environment
    loadd(seed_burn)
    loadd(full_burn)
    # These should be the same since no new tweets posted
    expect_equal(seed_burn, full_burn)

    # full_burn always outdated, seed_burn no longer outdated
    outdated(burner_config_2)
    vis_drake_graph(burner_config_2, targets_only = TRUE)

Now let’s post a new tweet and re-`make` the plan.

`seed_burn` will be read in from the file, which reflects the state of
the world before this tweet. Then `full_burn` will incorporate the new
tweet.

    # Tweet here
    post_tweet(status = 
                 digest::digest(sample(100, 1)), 
               token = firewire_token)


    # seed_burn (read from file) is up to date, and full_burn will always look outdated
    make(burner_plan_2)
    vis_drake_graph(burner_config_2, targets_only = TRUE) 

    loadd(seed_burn)
    loadd(full_burn)
    # We should have one extra row in full_burn than in seed_burn, because seed_burn wasn't rebuilt
    expect_gt(nrow(full_burn), nrow(seed_burn)) 

    seed_burn
    full_burn

    # Let's check that full_burn was saved to file
    saved_burn <- read_csv(burner_path)
    expect_identical(nrow(full_burn), nrow(saved_burn))

Now we’ve proven that we can successfully trigger the re-building of
`full_burn` every time and save it as the latest state of the world to a
file.

Every time new tweets arrive, that file will be updated at the end of
the `make()` run.

### Write our full plan

Let’s go back to our original NYCFireWire Twitter account.

For the purposes of illustration, I’ll set a `max_id` on our
`seed_fires` so that we can re-up and grab more tweets to build `fires`.

    fire_path <- here("data", "raw", "fires.csv")
    if (file_exists(fire_path)) file_delete(fire_path)

    plan <-
      drake_plan(
        seed_fires = get_tweets( 
          n_tweets_seed = 2,
          max_id = old_tweet_id,
          input_path = fire_path
        ), 
        fires = target(
          command = get_tweets(
            tbl = seed_fires,
            n_tweets_reup = 3,
            output_path = fire_path
          ),
          trigger = trigger(
            condition = TRUE # Always look for new tweets
          )
        ),
        addresses = pull_addresses(fires), # Extract addresses from tweets
        lat_long = get_lat_long(addresses), # Send to Google for lat-longs
        
        fire_sums = count_fires(lat_long), # Sum up n fires per lat-long combo
        plot = plot_fire_sums(fire_sums)
      )

### Run our plan

    plan

    make(plan, verbose = 4)

    # See what a couple of our targets look like
    loadd(addresses)
    addresses

    loadd(lat_long)
    lat_long

And our `fire_sums_plot` is saved to our `plots` directory.

### That’s all!

Any q’s?
