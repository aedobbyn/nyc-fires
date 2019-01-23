make(plan, verbose = 4)

plan

config <- drake_config(plan)
config$plan
config$prework
config$targets
config$cache_path
config$graph

vis_drake_graph()

loadd(plot)
loadd(dat)

clean(fires)

outdated(config)

vis_drake_graph()

make(plan)