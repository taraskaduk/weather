source("R/packages.R")
source("R/functions.R")
source("R/plan2.R")    

config <- drake_config(plan)
vis_drake_graph(config)

make(plan)
# unlink(tempdir(), recursive = TRUE)


