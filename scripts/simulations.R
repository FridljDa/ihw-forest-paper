#parent
library(magrittr)
library(dplyr)

#library(doParallel)
#n.cores <- parallel::detectCores()
#doParallel::registerDoParallel(cores = min(5, n.cores - 1))
#library(doRNG)
#set.seed(123)

#library(IHWStatsPaper)
# TODO
# devtools::load_all("../IHW")
# library("IHW")
devtools::load_all("IHWForestPaper")

##---parameters----
m <- 5e4 #TODO more
r <- 200
r <- 40 # number of monte carlo replicates, increases run time immensely!
alpha <- .1

#folds_fdp_eval <- sample(1:3, m, replace = TRUE)

forest_par <- list(
  ntrees = 3,
  n_censor_thres = 10,
  nsplit = 3,
  maxdepth = 3,
  nodesize = "auto"
)
## -----small region sim------
lengths <- seq(from = 1, to = 1001, by = 250)
#lengths <- 2

#eval_small_region_sim <- IHWForestPaper::eval_small_region_sim(m, r, lengths, forest_par)
#saveRDS(eval_small_region_sim, file = "precomputed_results/small_region_sim.Rds")

## -----noise sim------

#dimensions <- 2
dimensions <- seq(from = 1, to = 11, by = 2)

eval_noise_sim <- IHWForestPaper::eval_noise_sim(m, r, dimensions, forest_par)
saveRDS(eval_noise_sim, file = "precomputed_results/noise_sim.Rds")

