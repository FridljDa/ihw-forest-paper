# parent
library(magrittr)
library(dplyr)

# library(doParallel)
# n.cores <- parallel::detectCores()
# doParallel::registerDoParallel(cores = min(5, n.cores - 1))
# library(doRNG)
# set.seed(123)

# library(IHWStatsPaper)

# devtools::load_all("/Users/default/Google Drive/currentDocumants/Studium/Master/3.Semester/Masterarbeit/Code/IHW")
# library("IHW")
#devtools::load_all("/Users/default/Google Drive/currentDocumants/research/2022_IHW-Forest/Code/IHW")
devtools::load_all("IHWForestPaper")
devtools::load_all("../IHW")
## ---parameters----
m <- 1e5 # TODO more
r <- 1
# number of monte carlo replicates, increases run time immensely!

alpha <- .1

# folds_fdp_eval <- sample(1:3, m, replace = TRUE)

forest_par <- list(
  ntrees = 1,
  n_censor_thres = 1,
  nodedepth = 3,
  nodesize = 1000
)
## -----small region sim------
# lengths <- seq(from = 1, to = 1001, by = 250)

# eval_small_region_sim <- IHWForestPaper::eval_small_region_sim(m, r, lengths, forest_par)
# saveRDS(eval_small_region_sim, file = "precomputed_results/small_region_sim.Rds")

## -----noise sim------
# dimensions <- seq(from = 1, to = 11, by = 10)

eval_noise_sim <- eval_noise_sim(m,
  r,
  dimensions = seq(from = 2, to = 2, by = 1),
  forest_par,
  lfdr_only = TRUE,
  null_proportion = TRUE
)
saveRDS(eval_noise_sim, file = "precomputed_results/noise_sim.Rds")
