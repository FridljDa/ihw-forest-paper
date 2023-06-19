# parent
library(magrittr)
library(dplyr)

# library(doParallel)
# n.cores <- parallel::detectCores()
# doParallel::registerDoParallel(cores = min(5, n.cores - 1))
# library(doRNG)
# set.seed(123)

# library(IHWStatsPaper)

# library("IHW")
#devtools::load_all("/Users/default/Google Drive/currentDocumants/research/2022_IHW-Forest/Code/IHW")
devtools::load_all(here::here("IHWForestPaper"))
devtools::load_all("/g/huber/users/fridljand/R/IHW")
devtools::load_all(here::here("IHWForestPaper/adaptMT"))
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

#eval_noise_sim <- eval_noise_sim(m,
#  r,
#  dimensions = seq(from = 1, to = 4, by = 1),
#  forest_par,
#  lfdr_only = TRUE,
#  null_proportion = TRUE
#)
#saveRDS(eval_noise_sim, file = "precomputed_results/noise_sim.Rds")

## -----high dim sim------
eval_high_dim_sim <- eval_high_dim_sim(
  m = 1000,
  r = 1,
  dimensions = seq(from = 2, to = 2, by = 1),
  forest_par,
  lfdr_only = TRUE
)
saveRDS(eval_high_dim_sim, file = "data/eval_high_dim_sim.Rds")