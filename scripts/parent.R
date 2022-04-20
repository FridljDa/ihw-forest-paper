#parent
library(magrittr)
library(dplyr)

library(doParallel)
n.cores <- parallel::detectCores()
doParallel::registerDoParallel(cores = min(5, n.cores - 1))
library(doRNG)
set.seed(123)

library(IHWStatsPaper)
# TODO
# devtools::load_all("../IHW")
library("IHW")

##---parameters----
m <- 1e4
r <- 200
r <- 1 # number of monte carlo replicates, increases run time immensely!
alpha <- .1

#folds_fdp_eval <- sample(1:3, m, replace = TRUE)

forest_par <- list(
  ntrees = 3,
  ntaus = 10,
  nsplit = 3,
  maxdepth = 3,
  nodesize = "auto"
)

## -----noise sim------
dimensions <- seq(from = 1, to = 6, by = 1)
dimensions <- 2
source("IHWForestPaper/R/helper.R")
source("IHWForestPaper/R/noise_sim.R")
#rm(list = ls())
noise_sim_eval <- noise_sim_eval(m, r, dimensions, forest_par)
saveRDS(noise_sim_eval, file = "precomputed_results/noise_sim.Rds")