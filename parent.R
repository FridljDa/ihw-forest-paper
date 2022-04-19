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


source("scripts/helper.R")
source("scripts/noise_sim.R")
rm(list = ls())
