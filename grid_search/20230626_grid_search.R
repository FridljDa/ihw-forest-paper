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
devtools::load_all(here::here("IHWForestPaper"))

if(Sys.info()["sysname"] == "Darwin"){
  devtools::load_all("/Users/default/Google Drive/currentDocumants/research/2022_IHW-Forest/Code/IHW")
}else{
  devtools::load_all("/g/huber/users/fridljand/R/IHW")
}

#devtools::load_all(here::here("IHWForestPaper/adaptMT"))

## ---parameters----
# number of monte carlo replicates, increases run time immensely!

# folds_fdp_eval <- sample(1:3, m, replace = TRUE)

tau = 0.5
ntrees = 10
nodedepth = 3
nodesize = 1000

## -----high dim sim------
dimensions <- seq(from = 2, to = 20, by = 2)

print("dimensions\n")
print(dimensions)
print("\n")
eval_high_dim_sim_param_df <- eval_high_dim_sim_param(
  m = 10000,
  r = 10,
  tau = 0.5,
  ntrees = 10,
  nodedepth = 3,
  nodesize = 1000,
  dimensions = dimensions,
  forest_par,
  lfdr_only = TRUE
)
print("\n")
print(head(eval_high_dim_sim_param_df))

saveRDS(eval_high_dim_sim_param_df, paste0("simulation/data/", Sys.Date(), "_eval_high_dim_sim_param.Rds"))

