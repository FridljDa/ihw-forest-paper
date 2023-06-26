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

m = 1000
r = 5
#tau = 0.5
ntrees = c(5,10,20,30)
nodedepth = c(3,4,5,6)
nodesize = c(1000,2000,3000)

## -----high dim sim------
dimensions <- seq(from = 2, to = 4, by = 1)

cat("dimensions\n")
cat(dimensions)
cat("\n")
eval_high_dim_sim_param_df <- eval_high_dim_sim_param(
  m = m,
  r = r,
  dimensions = dimensions,
  tau = tau,
  ntrees = ntrees,
  nodedepth = nodedepth,
  nodesize = nodesize,
  forest_par
)
cat("\n")
print(head(eval_high_dim_sim_param_df))

saveRDS(eval_high_dim_sim_param_df, paste0("grid_search/data/", Sys.Date(), "_eval_high_dim_sim_param.Rds"))

