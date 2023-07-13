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

###---get input param---
# Check if a command-line argument is provided
if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  # Retrieve the command-line argument
  seed <- commandArgs(trailingOnly = TRUE)[1]
  seed <- as.numeric(seed)

} else {
  seed <- 1
}
set.seed(seed)

## ---parameters----
# number of monte carlo replicates, increases run time immensely!

# folds_fdp_eval <- sample(1:3, m, replace = TRUE)


## -----high dim sim------
dimensions <- seq(from = 2, to = 2, by = 1)

cat("seed\n")
print(seed)

cat("dimensions\n")
print(dimensions)
print("\n")

##---eval high dim sim---
sim <- high_dim_sim(m, r, dimensions)
#n.cores <- parallel::detectCores()
#doParallel::registerDoParallel(cores = min(3, n.cores - 1))
#browser()
eval_adapt <- lapply(seq_along(sim), function(i){
  #eval <- foreach(i = seq_along(sim), .combine = rbind) %dorng% {
  #i <- 1
  print(paste0("simulation run:", i,"/", length(sim)))
  sim_i <- sim[[i]]
  dimension_i <- sim_i$dimension
  seed_i <- sim_i$seed
  
  Ps_i <- sim_i$pvalue
  Xs_i <- sim_i$covariate
  Hs_i <- sim_i$Hs
  
  sim_res_i <- run_sim_adapt(Ps_i, Xs_i, Hs_i, seed_i, alpha, m = m, lfdr_only = lfdr_only, forest_par, null_proportion = null_proportion)
  
  mutate(sim_res_i, dimension = dimension_i)
})
eval_adapt <- bind_rows(eval_adapt)
##------

print("\n")
print(head(eval_adapt))

saveRDS(eval_high_dim_sim, paste0("simulation/data/", Sys.Date(), "_", seed,"_eval_high_dim_sim_adapt.Rds"))