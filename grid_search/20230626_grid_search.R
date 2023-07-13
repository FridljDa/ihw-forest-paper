# parent
library(magrittr)
library(dplyr)

# library(doParallel)
# n.cores <- parallel::detectCores()
# doParallel::registerDoParallel(cores = min(5, n.cores - 1))
# library(doRNG)
# set.seed(123)


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
  num_splits <- commandArgs(trailingOnly = TRUE)[2]
  num_splits <- as.numeric(num_splits)
  split_index <- commandArgs(trailingOnly = TRUE)[3]
  split_index <- as.numeric(split_index)
  
} else {
  seed <- 1
  num_splits <- 3
  split_index <- 2
}
set.seed(seed)

## ---parameters----
# number of monte carlo replicates, increases run time immensely!

# folds_fdp_eval <- sample(1:3, m, replace = TRUE)

m = 10000
r = 5

#forest param
tau = c(0.4,0.5,0.6,0.7,0.8,0.9)
#tau = c(0.4,0.5,0.6)
ntrees = c(5,10,20,30,40,50,100,200,300,500)
#ntrees = c(5,10)
nodesize = c(50,100,200,300,500,1000)
#nodesize = c(50)


param_grid <- expand.grid(
  tau = tau,
  ntrees = ntrees,
  nodesize = nodesize#,
  #dimensions = dimensions#, 
  #seed = seq_len(r) #TODO
)

dimensions <- seq(from = 2, to = 8, by = 1)
#dimensions <- seq(from = 2, to = 3, by = 1)
##----extract ---


# Calculating the size of each smaller data.frame
split_size <- ceiling(nrow(param_grid) / num_splits)

# Adding a new column for split indices
param_grid_sub <- param_grid %>%
  mutate(split_index_del = rep(1:num_splits, each = split_size, length.out = n()))

# Extracting the 3rd smaller data.frame
param_grid_sub <- param_grid_sub %>%
  filter(split_index_del == split_index) %>%
  select(-split_index_del)

## -----high dim sim------


cat("dimensions\n")
cat(dimensions)
cat("\n")
eval_high_dim_sim_param_df <- eval_high_dim_sim_param(
  m = m,
  r = r,
  dimensions = dimensions,
  tau = param_grid_sub$tau,
  ntrees = param_grid_sub$ntrees,
  nodesize = param_grid_sub$nodesize
)
cat("\n")
print(head(eval_high_dim_sim_param_df))
#nrow(eval_high_dim_sim_param_df) ==nrow(param_grid_sub)*length(dimensions)*r
#testthat::expect_equal(nrow(eval_high_dim_sim_param_df),
#                        nrow(param_grid_sub)*length(dimensions)*r)
saveRDS(eval_high_dim_sim_param_df, paste0("grid_search/data/", Sys.Date(), "_", seed, "_",split_index,"_eval_high_dim_sim_param.Rds"))

