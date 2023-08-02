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
devtools::load_all(here::here("IHWForestPaper/adaptMT"))

if(Sys.info()["sysname"] == "Darwin"){
  devtools::load_all("/Users/default/Google Drive/currentDocumants/research/2022_IHW-Forest/Code/IHW")
}else{
  devtools::load_all("/g/huber/users/fridljand/R/IHW")
}

###---get input param---
# Check if a command-line argument is provided
if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  # Retrieve the command-line argument
  seed <- commandArgs(trailingOnly = TRUE)[1]
  seed <- as.numeric(seed)

} else {
  seed <- 2
}
set.seed(seed)

## ---parameters----
# number of monte carlo replicates, increases run time immensely!

# folds_fdp_eval <- sample(1:3, m, replace = TRUE)


## -----high dim sim------
dimensions <- seq(from = 2, to = 10, by = 1)

cat("seed\n")
print(seed)

cat("dimensions\n")
print(dimensions)
print("\n")


m = 10000
r = 100

##---eval high dim sim---
sim <- high_dim_sim(m, r, dimensions)
#n.cores <- parallel::detectCores()
#doParallel::registerDoParallel(cores = min(3, n.cores - 1))
#browser()
# Define the filename where the simulation results will be stored
simulation_results_filename <- paste0("simulation/data/", Sys.Date(), "_", seed,"_eval_high_dim_sim_adapt.csv")

# Loop over each simulation
for(i in seq_along(sim)) {
  
  # Display the current simulation run
  cat("simulation run:", i,"/", length(sim), "\n")
  
  # Get the current simulation parameters
  sim_i <- sim[[i]]
  dimension_i <- sim_i$dimension
  seed_i <- sim_i$seed
  Ps_i <- sim_i$pvalue
  Xs_i <- sim_i$covariate
  Hs_i <- sim_i$Hs
  
  # Run the simulation with the current parameters
  sim_res_i <- run_sim_adapt(Ps_i, Xs_i, Hs_i, seed_i, alpha = 0.1, m = m, lfdr_only = lfdr_only, forest_par, null_proportion = null_proportion)
  
  # Add additional data to the simulation results
  sim_res_i <- mutate(sim_res_i, dimension = dimension_i, timestamp = as.character(Sys.time()))
  
  # If this is the first simulation, create a new CSV file.
  # Otherwise, append the results to the existing file.
  if(!file.exists(simulation_results_filename)) {
    write.csv(sim_res_i, simulation_results_filename, row.names = FALSE) 
  } else {
    write.table(sim_res_i, simulation_results_filename, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
  }
}

#print the head 
# Load the data from the CSV file
simulation_results_data <- read.csv(simulation_results_filename)

# Print the first few rows
head(simulation_results_data)
