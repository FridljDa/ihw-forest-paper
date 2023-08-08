# try simulation flexible rule
#devtools::install(here::here("IHWForestPaper"))
#library(IHWForestPaper)
devtools::load_all(here::here("IHWForestPaper"))

### ---get input param---
# Check if a command-line argument is provided
if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  # Retrieve the command-line argument
  split_index <- commandArgs(trailingOnly = TRUE)[1]
  split_index <- as.numeric(split_index)
  num_splits <- commandArgs(trailingOnly = TRUE)[2]
  num_splits <- as.numeric(num_splits)
  dry_run <- FALSE
} else {
  num_splits <- 3
  split_index <- 2
  dry_run <- TRUE
}
### ---

prop_alt_function_creator <- discrete_prop_alt_creator
prop_alt_function_name <- "discrete_prop_alt"

## -----flexible alternative sim------
if (dry_run) {
  dimensions <- seq(from = 2, to = 2, by = 1)
  m <- 1000
  r <- 1
  seed = seq_len(r)
  ndim = c(1,2,3)
  signal_strength = 0.9
  lp_norm = 1
  target_average_alt_prob = 0.1
  kappa = 0
} else {
  dimensions <- seq(from = 1, to = 6, by = 1)
  m <- c(1000, 10000, 100000)
  r <- 100
  seed = seq_len(r)
  ndim = dimensions
  signal_strength = seq(0.1, 0.8, length.out = 2)
  lp_norm = c(1, 2,0.5)
  target_average_alt_prob = seq(0.1, 0.2, by = 0.01)
  kappa = seq(0, 0.1, length.out = 5)
}


print("\n")

##---parameters ---
list_of_parameters <- list(
  dimensions = dimensions,
  m = m, 
  kappa = kappa,
  ndim = ndim,
  signal_strength = signal_strength,
  lp_norm = lp_norm,
  target_average_alt_prob = target_average_alt_prob
)

sim_parameters <- create_dataframe(list_of_parameters)
sim_parameters <- sim_parameters %>% merge(data.frame(seed = seed))
cat(timestamp(),"\n")

print("\n")

##----extract ---
sim_parameters_sub <- get_split(sim_parameters, num_splits, split_index)

##--create simulation---
simulation_list <- flexible_prop_alt_sim(
  sim_parameters = sim_parameters_sub,
  prop_alt_function_creator = prop_alt_function_creator,
  prop_alt_function_name = prop_alt_function_name
)

##---evaluate mehtods on simulation---
evaluated_simulation <- eval_sim_parallel(simulation_list,
                                          alpha = 0.1,
                                          methods = c("IHW-quantile", "IHW-forest", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM")
)

print("\n")

##---save result----
saveRDS(evaluated_simulation, paste0(
  "simulation_flexible/data/",
  Sys.Date(), "_", split_index, "_", prop_alt_function_name, "_eval_.Rds"
))