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
  num_splits <- 1
  split_index <- 1
  dry_run <- FALSE
}
### ---

prop_alt_function_creator <- discrete_prop_alt_creator
prop_alt_function_name <- "discrete_prop_alt"

## -----flexible alternative sim------
if (dry_run) {
  dimensions <- seq(from = 1, to = 5, by = 1)
  m <- c(1000)
  r <- 20
  seed = seq_len(r)
  #ndim = dimensions
  signal_strength = 0.8
  lp_norm = 1
  target_average_alt_prob = 0.2#seq(0.1, 0.2, by = 0.01)# # ##
  beta_shape1 = 0.25 #seq(0.25, 0.1, length.out = 5)
  kappa = seq(0, 0.1, length.out = 5)
  alpha = 0.1
  ndim <- 1#c(1,2,3)
  
  methods <- c("BH", "AdaPT") #"IHW-quantile", "IHW-forest", , "Boca-Leek", "Clfdr-EM"
} else {
  dimensions <- seq(from = 1, to = 10, by = 1)
  m <- c(1000)
  r <- 50
  seed = seq_len(r)
  #ndim = dimensions
  signal_strength = seq(0.8, 0.2, length.out = 5) #
  lp_norm = c(1,2,0.5)
  target_average_alt_prob = seq(0.3, 0.1, length.out = 5)#0.2#seq(0.1, 0.2, by = 0.01)# # ##
  beta_shape1 = 0.25 #seq(0.25, 0.1, length.out = 5)
  kappa = 0#seq(0, 0.1, length.out = 5)
  alpha = 0.1
  ndim <- seq(from = 1, to = 5, by = 1)#dimensions#c(1,2,3)#1#
  
  methods <- c("IHW-quantile", "IHW-forest", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM")
}


cat("\n")

##---parameters ---
list_of_parameters <- list(
  dimensions = dimensions,
  m = m, 
  kappa = kappa,
  #ndim = ndim,
  signal_strength = signal_strength,
  lp_norm = lp_norm,
  target_average_alt_prob = target_average_alt_prob,
  beta_shape1 = beta_shape1,
  alpha = alpha
)

sim_parameters <- create_dataframe(list_of_parameters)

sim_parameters <- sim_parameters %>% 
  merge(data.frame(seed = seed)) %>% 
  merge(data.frame(ndim = ndim)) %>%
  filter(ndim <= dimensions)

cat(timestamp(),"\n")

cat("\n")
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
                                          methods = methods
)

print("\n")

##---save result----
saveRDS(evaluated_simulation, paste0(
  "simulation_flexible/data/",
  Sys.Date(), "_", split_index, "_", prop_alt_function_name, "_eval.Rds"
))