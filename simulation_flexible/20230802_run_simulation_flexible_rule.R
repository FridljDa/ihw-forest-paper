# try simulation flexible rule
#devtools::install(here::here("IHWForestPaper"))
#library(IHWForestPaper)
devtools::load_all(here::here("IHWForestPaper"))

### ---get input param---
# Check if a command-line argument is provided
if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  # Retrieve the command-line argument
  seed <- commandArgs(trailingOnly = TRUE)[1]
  seed <- as.numeric(seed)
  #prop_alt_index <- commandArgs(trailingOnly = TRUE)[2]
  #prop_alt_index <- as.numeric(prop_alt_index)
  prop_alt_index <- 1
  dry_run <- FALSE
} else {
  seed <- 2
  prop_alt_index <- 1
  dry_run <- TRUE
}
set.seed(seed)
### ---

# prop_alt_creator_list <- list(discrete_prop_alt_creator)
# prop_alt_creator_list_names <- list("discrete_prop_alt")

# prop_alt_function_creator = prop_alt_creator_list[[prop_alt_index]]
# prop_alt_function_name <- prop_alt_creator_list_names[[prop_alt_index]]
prop_alt_function_creator <- discrete_prop_alt_creator
prop_alt_function_name <- "discrete_prop_alt"

## -----flexible alternative sim------
if (dry_run) {
  dimensions <- seq(from = 2, to = 2, by = 1)
  m <- 1000
  r <- 1
  
  
  dimensions <- seq(from = 2, to = 2, by = 1)
  m <- 1000
  r <- 1
  
} else {
  dimensions <- seq(from = 2, to = 6, by = 1)
  m <- 1000
  r <- 5
  
}


set.seed(seed)
cat("seed", seed, "\n")
cat("dimensions", dimensions, "\n")

print("\n")

##--create simulation---
simulation_list <- flexible_prop_alt_sim(
  m = m,
  r = r,
  dimensions = dimensions,
  prop_alt_function_creator = prop_alt_function_creator,
  prop_alt_function_name = prop_alt_function_name
)

##---evaluate mehtods on simulation---
evaluated_simulation <- eval_sim_parallel(simulation_list,
  alpha = 0.1,
  methods = c("IHW-quantile", "IHW-forest", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM")
)

##---save result----
saveRDS(evaluated_simulation, paste0(
  "simulation_flexible/data/",
  Sys.Date(), "_", seed, "_", prop_alt_function_name, "_eval_.Rds"
))
