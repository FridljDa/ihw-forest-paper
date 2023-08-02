#devtools::install(here::here("IHWForestPaper"))

if(Sys.info()["sysname"] == "Darwin"){
  devtools::load_all("/Users/default/Google Drive/currentDocumants/research/2022_IHW-Forest/Code/IHW")
}else{
  devtools::load_all("/g/huber/users/fridljand/R/IHW")
}

library(testthat)

test_that("run_sim returns correct output", {
  # Set seed for reproducibility
  set.seed(1)
  
  # Generate data
  X <- runif(2000, min = 0, max = 2.5) # covariate
  H <- rbinom(2000, 1, 0.1) # hypothesis true or false
  Z <- rnorm(2000, H * X) # Z-score
  Ps <- 1 - pnorm(Z) # pvalue
  
  methods <- c("IHW-quantile", "IHW-forest", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM")
  
  # Run the function
  sim_result <- run_sim(Ps, X, H, seed = 1, alpha = 0.1, methods = methods, forest_par = NULL, null_proportion = TRUE)
  
  # Check that output is a data frame
  expect_is(sim_result, "data.frame")
  
  # Check that output has expected number of columns
  expect_equal(nrow(sim_result), length(methods))
  
  # Check that output has expected column names
  expected_cols <- c("rjs", "pow", "FDP", "FWER", "method", "pi0s", "seed", "alpha", "m")
  expect_equal(names(sim_result), expected_cols)
})

test_that("run_sim handles Hs = NULL correctly", {
  # Set seed for reproducibility
  set.seed(1)
  
  # Generate data
  X <- runif(2000, min = 0, max = 2.5) # covariate
  H <- rbinom(2000, 1, 0.1) # hypothesis true or false
  Z <- rnorm(2000, H * X) # Z-score
  Ps <- 1 - pnorm(Z) # pvalue
  
  methods <- c("IHW-quantile", "IHW-forest", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM")
  
  # Run the function
  sim_result <- run_sim(Ps, X, H = NULL, seed = 1, alpha = 0.1, methods = methods, forest_par = NULL, null_proportion = TRUE)
  
  # Check that output is a data frame
  expect_is(sim_result, "data.frame")
  
  # Check that output has expected number of columns
  expect_equal(ncol(sim_result), 5)
  
  # Check that output has expected column names
  expected_cols <- c("rjs", "method", "seed", "alpha", "m")
  expect_equal(names(sim_result), expected_cols)
})

