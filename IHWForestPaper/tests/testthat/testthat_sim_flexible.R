
library(testthat)

test_that("average alternative probability is approximately equal to target", {
  # Define parameters
  seed <- 123
  dimensions <- 5
  ndim <- 3
  c <- 0.9
  lp_norm <- 2
  target_average_alt_prob <- 0.1
  m_i <- 100000
  
  # Create the function
  discrete_prop_alt <- discrete_prop_alt_creator(seed = seed, 
                                                 dimensions = dimensions, 
                                                 ndim = ndim, 
                                                 c = c, 
                                                 lp_norm = lp_norm, 
                                                 target_average_alt_prob = target_average_alt_prob)
  
  # Generate covariates for current simulation
  covariate_i <- matrix(runif(m_i * dimensions, -0.5, 0.5), nrow = m_i)
  
  # Calculate proportion of alternative cases
  prop_alt_i <- apply(covariate_i, 1, discrete_prop_alt)
  
  # Generate hypothesis tests
  Hs_i <- rbinom(m_i, size = 1, prob = prop_alt_i)
  
  mean_alt_probs <- mean(Hs_i)
  # Check if the average alternative probability is approximately equal to the target
  expect_lt(abs(mean_alt_probs - target_average_alt_prob), 0.05)
})
