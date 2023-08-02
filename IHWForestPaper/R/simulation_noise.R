library(doRNG)
library(doParallel)
library(parallel)

## ------Simulation------
prop_alt <- function(cov_row) {
  #r <- sum(cov_row^2)
  # exp(-5*r)
  # 1 / (1 + exp(-cov1))
  # 1 / (1 + exp(-1 * (3 * cov_row[1]+cov_row[2] - 5)))
  # browser()
  # ifelse(sum(cov_row^2) <= 0.5, 0.9, 0)
  # ifelse(r <= 0.1, 0.9, 0)
  # 1 / (1 + exp(-cov_row[1]))
  # ifelse(sum(cov_row^2)  <= 1, 0.02, 0.4)
  # ifelse(cov_row[1]^2+cov_row[2]^2  <= 1, 0.02, 0.4)
  #ifelse(cov_row[1] <= 0.1, 0.9, 0)
 # pi1s <- ifelse(Xs[ ,1] <= 0.1, 0.9, 0)
   #ifelse(r <= 0.1, 0.9, 0) #This works well
  ifelse(cov_row[length(cov_row)] <= 0.1, 0.9, 0)
}

#' Noise Simulation Function
#'
#' This function generates noise simulations in a high dimensional space. 
#' The dimensions, random seed, and number of simulations are user-defined.
#' This function can be useful for simulations in high dimensional analysis,
#' especially when the impact of noise needs to be considered.
#'
#' @param m Integer. Defines the number of simulations to perform.
#' @param r Integer. Defines the number of unique random seeds to use.
#' @param dimensions Integer vector. Each element specifies a number of dimensions to simulate.
#'
#' @return A list of lists. Each sublist contains simulation results for each seed 
#' and dimension combination, including the covariate matrix, proportion of alternative
#' hypothesis cases, boolean vector of hypothesis tests, p-values for each simulation,
#' dimension of the simulation, the seed used, and the number of simulations.
#'
#' @examples
#' noise_sim(1000, 5, c(2, 4, 6))
#'
#' @export
#'
noise_sim <- function(m, r, dimensions){
  
  # Create combinations of m, dimensions, and seed
  sim_combs <- expand.grid(
    m = m,
    dimensions = dimensions,
    seed = seq_len(r)
  )
  
  # Apply simulation for each row in the combinations
  simple_sim <- lapply(seq_len(nrow(sim_combs)), function(i) {
    
    # Assign m, dimension, and seed for current iteration
    m_i <- sim_combs$m[i]
    dimension_i <- sim_combs$dimensions[i]
    seed_i <- sim_combs$seed[i]
    
    # Generate covariates for current simulation
    covariate_i <- matrix(runif(m_i * dimension_i, 0, 1), nrow = m_i)
    
    # Calculate proportion of alternative cases
    prop_alt_i <- apply(covariate_i, 1, prop_alt)
    
    # Generate hypothesis tests
    Hs_i <- rbinom(m, size = 1, prop_alt_i)
    
    # Generate p-values
    pvalue_i <- ifelse(Hs_i,
                       rbeta(m_i, 0.25, 1),  # for Hs_i == TRUE
                       runif(m_i)             # for Hs_i == FALSE
    )
    
    # Return results of the simulation as a list
    return(list(covariate = covariate_i, prop_alt = prop_alt_i, Hs = Hs_i, pvalue = pvalue_i, 
                dimension = dimension_i, seed = seed_i, m_i = m))
  })
  
  # Return all simulation results
  simple_sim
}
