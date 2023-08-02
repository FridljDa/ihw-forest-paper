
inv_logit <- function(x) {exp(x) / (1 + exp(x))}

#' High Dimensional Simulation Function
#'
#' This function creates simulations in a high dimensional space. The dimensions,
#' random seed, and number of simulations are user-defined. Simulations are 
#' created using Bernoulli, Exponential distributions and specific beta values.
#' This function can be useful for simulations in high dimensional analysis.
#'
#' The key equations for the simulation are:
#' \deqn{\beta_{\pi} = [3, 3, \underbrace{0, \dots, 0}_{d-2}]}
#' \deqn{\beta_{\mu} = [2, 2, \underbrace{0, \dots, 0}_{d-2}]}
#' \deqn{\beta_{0_{\mu}} = 0}
#'
#' \deqn{X \sim U[0,1]^{d}}
#' \deqn{\pi_1(X) = \left(\frac{1}{{1 + e^{-(X \times \beta_{\pi} + \beta_{0_{\pi}})}}}\right)}
#' \deqn{\mu(X) = \max(1, X \times \beta_{\mu} + \beta_{0_{\mu}})}
#' \deqn{H \mid X \sim \text{{Bernoulli}}(\pi(X))}
#' \deqn{Y \mid H = 1 \sim \text{{Exponential}}(1/\mu(X))}
#' \deqn{Y \mid H = 0 \sim \text{{Exponential}}(1)}
#' \deqn{P = e^{-Y}}
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
#' high_dim_sim(1000, 5, c(2, 4, 6))
#'
#' @export
#'
high_dim_sim <- function(m, r, dimensions){
  
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
    covariate_i <- matrix(runif(m_i * dimension_i), nrow = m_i)
    
    # Assign beta.pi, pi and mu parameters
    pi1 <- 0.1
    beta.pi <- c(3, 3, rep(0, dimension_i-2))
    beta0.pi <- uniroot(function(b){
      mean(inv_logit(covariate_i %*% beta.pi + b)) - pi1
    }, c(-100, 100))$root
    pi <- inv_logit(covariate_i %*% beta.pi + beta0.pi)
    
    # Assign beta.mu parameters
    beta.mu <- c(2, 2, rep(0, dimension_i-2))
    beta0.mu <- 0
    mu <- pmax(1, covariate_i %*% beta.mu + beta0.mu)
    
    # Generate hypothesis tests
    Hs_i <- as.logical(ifelse(runif(m) < pi, 1, 0))
    
    # Generate p-values
    y <- ifelse(Hs_i, rexp(m, 1/mu), rexp(m, 1))
    pvalue_i <- exp(-y)
    prop_alt_i <- mean(Hs_i)
    
    # Return results of the simulation as a list
    return(list(covariate = covariate_i, prop_alt = prop_alt_i, Hs = Hs_i, 
                pvalue = pvalue_i, dimension = dimension_i, seed = seed_i, m_i = m))
  })
  
  # Return all simulation results
  simple_sim
}
