###----prop_alt_function_creator s----
#' Check if a vector is within a ball of a given radius in the p-norm
#'
#' @param vector A numeric vector.
#' @param radius The radius of the ball.
#' @param lp_norm The norm to be used. Must be a positive number.
#'
#' @return TRUE if the vector is within the ball, FALSE otherwise.
#'
#' @examples
#' in_ball(c(1, 2, 3), 4, 2)
#' in_ball(c(1, 2, 3), 2, 2)
#' in_ball(c(1, 2, 3), 1, 1)
in_ball <- function(vector, radius, lp_norm) {
  # Calculate the L^p norm of the vector
  vector_norm <- sum(abs(vector)^lp_norm)^(1/lp_norm)
  
  # Check if the L^p norm of the vector is less than or equal to the radius
  return(vector_norm <= radius)
}


#' Calculate the volume of an n-dimensional ball in the p-norm
#'
#' @param radius The radius of the ball.
#' @param lp_norm The norm to be used. Must be a positive number.
#' @param dimension The number of dimensions.
#'
#' @return The volume of the n-dimensional ball.
#'
#' @examples
#' ball_volume(1, 2, 3)
#' ball_volume(2, 2, 3)
#' ball_volume(1, 1, 2)
ball_volume <- function(radius, lp_norm, dimension) {
  return(2^dimension * base::gamma(dimension/lp_norm + 1) / base::gamma(dimension/2 + 1) * radius^dimension)
}


#' Create a function to generate discrete alternative probabilities.
#'
#' This function creates a function that generates discrete alternative probabilities based on random covariate values.
#'  The volume of an n-dimensional ball in the p-norm (also known as the L^p norm) is given by the formula:
#V = 2^n \frac{\Gamma\left(\frac{n}{p} + 1\right)}{\Gamma\left(\frac{n}{2} + 1\right)} r^n

#' @param seed The seed for random number generation (optional).
#' @param dimensions The total number of dimensions in the covariate matrix.
#' @param ndim The number of dimensions to consider when calculating the alternative probability (default is 1).
#' @param signal_strength signal_strength, between 0 and 1
#' @param lp_norm The norm to consider for the calculation (default is 1).
#' @param target_average_alt_prob The target average for the alternative probability.
#'
#' @return A function that generates discrete alternative probabilities based on input covariate values.
#'
#' @examples
#' # Create an alternative probability generator with 5 dimensions and considering 2 dimensions
#' alt_prob_generator <- discrete_prop_alt_creator(seed = 42, dimensions = 5, ndim = 2, c = 0.9)
#' # Generate alternative probability for a covariate vector
#' alt_prob <- alt_prob_generator(c(0.2, 0.5, 0.1, 0.8, 0.4))
#'
#' @export
discrete_prop_alt_creator <- function(seed, dimensions, ndim = 1, signal_strength = 0.9, lp_norm = 1, target_average_alt_prob = 0.1) {
  set.seed(seed)
  selected_dimensions <- sample(seq_len(dimensions), ndim, replace = FALSE)
  target_volume <- target_average_alt_prob/signal_strength
  radius <- uniroot(function(radius) ball_volume(radius, lp_norm, ndim) - target_volume, 
                    interval = c(0,1))$root
  
  discrete_prop_alt <- function(covariate_row) {
    covariate_row_selected <- covariate_row[selected_dimensions]
    ifelse(in_ball(covariate_row_selected, radius, lp_norm), signal_strength, 0)
  }
  
  return(discrete_prop_alt)
}

##--- flexible_prop_alt_sim---
#' Simulate alternative proportions flexibly 
#'
#' @description This function is designed to perform multiple simulations 
#' to calculate the proportion of alternative cases given various conditions.
#'
#' @param m A vector specifying the number of observations per simulation
#' @param r An integer specifying the number of replications for the simulation
#' @param dimensions A vector specifying the dimensionality of the covariates in the simulations
#' @param prop_alt_function_creator A function that creates alternative proportion functions, default is 'discrete_prop_alt_creator'
#' @param prop_alt_function_name A string naming the proportion alternative function creator, default is "discrete_prop_alt_creator"
#' @param additional_arguments_prop_alt_function_creator A dataframe that contains additional arguments needed for the prop_alt_function_creator function
#' @return A list containing the simulation results for each combination of m, dimension, and seed, including additional arguments if provided
#' 
#' @examples
#' flexible_prop_alt_sim(m = c(100,200), r = 2, dimensions = c(2,3))
#'
#' @export
flexible_prop_alt_sim <- function(m, r, dimensions, 
                                  prop_alt_function_creator = discrete_prop_alt_creator, 
                                  prop_alt_function_name = "discrete_prop_alt_creator",
                                  additional_arguments_prop_alt_function_creator = NULL) {
  
  # Create a dataframe with all combinations of seeds, dimensions, and m values
  sim_combs <- expand.grid(
    seed = seq_len(r),
    dimensions = dimensions
  )
  
  # If additional arguments are provided, merge them with the simulation combinations dataframe
  if(!is.null(additional_arguments_prop_alt_function_creator)){
    additional_arguments_names <- colnames(additional_arguments_prop_alt_function_creator)
    sim_combs <- sim_combs %>%
      merge(additional_arguments_prop_alt_function_creator)
    
    #unclean filter
    if("ndim" %in% colnames(sim_combs)){
      sim_combs <- sim_combs %>% filter(ndim <= dimensions)
    }
  }
  
  # Add the proportion alternative function to each row of the dataframe
  sim_combs$prop_alt_function <- purrr::pmap(sim_combs, prop_alt_function_creator)
  
  sim_combs <- sim_combs %>% merge(data.frame(m = m))
  
  # Apply simulation for each row in the combinations
  simple_sim <- lapply(seq_len(nrow(sim_combs)), function(i) {
    
    # Assign m, dimension, and seed for current iteration
    m_i <- sim_combs$m[[i]]
    dimension_i <- sim_combs$dimensions[[i]]
    seed_i <- sim_combs$seed[[i]]
    prop_alt_function_i <- sim_combs$prop_alt_function[[i]]
    
    # Generate covariates for current simulation
    covariate_i <- matrix(runif(m_i * dimension_i, -0.5, 0.5), nrow = m_i)
    
    # Calculate proportion of alternative cases
    prop_alt_i <- apply(covariate_i, 1, prop_alt_function_i)
    
    # Generate hypothesis tests
    Hs_i <- rbinom(m_i, size = 1, prob = prop_alt_i)
    
    # Generate p-values
    pvalue_i <- ifelse(Hs_i,
                       rbeta(m_i, 0.25, 1),  # for Hs_i == TRUE
                       runif(m_i)             # for Hs_i == FALSE
    )
    
    # Return results of the simulation as a list
    res <- list(covariate = covariate_i, prop_alt = prop_alt_i, Hs = Hs_i, pvalue = pvalue_i, 
                dimension = dimension_i, seed = seed_i, m_i = m_i, prop_alt_function_name = prop_alt_function_name)
    
    # Add the values of the additional arguments to the results if they exist
    if(!is.null(additional_arguments_prop_alt_function_creator)){
      for(arg in additional_arguments_names){
        res[[arg]] <- sim_combs[[i, arg]]
      }
    }
    
    return(res)
  })
  
  # Return all simulation results
  return(simple_sim)
}
