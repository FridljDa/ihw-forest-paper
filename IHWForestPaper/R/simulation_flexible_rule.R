### ----prop_alt_function_creator s----
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
  vector_norm <- sum(abs(vector)^lp_norm)^(1 / lp_norm)

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
  return(2^dimension * base::gamma(dimension / lp_norm + 1) / base::gamma(dimension / 2 + 1) * radius^dimension)
}


#' Create a function to generate discrete alternative probabilities.
#'
#' This function creates a function that generates discrete alternative probabilities based on random covariate values.
#'  The volume of an n-dimensional ball in the p-norm (also known as the L^p norm) is given by the formula:
# V = 2^n \frac{\Gamma\left(\frac{n}{p} + 1\right)}{\Gamma\left(\frac{n}{2} + 1\right)} r^n

#' @param seed The seed for random number generation (optional).
#' @param dimensions The total number of dimensions in the covariate matrix.
#' @param ndim The number of dimensions to consider when calculating the alternative probability (default is 1).
#' @param signal_strength signal_strength, between 0 and 1. If \code{target_average_alt_prob} is fixed, a high \code{signal_strength}
#' results in a small radius internally, i.e. small covariate region with signal.
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
  target_volume <- target_average_alt_prob / signal_strength
  radius <- uniroot(function(radius) ball_volume(radius, lp_norm, ndim) - target_volume,
    interval = c(0, 1)
  )$root

  discrete_prop_alt <- function(covariate_row) {
    covariate_row_selected <- covariate_row[selected_dimensions]
    ifelse(in_ball(covariate_row_selected, radius, lp_norm), signal_strength, 0)
  }

  return(discrete_prop_alt)
}

## --- flexible_prop_alt_sim---
#' Simulate alternative proportions flexibly
#'
#' @description This function is designed to perform multiple simulations
#' to calculate the proportion of alternative cases given various conditions.
#'
#' The function assumes the following model:
#' \deqn{
#'   \begin{aligned}
#'   &X_i \stackrel{\text{iid.}}{\sim} \operatorname{U}[-0.5,0.5]^d, \\
#'   &H_i \mid X_i \sim  \operatorname{Bernoulli}(1-\pi_0(X_i)),\\
#'   &P_i \mid H_i = 0, X_i \stackrel{\text{iid.}}{\sim} (1-\kappa) \operatorname{U}[0,1]+\kappa \operatorname{Beta}(1,0.5),\\
#'   &P_i \mid H_i = 1, X_i  \sim \operatorname{Beta}(0.25,1).
#'   \end{aligned}
#' }
#'
#' @param sim_parameters A data frame containing simulation parameters with the following columns:
#'   \describe{
#'     \item{seed}{A vector specifying the random seed for each simulation.}
#'     \item{dimensions}{A vector specifying the dimensionality of the covariates in the simulations.}
#'     \item{m}{A vector specifying the number of observations per simulation.}
#'     \item{kappa}{A vector specifying the mixing parameter for generating alternative proportions.}
#'   }
#' @param prop_alt_function_creator \pi_0(X_i) A function that creates alternative proportion functions, default is 'discrete_prop_alt_creator'.
#' @param prop_alt_function_name A string naming the proportion alternative function creator, default is "discrete_prop_alt_creator".
#' @param additional_arguments_prop_alt_function_creator A data frame that contains additional arguments needed for the prop_alt_function_creator function.
#' @return A list containing the simulation results for each combination of m, dimension, and seed, including additional arguments if provided.
#' @examples
#' # Simulate data
#' sim_results <- flexible_prop_alt_sim(
#'   sim_parameters = data.frame(
#'     seed = c(123, 456),
#'     dimensions = c(2, 3),
#'     m = c(100, 200),
#'     kappa = c(0.2, 0.3)
#'   )
#' )
#' # Extract the covariates and Hs values from the first simulation
#' Xs <- sim_results[[1]]$covariate
#' Hs <- sim_results[[1]]$Hs
#' # Define a title for the plot
#' title_list <- list(main = "My 2D Simulation", subtitle = "An example plot")
#' # Plot the 2D simulation using the plot_2d_simulation function
#' plot_2d_simulation(Xs, Hs, title_list)
#' @export
flexible_prop_alt_sim <- function(sim_parameters,
                                  prop_alt_function_creator = discrete_prop_alt_creator,
                                  prop_alt_function_name = "discrete_prop_alt_creator") {
  missing_cols <- setdiff(c("seed", "dimensions", "m", "kappa", "beta_shape1"), colnames(sim_parameters))
  if (length(missing_cols) > 0) {
    stop("The sim_parameters data frame is missing the following required columns: ", 
         paste(missing_cols, collapse = ", "), ".")
  }

  # Clean filter to remove invalid combinations
  if ("ndim" %in% colnames(sim_parameters)) {
    sim_parameters <- sim_parameters %>% filter(ndim <= dimensions)
  }
  # }

  # Add the proportion alternative function to each row of the dataframe
  # get a list of the function's formal arguments
  parameter_prop_alt <- formals(prop_alt_function_creator)
  parameter_prop_alt <- as.vector(names(parameter_prop_alt))
  intersection <- intersect(parameter_prop_alt, colnames(sim_parameters))
  sim_parameters_selected <- sim_parameters[, intersection]

  sim_parameters$prop_alt_function <- purrr::pmap(sim_parameters_selected, prop_alt_function_creator)

  # Apply simulation for each row in the combinations
  simple_sim <- lapply(seq_len(nrow(sim_parameters)), function(i) {
    # Assign m, dimension, and seed for the current iteration
    # Extract row i from the data.frame
    row_i <- sim_parameters[i, ]
    m_i <- sim_parameters$m[[i]]
    dimension_i <- sim_parameters$dimensions[[i]]
    seed_i <- sim_parameters$seed[[i]]
    prop_alt_function_i <- sim_parameters$prop_alt_function[[i]]
    kappa_i <- sim_parameters$kappa[[i]]
    beta_shape1_i <- sim_parameters$beta_shape1[[i]]

    set.seed(seed_i)
    # Generate covariates for the current simulation
    covariate_i <- matrix(runif(m_i * dimension_i, -0.5, 0.5), nrow = m_i)

    # Calculate the proportion of alternative cases
    prop_alt_i <- apply(covariate_i, 1, prop_alt_function_i)

    # Generate hypothesis tests
    Hs_i <- rbinom(m_i, size = 1, prob = prop_alt_i)

    #https://github.com/Huber-group-EMBL/covariate-powered-cross-weighted-multiple-testing/blob/master/IHWStatsPaper/R/betamix_simulations_functions.R#L27
    # Generate p-values
    pvalue_i <- ifelse(Hs_i,
      rbeta(m_i, beta_shape1_i, 1), # for Hs_i == TRUE
      (1 - kappa_i) * runif(m_i) + kappa_i * rbeta(m_i, 1, 0.5) # for Hs_i == FALSE
    )

    # Create a list to store the results of the current simulation
    res <- as.list(row_i)
    res$prop_alt_function <- NULL

    # Return results of the simulation as a list
    additional_elements <- list(
      covariate = covariate_i,
      prop_alt = prop_alt_i,
      Hs = Hs_i,
      pvalue = pvalue_i,
      prop_alt_function_name = prop_alt_function_name
    )

    res <- append(res, additional_elements)

    # Return the list of results for the current simulation
    return(res)
  })

  # Return all simulation results
  return(simple_sim)
}
