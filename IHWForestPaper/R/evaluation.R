#' Simulates and evaluates multiple testing procedures.
#'
#' @param Ps Vector of p-values.
#' @param Xs Vector of covariates.
#' @param Hs Vector with indicators of alternatives (1) and true nulls (0). Default is NULL.
#' @param seed Seed for reproducibility.
#' @param alpha Significance level. Default is 0.1.
#' @param forest_par Parameter for the forest method.
#' @param null_proportion Logical; if TRUE, estimates the null proportion. Default is TRUE.
#' @param methods Vector of methods to run. Default includes all valid methods: "IHW-quantile", "IHW-forest", "IHW-forest-drop-inbag", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM", "AdaPT-xgboost".
#' @return Data frame with columns for each method and associated statistics.
#' @import dplyr
#' @examples
#' save.seed <- .Random.seed
#' set.seed(1)
#' X <- runif(20000, min = 0, max = 2.5) # covariate
#' H <- rbinom(20000, 1, 0.1) # hypothesis true or false
#' Z <- rnorm(20000, H * X) # Z-score
#' .Random.seed <- save.seed
#' pvalue <- 1 - pnorm(Z) # pvalue
#' run_sim(pvalue, X, H, 1)
#' @export
run_sim <- function(Ps, Xs, Hs = NULL, seed = NULL, alpha = 0.1, forest_par = NULL, null_proportion = T, methods = c("IHW-quantile", "IHW-forest", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM"), folds = NULL, silent = FALSE) {
  valid_methods <- c("IHW-quantile", "IHW-forest", "IHW-forest-drop-inbag", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM", "AdaPT-xgboost")
  invalid_methods <- setdiff(methods, valid_methods)

  if (length(invalid_methods) > 0) {
    stop(paste("The following methods are not valid:", paste(invalid_methods, collapse = ", "), ". Valid methods are:", paste(valid_methods, collapse = ", "), "."))
  }
  #repr
  set.seed(seed)
  
  Xs <- as.matrix(Xs)
  sim_res <- data.frame()

  if ("IHW-quantile" %in% methods) {
    time_taken <- system.time({
      ihw_quantile_res <- fdp_eval_error_wrapper(Hs, ihw_quantile_wrapper(Ps, Xs, alpha, null_proportion = null_proportion, folds = folds))
    })
    sim_res <- bind_rows(sim_res, mutate(ihw_quantile_res, method = "IHW-quantile"))
    if(!silent) cat("Ran IHW-quantile. Time taken:", time_taken["elapsed"], "seconds\n")
  }

  if ("IHW-forest" %in% methods) {
    time_taken <- system.time({
      ihw_forest_res <- fdp_eval_error_wrapper(Hs, ihw_forest_wrapper(Ps, Xs, alpha, forest_par, null_proportion = null_proportion, drop_inbag = FALSE, folds = folds))
    })
    sim_res <- bind_rows(sim_res, mutate(ihw_forest_res, method = "IHW-forest"))
    if(!silent) cat("Ran IHW-forest. Time taken:", time_taken["elapsed"], "seconds\n")
  }
  
  if ("BH" %in% methods) {
    time_taken <- system.time({
      bh_res <- fdp_eval(Hs, p.adjust(Ps, method = "BH") <= alpha)
    })
    sim_res <- bind_rows(sim_res, mutate(bh_res, method = "BH"))
    if(!silent) cat("Ran BH. Time taken:", time_taken["elapsed"], "seconds\n")
  }
  
  if ("AdaPT" %in% methods) {
    time_taken <- system.time({
      adapt_res <- fdp_eval_error_wrapper(Hs, adapt_mtp(Ps, Xs, alpha))
    })
    sim_res <- bind_rows(sim_res, mutate(adapt_res, method = "AdaPT"))
    if(!silent) cat("Ran AdaPT. Time taken:", time_taken["elapsed"], "seconds\n")
  }
  
  if ("Boca-Leek" %in% methods) {
    time_taken <- system.time({
      boca_leek_res <- fdp_eval_error_wrapper(Hs, boca_leek_wrapper(Ps, as.data.frame(Xs), alpha))
    })
    sim_res <- bind_rows(sim_res, mutate(boca_leek_res, method = "Boca-Leek"))
    if(!silent) cat("Ran Boca-Leek. Time taken:", time_taken["elapsed"], "seconds\n")
  }
  
  if ("Clfdr-EM" %in% methods) {
    time_taken <- system.time({
      lfdr_em_res <- fdp_eval_error_wrapper(Hs, betamix_datadriven_lfdr(Ps, as.data.frame(Xs), alpha))
    })
    sim_res <- bind_rows(sim_res, mutate(lfdr_em_res, method = "Clfdr-EM"))
    if(!silent) cat("Ran Clfdr-EM. Time taken:", time_taken["elapsed"], "seconds\n")
  }
  
  if ("AdaPT-xgboost" %in% methods) {
    time_taken <- system.time({
      adapt_xgboost_res <- fdp_eval_error_wrapper(Hs, adapt_xgboost_wrapper(Ps, Xs))
    })
    sim_res <- bind_rows(sim_res, mutate(adapt_xgboost_res, method = "AdaPT-xgboost"))
    if(!silent) cat("Ran AdaPT-xgboost. Time taken:", time_taken["elapsed"], "seconds\n")
  }

  if (!is.null(Hs)) {
    sim_res <- sim_res %>% mutate(
      pi0s = mean(1 - Hs)
    )
  }

  sim_res <- sim_res %>% mutate(
    seed = seed,
    alpha = alpha,
    m = length(Ps)
  )
}

#' Evaluate Simulation Parallelly
#'
#' This function is designed to evaluate a list of simulations in parallel using a user-specified number of cores.
#' The simulations are run for each method given in the `methods` argument.
#'
#' @param simulation_list A list of simulations, where each simulation is a list containing:
#'   \itemize{
#'     \item \code{seed}: The seed value used for random number generation.
#'     \item \code{pvalue}: The p-values used in the simulation.
#'     \item \code{covariate}: The covariate values used in the simulation.
#'     \item \code{Hs}: Hypothesis status or any other relevant information required for the analysis.
#'     \item \code{alpha}: The alpha level used in the analysis.
#'     \item Other context information with a length of one (optional).
#'   }
#' @param methods Vector of methods to be used for multiple testing procedures.
#' @param forest_par Parameters for the forest method.
#' @param null_proportion Logical, if TRUE the proportion of null hypotheses is estimated.
#' @param folds The number of folds for cross-validation.
#' @return A list of results from running the simulations.
#' @export
#' @examples
#' sim_list <- list(
#'   list(seed = 12345, pvalue = c(0.05, 0.10, 0.20), covariate = c(1.2, 3.4, 5.6), Hs = c(TRUE, FALSE, TRUE), alpha = 0.05),
#'   list(seed = 67890, pvalue = c(0.01, 0.03, 0.05), covariate = c(2.3, 4.5, 6.7), Hs = c(FALSE, FALSE, TRUE), alpha = 0.1)
#' )
#' results <- eval_sim_parallel(simulation_list = sim_list, context_information = c("info1", "info2"))
eval_sim_parallel <- function(simulation_list,
                              methods = c("IHW-quantile", "IHW-forest", "IHW-forest-drop-inbag", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM"),
                              forest_par = NULL,
                              null_proportion = TRUE,
                              folds = NULL,
                              parallel = TRUE,
                              silent = FALSE) {
  
  required_items <- c("seed", "pvalue", "covariate", "Hs", "alpha")
  if (any(sapply(simulation_list, function(sim) !all(required_items %in% names(sim))))) {
    stop("All simulations in simulation_list must contain the following items: ", paste(required_items, collapse = ", "))
  }
  
  # Function to apply for each simulation
  eval_function <- function(i) {
    cat(paste0("simulation run:", i, "/", length(simulation_list), "\n"))
    sim_i <- simulation_list[[i]]
    seed_i <- sim_i$seed
    Ps_i <- sim_i$pvalue
    Xs_i <- sim_i$covariate
    Hs_i <- sim_i$Hs
    alpha_i <- sim_i$alpha
    
    sim_res_i <- run_sim(Ps_i,
                         Xs_i,
                         Hs = Hs_i,
                         seed = seed_i,
                         alpha = alpha_i,
                         forest_par = forest_par,
                         null_proportion = null_proportion,
                         methods = methods,
                         folds = folds,
                         silent = silent
    )
    
    # Add context information to sim_res_i
    if (!is.null(context_information)) {
      for (info in context_information) {
        sim_res_i[[info]] <- sim_i[[info]]
      }
    }
    
    return(sim_res_i)
  }
  
  # use sapply to get a logical vector indicating whether each element has length 1
  is_single_double <- sapply(simulation_list[[1]], function(x) is.numeric(x) && length(x) == 1)
  
  # use this logical vector to subset the names of the list
  context_information <- names(simulation_list[[1]])[is_single_double]
  
  # Determine if the computation should be run in parallel
  if (parallel) {
    n.cores <- parallel::detectCores()
    doParallel::registerDoParallel(cores = n.cores - 1)
    
    eval <- foreach(i = seq_along(simulation_list), .combine = rbind) %dorng% {
      eval_function(i)
    }
  } else {
    eval <- purrr::map_dfr(seq_along(simulation_list), eval_function)
  }
  
  return(eval)
}
