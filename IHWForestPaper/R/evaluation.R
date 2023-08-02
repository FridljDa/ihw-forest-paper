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
run_sim <- function(Ps, Xs, Hs = NULL, seed = NULL, alpha = 0.1, forest_par = NULL, null_proportion = T, methods = c("IHW-quantile", "IHW-forest", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM"), folds = NULL) {
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
    ihw_quantile_res <- fdp_eval_error_wrapper(Hs, ihw_quantile_wrapper(Ps, Xs, alpha, null_proportion = null_proportion, folds = folds))
    sim_res <- bind_rows(sim_res, mutate(ihw_quantile_res, method = "IHW-quantile"))
  }

  if ("IHW-forest" %in% methods) {
    ihw_forest_res <- fdp_eval_error_wrapper(Hs, ihw_forest_wrapper(Ps, Xs, alpha, forest_par, null_proportion = null_proportion, drop_inbag = FALSE, folds = folds))
    sim_res <- bind_rows(sim_res, mutate(ihw_forest_res, method = "IHW-forest"))
  }

  # if ("IHW-forest-drop-inbag" %in% methods) {
  #  ihw_forest_drop_inbag_res <- fdp_eval_error_wrapper(Hs, ihw_forest_wrapper(Ps, Xs, alpha, forest_par, null_proportion = null_proportion, drop_inbag = TRUE, folds = folds))
  #  sim_res <- bind_rows(sim_res, mutate(ihw_forest_drop_inbag_res, method = "IHW-forest-drop-inbag"))
  # }

  if ("BH" %in% methods) {
    bh_res <- fdp_eval(Hs, p.adjust(Ps, method = "BH") <= alpha)
    sim_res <- bind_rows(sim_res, mutate(bh_res, method = "BH"))
  }

  if ("AdaPT" %in% methods) {
    adapt_res <- fdp_eval_error_wrapper(Hs, adapt_mtp(Ps, Xs, alpha, formula_rhs = "~."))
    sim_res <- bind_rows(sim_res, mutate(adapt_res, method = "AdaPT"))
  }

  if ("Boca-Leek" %in% methods) {
    boca_leek_res <- fdp_eval_error_wrapper(Hs, boca_leek_wrapper(Ps, as.data.frame(Xs), alpha))
    sim_res <- bind_rows(sim_res, mutate(boca_leek_res, method = "Boca-Leek"))
  }

  if ("Clfdr-EM" %in% methods) {
    ## see https://github.com/Huber-group-EMBL/covariate-powered-cross-weighted-multiple-testing/blob/master/IHWStatsPaper/R/betamix_simulations_functions.R#L32
    lfdr_em_res <- fdp_eval_error_wrapper(Hs, betamix_datadriven_lfdr(Ps, as.data.frame(Xs), alpha, formula_rhs = "~."))
    sim_res <- bind_rows(sim_res, mutate(lfdr_em_res, method = "Clfdr-EM"))
  }

  if ("AdaPT-xgboost" %in% methods) {
    adapt_xgboost_res <- fdp_eval_error_wrapper(Hs, adapt_xgboost_wrapper(Ps, Xs))
    sim_res <- bind_rows(sim_res, mutate(adapt_xgboost_res, method = "AdaPT-xgboost"))
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
#' @param simulation_list A list of simulations.
#' @param alpha The alpha level used in the analysis.
#' @param methods Vector of methods to be used for multiple testing procedures.
#' @param forest_par Parameters for the forest method.
#' @param null_proportion Logical, if TRUE the proportion of null hypotheses is estimated.
#' @param folds The number of folds for cross-validation.
#' @return A list of results from running the simulations.
#' @export
#' @examples
#' sim_list <- list() # Add simulations to this list
#' results <- eval_sim_parallel(simulation_list = sim_list, context_information = c("info1", "info2"))
eval_sim_parallel <- function(simulation_list,
                              alpha = 0.1,
                              methods = c("IHW-quantile", "IHW-forest", "IHW-forest-drop-inbag", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM"),
                              forest_par = NULL,
                              null_proportion = TRUE,
                              folds = NULL) {
  
  # use sapply to get a logical vector indicating whether each element has length 1
  is_length_one <- sapply(simulation_list[[1]], function(x) length(x) == 1)
  
  # use this logical vector to subset the names of the list
  context_information <- names(simulation_list[[1]])[is_length_one]
  
  n.cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores = n.cores - 1)

  eval <- foreach(i = seq_along(simulation_list), .combine = rbind) %dorng% {
    cat(paste0("simulation run:", i, "/", length(simulation_list), "\n"))

    sim_i <- simulation_list[[i]]
    seed_i <- sim_i$seed
    Ps_i <- sim_i$pvalue
    Xs_i <- sim_i$covariate
    Hs_i <- sim_i$Hs

    sim_res_i <- run_sim(Ps_i,
      Xs_i,
      Hs = Hs_i,
      seed = seed_i,
      alpha = alpha,
      forest_par = forest_par,
      null_proportion = null_proportion,
      methods = methods,
      folds = folds
    )

    # Add context information to sim_res_i
    if (!is.null(context_information)) {
      for (info in context_information) {
        sim_res_i[[info]] <- sim_i[[info]]
      }
    }

    return(sim_res_i)
  }

  return(eval)
}
