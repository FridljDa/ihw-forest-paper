#' Error handling for the fdp_eval function output
#'
#' This function takes as input the output of fdp_eval function
#' and checks if it inherits from 'try-error'. If so, it
#' returns a data frame with NA values, otherwise, it returns the original input.
#'
#' @param x Output from fdp_eval function
#' @return A data frame with columns `rjs`, `pow`, `FDP`, `FWER`. If the input has 'try-error' class, the data frame will be filled with NA values.
#' @export
#' @examples
#' save.seed <- .Random.seed
#' set.seed(1)
#' X <- runif(20000, min = 0, max = 2.5) # covariate
#' H <- rbinom(20000, 1, 0.1) # hypothesis true or false
#' Z <- rnorm(20000, H * X) # Z-score
#' .Random.seed <- save.seed
#' pvalue <- 1 - pnorm(Z) # pvalue
#' rjs <- p.adjust(pvalue, method = "BH") <= 0.1
#' error_fdp_table(fdp_eval(H, rjs)) # Expected output: data frame with fdp_eval output
error_fdp_table <- function(x) {
  if (inherits(x, "try-error")) {
    x <- data.frame(rjs = NA, pow = NA, FDP = NA, FWER = NA)
  }
  x
}


# Evaluate Multiple Testing Procedure
#
# This function calculates power, false discovery proportion, and family-wise error rate for the given hypotheses
# and rejections. If no hypotheses are provided, it only returns the total count of rejections.
#
# @param Hs A numeric vector indicating the true (1) and false (0) hypotheses. Defaults to NULL.
# @param rjs A numeric vector indicating the rejected hypotheses.
# @return A data frame with columns 'rjs' (total rejections), 'pow' (Power), 'FDP' (False Discovery Proportion), 'FWER' (Family-wise Error Rate). If no hypotheses are provided, only 'rjs' is returned.
# @export
#
# Examples:
# fdp_eval(rjs = c(0,1,1,1,0)) # only 'rjs' returned as no hypotheses are provided
# fdp_eval(Hs = c(1,1,0,0,1), rjs = c(0,1,1,1,0)) # 'rjs', 'pow', 'FDP', 'FWER' returned as hypotheses are provided
fdp_eval <- function(Hs = NULL, rjs) {
  rjs_total <- sum(rjs)
  if (is.null(Hs)) {
    data.frame(rjs = rjs_total)
  } else {
    pow <- sum(rjs * Hs) / max(1, sum(Hs))
    FDP <- sum(rjs * (1 - Hs)) / max(1, rjs_total)
    FWER <- sum((1 - Hs) * rjs) > 0
    data.frame(rjs = rjs_total, pow = pow, FDP = FDP, FWER = FWER)
  }
}

# Evaluate Multiple Testing Procedure with Error Handling
#
# This is a wrapper function for fdp_eval that includes error handling. The function attempts to execute the fdp_eval function
# and then applies the error_fdp_table function on the result, which could include an error.
#
# @param Hs A numeric vector indicating the true (1) and false (0) hypotheses. Defaults to NULL.
# @param rjs A numeric vector indicating the rejected hypotheses.
# @return The result of applying the error_fdp_table function on the fdp_eval result or the thrown error.
#
# Examples:
# fdp_eval_error_wrapper(rjs = c(0,1,1,1,0)) # Execute with only rejections vector
# fdp_eval_error_wrapper(Hs = c(1,1,0,0,1), rjs = c(0,1,1,1,0)) # Execute with both hypotheses and rejections vectors
fdp_eval_error_wrapper <- function(Hs = NULL, rjs) {
  error_fdp_table(try(fdp_eval(Hs, rjs)))
}


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
run_sim <- function(Ps, Xs, Hs = NULL, seed = NA, alpha = 0.1, forest_par = NULL, null_proportion = T, methods = c("IHW-quantile", "IHW-forest", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM"), folds = NULL) {  valid_methods <- c("IHW-quantile", "IHW-forest", "IHW-forest-drop-inbag", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM", "AdaPT-xgboost")
  invalid_methods <- setdiff(methods, valid_methods)

  if (length(invalid_methods) > 0) {
    stop(paste("The following methods are not valid:", paste(invalid_methods, collapse = ", "), ". Valid methods are:", paste(valid_methods, collapse = ", "), "."))
  }
  
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
  
  if ("IHW-forest-drop-inbag" %in% methods) {
    ihw_forest_drop_inbag_res <- fdp_eval_error_wrapper(Hs, ihw_forest_wrapper(Ps, Xs, alpha, forest_par, null_proportion = null_proportion, drop_inbag = TRUE, folds = folds))
    sim_res <- bind_rows(sim_res, mutate(ihw_forest_drop_inbag_res, method = "IHW-forest-drop-inbag"))
  }

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
  
  if(!is.null(Hs)) sim_res <- sim_res %>% mutate(
    pi0s = mean(1 - Hs)
  )
  
  sim_res <- sim_res %>% mutate(
    seed = seed,
    alpha = alpha,
    m = length(Ps)
  )
}
