ihw_quantile_wrapper <- function(Ps, Xs, alpha, null_proportion = T) {
  ihw_quantile <- IHW::ihw(Ps, Xs, alpha, stratification_method = "quantiles", null_proportion = null_proportion)
  IHW::rejected_hypotheses(ihw_quantile)
}

#' Wrapper for the IHW-Forest procedure
#'
#' @param Ps   Numeric vector of unadjusted p-values.
#' @param Xs   Vector or matrix of covariates
#' @param alpha    Significance level at which to apply method
#' @param Storey   Bool (default: FALSE): is the procedure pi0 adaptive or not?
#' @param forest_par  TODO
#'
#' @return         Binary vector of rejected/non-rejected hypotheses.
#'
#' @export
ihw_forest_wrapper <- function(Ps, Xs, alpha, forest_par, null_proportion = T) {
  ihw_forest <- IHW::ihw(Ps, Xs, alpha,
    stratification_method = "forest", null_proportion = null_proportion,
    ntrees = forest_par$ntrees, n_censor_thres = forest_par$n_censor_thres, nodedepth = forest_par$nodedepth,
    nodesize = forest_par$nodesize
  )
  IHW::rejected_hypotheses(ihw_forest)
}

# install.packages("devtools")
# devtools::install_github("ryurko/adaptMT")
## adaptMT::adapt_xgboost,
# adaptMT::adapt_xgboost_cv

# https://github.com/ryurko/AdaPT-GWAS-manuscript-code/blob/master/R/bmi/create_gtex_adapt_results.R#L68



#' Wrapper for AdaPT Wrapper devtools::install_github("ryurko/adaptMT")
#'
#' @param Ps   Numeric vector of unadjusted p-values.
#' @param Xs   Vector or matrix of covariates
#' @param alpha    Significance level at which to apply method
#'
#' @example
#' Xs <- runif(20000, min=0, max=2.5) # covariate
#' Hs <- rbinom(20000,1,0.1) # hypothesis true or false
#' Zs <- rnorm(20000, Xs*Hs) # Z-score
#' Ps <- 1-pnorm(Zs) # pvalue
#' adapt_xgboost_cv_wrapper(Ps, Xs)
#'
#' @return         Binary vector of rejected/non-rejected hypotheses.
#'
#' @export
adapt_xgboost_cv_wrapper <- function(Ps, Xs, alpha = 0.1,
                                     args_search = list("nrounds100md2" = list(
                                       "nrounds" = 15,
                                       "max_depth" = 3,
                                       # "min_child_weight" = 1,
                                       "verbose" = 0,
                                       "nthread" = 2
                                     ))) {
  res <- adaptMT::adapt_xgboost_cv(
    as.matrix(Xs),
    Ps,
    piargs = args_search,
    muargs = args_search,
    alphas = alphas
  )

  rejections <- rep(0, 20000)

  rejections[c(res$rejs)] <- 1
  rejections
}
