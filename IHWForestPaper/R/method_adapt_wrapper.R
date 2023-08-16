#' Wrapper of AdaPT multiple testing procedure
#' @param Ps       Numeric vector of p-values
#' @param Xs       Data frame with features
#'
#' @param alpha    Nominal testing level
#' @param return_fit  Boolean, whether to return the fitted adapt object (or only the indicator of rejections), defaults to false
#' @importFrom adaptMT adapt_glm
#' @return Binary vector of rejected/non-rejected hypotheses.
#' @seealso \url{https://github.com/Huber-group-EMBL/covariate-powered-cross-weighted-multiple-testing/blob/master/IHWStatsPaper/R/adapt_wrapper.R}
#'
#' @references AdaptMT CRAN package
#' @export
adapt_mtp <- function(Ps, Xs, alpha, return_fit=FALSE){
  Xs <- as.data.frame(Xs)
  formula_rhs <- create_formula(Xs)
  
  #To avoid all models failing, we increase the starting regime of p-values we can see
  s0 <- 0.45
  while(0.3 >= sum(dplyr::between(Ps, s0, 1-s0))/length(Ps)){
    s0 <- s0 - 0.05
  }

  adapt_glm_fit <- adaptMT::adapt_glm(Xs, 
                                      Ps, 
                                      formula_rhs, 
                                      formula_rhs, 
                                      alphas = alpha,
                                      s0 = rep(s0, length(Ps)))
  adapt_glm_rjs <- adapt_glm_fit$qvals <= alpha
  if (return_fit){
    return(list(rjs=adapt_glm_rjs, fit=adapt_glm_fit))
  } else {
    return(adapt_glm_rjs)
  }
}


# install.packages("devtools")
# devtools::install_github("ryurko/adaptMT")
## adaptMT::adapt_xgboost,
# adaptMT::adapt_xgboost_cv

# https://github.com/ryurko/AdaPT-GWAS-manuscript-code/blob/master/R/bmi/create_gtex_adapt_results.R#L68

#devtools::load_all(""IHWForestPaper/adaptMT")
#' Wrapper for AdaPT Wrapper devtools::install_github("ryurko/adaptMT")
#'
#' @param Ps   Numeric vector of unadjusted p-values.
#' @param Xs   Vector or matrix of covariates
#' @param alpha    Significance level at which to apply method
#'
#' Xs <- runif(20000, min=0, max=2.5) # covariate
#' Hs <- rbinom(20000,1,0.1) # hypothesis true or false
#' Zs <- rnorm(20000, Xs*Hs) # Z-score
#' Ps <- 1-pnorm(Zs) # pvalue
#' adapt_xgboost_cv_wrapper(Ps, Xs)
#'
#' @return         Binary vector of rejected/non-rejected hypotheses.
#'
adapt_xgboost_cv_wrapper <- function(Ps, Xs, alphas = 0.1,
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

#devtools::load_all("IHWForestPaper/adaptMT")
#' Wrapper for AdaPT Wrapper devtools::install_github("ryurko/adaptMT")
#' @importFrom adaptMT adapt_xgboost
#' @param Ps   Numeric vector of unadjusted p-values.
#' @param Xs   Vector or matrix of covariates
#' @param alpha    Significance level at which to apply method
#' @examples 
#' Xs <- runif(20000, min=0, max=2.5) # covariate
#' Hs <- rbinom(20000,1,0.1) # hypothesis true or false
#' Zs <- rnorm(20000, Xs*Hs) # Z-score
#' Ps <- 1-pnorm(Zs) # pvalue
#' adapt_xgboost_wrapper(Ps, Xs)
#'
#' @return         Binary vector of rejected/non-rejected hypotheses.
#'
adapt_xgboost_wrapper <- function(Ps, 
                                  Xs, 
                                  alpha = 0.1) {

  res <- adapt_xgboost(as.matrix(Xs), 
                       Ps, 
                       alphas = alpha, 
                       nfits = 5)
  
  
  rejection_position <- res[["rejs"]]
  rejections <- rep(0, length(Ps))
  rejections[rejection_position] <- 1
  rejections
}
