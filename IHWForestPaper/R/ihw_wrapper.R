#' @import R.utils
ihw_quantile_wrapper <- function(Ps, Xs, alpha, per_covariate_bins = 5, null_proportion = T, timeout = 300) {
  #number of covariates
  number_covariates <- ncol(Xs)

  ihw_quantile <- tryCatch(
    {
      R.utils::withTimeout(
        {
          IHW::ihw(Ps, 
                   Xs, 
                   alpha, 
                   nbins = per_covariate_bins^number_covariates,
                   stratification_method = "quantiles", 
                   null_proportion = null_proportion)
        },
        timeout = timeout
      )
    },
    error = function(e) {
      NULL
    }
  )
  
  if (isS4(ihw_quantile)) {
    IHW::rejected_hypotheses(ihw_quantile)
  } else {
    NA
  }
}

#' Wrapper for the IHW-Forest procedure
#'
#' @param Ps   Numeric vector of unadjusted p-values.
#' @param Xs   Vector or matrix of covariates
#' @param alpha    Significance level at which to apply method
#' @param Storey   Bool (default: FALSE): is the procedure pi0 adaptive or not?
#' @param forest_par  TODO
#' @param drop_inbag do not use hypotheses, which have already been used for the 
#'        construction of the partition for the construction of the weights as well
#' @return         Binary vector of rejected/non-rejected hypotheses.
#'
#' @export
ihw_forest_wrapper <- function(Ps, Xs, alpha, forest_par, null_proportion = T, per_covariate_bins = 5, drop_inbag = FALSE) {
  #number_covariates <- ncol(Xs)
  
  #nodedepth <- number_covariates * log2(per_covariate_bins)
  #nbins_quantile <- per_covariate_bins^number_covariates
  #nodesize <- ceiling(length(Ps)/nbins_quantile)
  ntrees <-forest_par$ntrees
  if(drop_inbag) ntrees <- 10*ntrees

  ihw_forest <- IHW::ihw(Ps, Xs, alpha,
    stratification_method = "forest", null_proportion = null_proportion,
    ntrees = forest_par$ntrees, 
    tau = forest_par$tau, 
    #nodedepth = forest_par$nodedepth,
    nodesize = forest_par$nodesize, 
    #nodedepth = nodedepth,
    #nodesize = nodesize,
    lambdas = Inf,
    drop_inbag = drop_inbag
  )
  
  IHW::rejected_hypotheses(ihw_forest)
}

