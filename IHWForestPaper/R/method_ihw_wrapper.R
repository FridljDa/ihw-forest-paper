#' Wrapper for the IHW-Quantile procedure
#'
#' This function applies the IHW-Quantile procedure to a given set of p-values and covariates.
#'
#' @param Ps Numeric vector of unadjusted p-values.
#' @param Xs Vector or matrix of covariates.
#' @param alpha Significance level at which to apply method.
#' @param per_covariate_bins Number of bins for the covariate stratification. Default is 5.
#' @param null_proportion Logical; if TRUE, estimates the null proportion. Default is TRUE.
#' @param timeout Maximum amount of time (in seconds) for the procedure to run. Default is 300.
#' @param folds The number of folds for cross-validation. Default is NULL.
#' @return Binary vector of rejected/non-rejected hypotheses. If the procedure fails or times out, returns NA.
#' @import IHW
#' @examples
#' # Generate some example data
#' Ps <- runif(100)  # P-values
#' Xs <- rnorm(100)  # Covariates
#' # Run the IHW-Quantile procedure
#' rejections <- ihw_quantile_wrapper(Ps, Xs, alpha = 0.05)
#' @importFrom R.utils withTimeout
#' @export
ihw_quantile_wrapper <- function(Ps, Xs, alpha, per_covariate_bins = 5, null_proportion = T, timeout = 300, folds = NULL) {
  
  # Determine the number of covariates
  number_covariates <- if(is.matrix(Xs) || is.data.frame(Xs)) ncol(Xs) else 1
  nbins <- per_covariate_bins^number_covariates
  
  #if too many bins for the observations, abort
  if(nbins >= 2*length(Ps)) return(NA)
  
  # Run the IHW-Quantile procedure, catching any errors and setting a timeout
  ihw_quantile <- tryCatch(
    {
      R.utils::withTimeout(
        {
          IHW::ihw(Ps, 
                   Xs, 
                   alpha, 
                   nbins = nbins,
                   stratification_method = "quantiles", 
                   null_proportion = null_proportion,
                   folds = folds)
        },
        timeout = timeout
      )
    },
    error = function(e) {
      NULL
    }
  )
  
  # If the procedure completed successfully, return the vector of rejections; otherwise, return NA
  if (isS4(ihw_quantile)) {
    return(IHW::rejected_hypotheses(ihw_quantile))
  } else {
    return(NA)
  }
}

#' Wrapper for the IHW-Forest procedure
#'
#' This function applies the IHW-Forest procedure to a given set of p-values and covariates.
#' 
#' @param Ps   Numeric vector of unadjusted p-values.
#' @param Xs   Vector or matrix of covariates.
#' @param alpha Significance level at which to apply method.
#' @param forest_par List of parameters for the forest stratification method.
#' @param null_proportion Logical; if TRUE, estimates the null proportion. Default is TRUE.
#' @param per_covariate_bins Number of bins for the covariate stratification. Default is 5.
#' @param drop_inbag Logical; if TRUE, does not use hypotheses, which have already been used for the construction of the partition for the construction of the weights as well. Default is FALSE.
#' @param folds The number of folds for cross-validation. Default is NULL.
#' @return Binary vector of rejected/non-rejected hypotheses.
#' @import IHW
#' @examples
#' # Generate some example data
#' Ps <- runif(100)  # P-values
#' Xs <- rnorm(100)  # Covariates
#' # Run the IHW-Forest procedure
#' rejections <- ihw_forest_wrapper(Ps, Xs, alpha = 0.05)
#' @export
ihw_forest_wrapper <- function(Ps, Xs, alpha, forest_par = NULL, null_proportion = TRUE, per_covariate_bins = 5, drop_inbag = FALSE, folds = NULL) {
  if(is.null(forest_par)){
    ihw_forest <- IHW::ihw(Ps, Xs, alpha,
                           stratification_method = "forest", 
                           null_proportion = null_proportion,
                           lambdas = Inf,
                           folds = folds
    )
  }else{
    ihw_forest <- IHW::ihw(Ps, Xs, alpha,
                           stratification_method = "forest", 
                           null_proportion = null_proportion,
                           ntrees = forest_par$ntrees, 
                           tau = forest_par$tau, 
                           nodesize = forest_par$nodesize, 
                           lambdas = Inf,
                           folds = folds
    )
  }
  
  IHW::rejected_hypotheses(ihw_forest)
}
