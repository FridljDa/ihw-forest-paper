#' Estimate prior probability with random forest 
#'
#' Hypotheses are stratified into bins based on random forest construction, alternative to \code{prior_prob_by_filter_multivariate}
#'   prior_prob are homogenous wrt to Storeys null proportion estimator
#'
#'  see \url{https://doi.org/10.7717/peerj.6035} for details on BocaLeek construction
#' @param pvalues Numeric vector of unadjusted p-values.
#' @param covariates Matrix which contains the covariates (independent under the H0 of the p-value) for each test.
#' @param folds Integer vector, Pre-specify assignment of hypotheses into folds.
#' @param ntrees Integer, see same parameter in \code{\link[randomForestSRC]{rfsrc}}
#' @param tau Double, censoring threshold tau of the pvalues in the stratification method "forest". See more in group_by_forest
#' @param nodedepth Integer, see same parameter in \code{\link[randomForestSRC]{rfsrc}}
#' @param nodesize Integer, see same parameter in \code{\link[randomForestSRC]{rfsrc}}
#' @param mtry Integer, see same parameter in \code{\link[randomForestSRC]{rfsrc}}
#'               Use "auto" for automatic selection.
#' @param seed Integer, specifies random seed to be used
#' @importFrom randomForestSRC rfsrc
#' @return prior probabilites
#' @examples
#'
#' save.seed <- .Random.seed
#' set.seed(1)
#' X <- runif(20000, min = 0, max = 2.5) # covariate
#' H <- rbinom(20000, 1, 0.1) # hypothesis true or false
#' Z <- rnorm(20000, H * X) # Z-score
#' folds <- sample(1:5, 20000, replace = TRUE)
#' .Random.seed <- save.seed
#' pvalue <- 1 - pnorm(Z) # pvalue
#' prior_prob <- estimate_prior_prob(pvalue, as.matrix(X), folds)
#' @export
estimate_prior_prob <- function(pvalues, covariates, folds, ntrees = 10, tau = 0.5, nodedepth = NULL, nodesize = 300, mtry = "auto", seed = NULL) {
  m <- length(pvalues)
  nfolds <- length(unique(folds))
  
  #if (mtry == "auto") mtry <- ceiling(0.9 * ncol(covariates)) # a lot of noise data => high mtry
  
  nodesize <- as.integer(nodesize)
  #mtry <- as.integer(mtry)
  nodedepth <- as.integer(nodedepth)
  
  prior_prob <- rep(NA, m)
  
  #prior_prob <- lapply(seq_len(nfolds), function(i) {
  for(i in seq_len(nfolds)){
    # binary indicator from Boca and leek/storey
    data <- data.frame(
      indic = (pvalues >= tau)/(1-tau),
      covariates = covariates
    )
    data_other_folds <- data[folds != i, ]
    data_hold_out_fold <- data[folds == i, ]
    
    # grow forest based on other folds
    forest_other_fold <- randomForestSRC::rfsrc(
      indic ~ . - indic,
      data = data_other_folds,
      ntree = ntrees,
      #mtry = mtry,
      nodesize = nodesize,
      nodedepth = nodedepth,
      splitrule = "mse",
      block.size = FALSE,
      forest.wt = FALSE,
      seed = seed
    )
    
    # predict terminal nodes for all covariates based on the forest structure
    predict_prior_prob <- randomForestSRC::predict.rfsrc(forest_other_fold, data_hold_out_fold)$predicted
    
    #prior_prob <- predict_prior_prob$predicted
    prior_prob[folds == i] <- predict_prior_prob
    
  }
  
  return(prior_prob)
}

#' Boca-Leek Wrapper Function for Prior Probability Estimation
#'
#' This function serves as a wrapper for the `estimate_prior_prob` function,
#' applying a random forest approach to estimate prior probabilities and adjust p-values
#' based on the Benjamini-Hochberg method. It then checks if the adjusted p-values are
#' below the given significance level alpha.
#'
#' @param pvalues Numeric vector of unadjusted p-values.
#' @param covariates Matrix which contains the covariates (independent under the H0 of the p-value) for each test.
#' @param alpha Double, significance level for the adjusted p-values.
#' @param nfolds Integer, number of folds to use for cross-validation (default is 5).
#' @param ntrees Integer, see same parameter in \code{\link[randomForestSRC]{rfsrc}} (default is 10).
#' @param tau Double, censoring threshold tau of the p-values in the stratification method "forest" (default is 0.5).
#' @param nodedepth Integer, see same parameter in \code{\link[randomForestSRC]{rfsrc}}.
#' @param nodesize Integer, see same parameter in \code{\link[randomForestSRC]{rfsrc}} (default is 300).
#' @param mtry Integer, see same parameter in \code{\link[randomForestSRC]{rfsrc}}. Use "auto" for automatic selection (default is "auto").
#' @param seed Integer, specifies the random seed to be used.
#' @return Logical vector indicating if the adjusted p-values are below the significance level alpha.
#' @importFrom randomForestSRC rfsrc
#' @importFrom stats p.adjust
#' @export
boca_leek_wrapper <- function(pvalues, covariates, alpha, nfolds = 5, ntrees = 10, tau = 0.5, nodedepth = NULL, nodesize = 300, mtry = "auto", seed = NULL) {
  
  # Sample the fold assignments for cross-validation
  folds <- sample(seq_len(nfolds), length(pvalues), replace = TRUE)
  
  # Estimate the prior probabilities using a random forest approach
  prior_prob <- estimate_prior_prob(pvalues, as.matrix(covariates), folds, ntrees = ntrees, tau = tau, nodedepth = nodedepth, nodesize = nodesize, mtry = mtry, seed = seed)
  
  # Adjust the p-values using the Benjamini-Hochberg method
  pvalue_adj <- p.adjust(pvalues, "BH")
  
  # Check if the adjusted p-values are below the significance level alpha
  prior_prob * pvalue_adj <= alpha
}
