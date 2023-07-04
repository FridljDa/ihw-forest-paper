ihw_quantile_wrapper <- function(Ps, Xs, alpha, per_covariate_bins = 5, null_proportion = T) {
  number_covariates <- ncol(Xs)
  
  ihw_quantile <- IHW::ihw(Ps, 
                           Xs, 
                           alpha, 
                           nbins = per_covariate_bins^number_covariates,
                           stratification_method = "quantiles", 
                           null_proportion = null_proportion)
  
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
ihw_forest_wrapper <- function(Ps, Xs, alpha, forest_par, null_proportion = T, per_covariate_bins = 5) {
  number_covariates <- ncol(Xs)
  
  #nodedepth <- number_covariates * log2(per_covariate_bins)
  nbins_quantile <- per_covariate_bins^number_covariates
  nodesize <- ceiling(length(Ps)/nbins_quantile)
    
  ihw_forest <- IHW::ihw(Ps, Xs, alpha,
    stratification_method = "forest", null_proportion = null_proportion,
    ntrees = forest_par$ntrees, 
    tau = forest_par$tau, 
    #nodedepth = forest_par$nodedepth,
    #nodesize = forest_par$nodesize, 
    #nodedepth = nodedepth,
    nodesize = nodesize,
    lambdas = Inf
  )
  
  IHW::rejected_hypotheses(ihw_forest)
}

#' Local false discovery rate procedure with local fdrs estimated from the Betamix-model through the EM algorithml
#'
#' @param Ps     Numeric vector of unadjusted p-values.
#' @param Xs     Data frame with features
#' @param alpha  Significance level at which to apply method
#' @param formula_rhs Formula defining the RHS in the fitted GLMs, defaults to ~X1+X2 (used in simulations herein).
#' @param maxiter  Total number of iterations to run the EM algorithm
#'
#' @return Binary vector of rejected/non-rejected hypotheses.
#' @export
betamix_datadriven_lfdr <- function(Ps, Xs, alpha, formula_rhs="~X1+X2", maxiter=200,...){
  gamma_glm_fit  <- gamma_glm_basic_em(Ps, Xs, formula_rhs=formula_rhs, maxiter = maxiter, tau_pi0=0.5,...)
  betamix_oracle_lfdr(Ps, gamma_glm_fit$pi1s, gamma_glm_fit$alphas, alpha)
}

#' Simulation: Misspecified conditional Beta-uniform mixture model
#
#' @param m Number of hypotheses (default: m=10000)
#' @param mus_slope Numeric (default:1.5) parameter bar(beta) in equation (12)
#' @param one_sided_tests Bool (default:FALSE), if true adds some nulls that are strictly superuniform
#' @param prob_one_sided Numeric (default:0.25) proportion of nulls that are strictly superuniform
#'
#' @return Data frame with columns `Hs` (null or alternative), `Ps` (p-value), `Xs` (side-information),
#'         `alphas` (parameters of alternative distribution), `pi1s` (probabilities of being from the alternative distribution),
#'         `oracle_lfdr` (oracle local fdr)
#' @export
beta_unif_sim <- function(m=10000, mus_slope=1.5, one_sided_tests=FALSE, prob_one_sided=0.25){
  Xs <- matrix(runif(m*2, 0,1), ncol=2)
  colnames(Xs) <- c("X1", "X2")
  
  pi1s <- ifelse( Xs[,1]^2 + Xs[,2]^2 <= 1, 0.02, 0.4)
  mus <- pmax(1.3, sqrt(Xs) %*% c(1,1)*mus_slope)
  
  mu_alphas <- 1/mus
  
  Hs <- stats::rbinom(m, size=1, prob=pi1s)
  Ps <- stats::runif(m)*(1-Hs) + stats::rbeta(m, mu_alphas, 1)*Hs
  Xs <- data.frame(Xs)
  if (one_sided_tests){
    Hs_alt <-  1- (1-Hs)*stats::rbinom(m, size=1, prob=prob_one_sided)
    Ps[Hs_alt == 0] <- stats::rbeta(sum(Hs_alt == 0), 1, 0.5)
    oracle_lfdr_null <- (1-pi1s)*( 1-prob_one_sided + prob_one_sided*stats::dbeta(Ps, 1, 0.5) )
  } else{
    oracle_lfdr_null <- 1-pi1s
  }
  oracle_lfdr_alternative <- pi1s*dbeta(Ps, mu_alphas, 1)
  oracle_lfdr <- oracle_lfdr_null/(oracle_lfdr_null+oracle_lfdr_alternative)
  list(Xs=Xs, Ps=Ps, Hs=Hs, alphas=mu_alphas, pi1s=pi1s, oracle_lfdrs=oracle_lfdr)
}
