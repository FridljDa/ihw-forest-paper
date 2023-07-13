error_fdp_table <- function(x) {
  if (inherits(x, "try-error")){
    x <- data.frame(rjs=NA, pow=NA, FDP=NA, FWER=NA)
  }
  x
}

#' Evaluate multiple testing procedure
#
#' @param Hs Vector with indicators of alternatives (1) and true nulls (0)
#' @param rjs Vector with indicator of rejected hypotheses
#' @return Data frame with columns `rjs` (total rejections), `pow` (Power), `FDP` (False discovery proportion)
#' @export
fdp_eval <- function(Hs, rjs){
  rjs_total <- sum(rjs)
  pow <- sum(rjs*Hs)/max(1,sum(Hs))
  FDP <- sum(rjs*(1-Hs))/max(1,rjs_total)
  FWER <- sum( (1-Hs)*rjs) > 0
  data.frame(rjs=rjs_total, pow=pow, FDP=FDP, FWER=FWER)
}


#' Apply multiple testing methods to the simulation with the continuous covariate.
#
#' @param seed     Integer; used for printing which simulation it running (does not set an actual RNG seed)
#' @param alpha Numeric (default: 0.1), nominal significance level at which to apply methods
#' @param m Number of hypotheses (default: m=10000)
#' @param lfdr_only Bool (default:FALSE), whether to run all methods (if FALSE) or only lfdr based methods (if TRUE)
#' @param forest_par TODO
#' @param null_proportion TODO
#'
#' @import dplyr 
#' @return Data frame with FDP and Power of different methods on this simulation.
#' @export
run_sim <- function(Ps, Xs, Hs, seed, alpha=0.1, m=10000, lfdr_only=FALSE, forest_par, null_proportion = T){
  ihw_quantile_res <-  error_fdp_table(try(fdp_eval(Hs, ihw_quantile_wrapper(Ps, Xs, alpha, null_proportion = null_proportion))))
  
  ihw_forest_res <-  error_fdp_table(try(fdp_eval(Hs, ihw_forest_wrapper(Ps, Xs, alpha, forest_par, null_proportion = null_proportion)))) 
  
  bh_res <- fdp_eval(Hs,  p.adjust(Ps, method="BH") <= alpha)
  
  sim_res <-  bind_rows(mutate(ihw_quantile_res, method="IHW-quantile"),
                        mutate(ihw_forest_res, method="IHW-forest"),
                        mutate(bh_res, method="BH"))
  
  if (!lfdr_only){
    adapt_res <-  error_fdp_table(try(fdp_eval(Hs, adapt_mtp(Ps, Xs, alpha, formula_rhs = "~."))))
    #adapt_xgboost_res <- error_fdp_table(try(fdp_eval(Hs, adapt_xgboost_cv_wrapper(Ps, Xs, alpha))))
    #adapt_xgboost_res <- error_fdp_table(try(fdp_eval(Hs, adapt_xgboost(Ps, Xs, alpha))))
    
    #see https://github.com/Huber-group-EMBL/covariate-powered-cross-weighted-multiple-testing/blob/master/IHWStatsPaper/R/betamix_simulations_functions.R#L32
    lfdr_em_res <- error_fdp_table(try(fdp_eval(Hs,  betamix_datadriven_lfdr(Ps, as.data.frame(Xs), alpha, formula_rhs = "~."))))
    boca_leek_res <- error_fdp_table(try(fdp_eval(Hs,  boca_leek_wrapper(Ps, as.data.frame(Xs), alpha))))

    sim_res <- bind_rows(sim_res,
                         mutate(adapt_res, method="AdaPT"),
                         mutate(boca_leek_res, method="Boca-Leek"),
                         #mutate(adapt_xgboost_res, method="AdaPT-xgboost"),
                         mutate(lfdr_em_res, method="Clfdr-EM")
                         )
  }
  mutate(sim_res,
         seed = seed,
         pi0s = mean(1-Hs),
         alpha=alpha,
         m = m)
}

run_ihw_forest <- function(Ps, Xs, Hs, seed, alpha=0.1, m=10000, lfdr_only=FALSE, forest_par, null_proportion = T){
  ihw_forest_res <-  error_fdp_table(try(fdp_eval(Hs, ihw_forest_wrapper(Ps, Xs, alpha, forest_par, null_proportion = null_proportion)))) 
  
  sim_res <-  mutate(ihw_forest_res, method="IHW-forest")
  
  mutate(sim_res,
         seed = seed,
         pi0s = mean(1-Hs),
         alpha=alpha,
         m = m)
}

#' Apply multiple testing methods to the simulation with the continuous covariate.
#
#' @param seed     Integer; used for printing which simulation it running (does not set an actual RNG seed)
#' @param alpha Numeric (default: 0.1), nominal significance level at which to apply methods
#' @param m Number of hypotheses (default: m=10000)
#' @param lfdr_only Bool (default:FALSE), whether to run all methods (if FALSE) or only lfdr based methods (if TRUE)
#' @param forest_par TODO
#' @param null_proportion TODO
#'
#' @import dplyr 
#' @return Data frame with FDP and Power of different methods on this simulation.
#' @export
run_sim_adapt <- function(Ps, Xs, Hs, seed, alpha=0.1, m=10000, lfdr_only=FALSE, forest_par, null_proportion = T){
  adapt_xgboost_cv_res <- error_fdp_table(try(fdp_eval(Hs, adapt_xgboost_cv_wrapper(Ps, Xs, alpha))))
  adapt_xgboost_res <- error_fdp_table(try(fdp_eval(Hs, adapt_xgboost(Ps, Xs, alpha))))
  
  sim_res <-  bind_rows(mutate(adapt_xgboost_cv_res, method="AdaPT-xgboost-cv"),
                        mutate(adapt_xgboost_res, method="AdaPT-xgboost"))
  mutate(sim_res,
         seed = seed,
         pi0s = mean(1-Hs),
         alpha=alpha,
         m = m)
}

run_ihw_forest <- function(Ps, Xs, Hs, seed, alpha=0.1, m=10000, lfdr_only=FALSE, forest_par, null_proportion = T){
  ihw_forest_res <-  error_fdp_table(try(fdp_eval(Hs, ihw_forest_wrapper(Ps, Xs, alpha, forest_par, null_proportion = null_proportion)))) 
  
  sim_res <-  mutate(ihw_forest_res, method="IHW-forest")
  
  mutate(sim_res,
         seed = seed,
         pi0s = mean(1-Hs),
         alpha=alpha,
         m = m)
}