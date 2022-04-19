ihw_quantile_wrapper <- function(Ps, Xs, alpha, folds){
  ihw_quantile <- ihw(Ps, Xs, .1, stratification_method = "quantiles", folds = folds, null_proportion = T)
  rejected_hypotheses(ihw_quantile)
}

ihw_forest_wrapper <- function(Ps, Xs, alpha, folds, forest_par){
  ihw_forest <- ihw(Ps, Xs, .1, stratification_method = "forest", folds = folds, null_proportion = T,
                    ntrees = forest_par$ntrees, ntaus = forest_par$ntaus, maxdepth = forest_par$maxdepth, 
                    nodesize = forest_par$nodesize, nsplit = forest_par$nsplit)
  rejected_hypotheses(ihw_forest)
}

error_fdp_table <- function(x) {
  if (inherits(x, "try-error")){
    x <- data.frame(rjs=NA, pow=NA, FDP=NA, FWER=NA)
  }
  x
}


#' Apply multiple testing methods to the simulation with the continuous covariate.
#
#' @param seed     Integer; used for printing which simulation it running (does not set an actual RNG seed)
#' @param alpha Numeric (default: 0.1), nominal significance level at which to apply methods
#' @param m Number of hypotheses (default: m=10000)
#' @param lfdr_only Bool (default:FALSE), whether to run all methods (if FALSE) or only lfdr based methods (if TRUE)
#'
#' @return Data frame with FDP and Power of different methods on this simulation.
#' @export
run_sim <- function(Ps, Xs, Hs, folds, seed, alpha=0.1, m=10000, lfdr_only=FALSE, forest_par){
  ihw_quantile_res <-  error_fdp_table(try(fdp_eval(Hs, ihw_quantile_wrapper(Ps, Xs, alpha, folds))))
  
  ihw_forest_rjs <- ihw_forest_wrapper(Ps, Xs, alpha, folds, forest_par)
  ihw_forest_res <- fdp_eval(Hs, ihw_forest_rjs)
  
  bh_res <- fdp_eval(Hs,  p.adjust(Ps, method="BH") <= alpha)
  
  sim_res <-  bind_rows(mutate(ihw_quantile_res, method="IHW-quantile"),
                        mutate(ihw_forest_res, method="IHW-forest"),
                        mutate(bh_res, method="BH"))

  if (!lfdr_only){
    adapt_res <-  error_fdp_table(try(fdp_eval(Hs, adapt_mtp(Ps, Xs, alpha, formula_rhs = "~."))))
    #lfdr_oracle_res <- fdp_eval(Hs,  oracle_local_fdr_test(Ps, oracle_lfdrs, alpha))
    #lfdr_em_res <- error_fdp_table(try(fdp_eval(Hs,  betamix_datadriven_lfdr(Ps, Xs, alpha))))
    
    #sabha_res <-fdp_eval(Hs,  groupwise_sabha(Ps, Xs, alpha)) 
    #sbh_res <- fdp_eval(Hs,  stratified_bhq(Ps, Xs, alpha)) 
    #TODO adaptMT::adapt_xgboost, 
    #adaptMT::adapt_xgboost_cv
    #see https://github.com/FridljDa/ihw-forest-paper/blob/main/depreaceated/IHW_master/experimental-vigniettes/2022-03/2022-03-23_benchmark_forests.Rmd#L142
   
    sim_res <- bind_rows(sim_res,
                         mutate(adapt_res, method="AdaPT")#,
                         #mutate(lfdr_oracle_res, method="Clfdr-oracle"),
                         #mutate(lfdr_em_res, method="Clfdr-EM"),
                         #mutate(sabha_res , method="SABHA"),
                         #mutate(sbh_res , method="SBH")
                         )
  }
  mutate(sim_res,
         seed = seed,
         pi0s = mean(1-Hs),
         alpha=alpha,
         m = m)
}