#' clfdr: Cai's local fdr based method
#'
#'
#' @param unadj_p  Numeric vector of unadjusted p-values.
#' @param groups   Factor to which different hypotheses belong
#' @param alpha    Significance level at which to apply method
#' @param lfdr_estimation  Method used to estimate the loca fdr, defaults to "fdrtool"
#'
#' @return Clfdr multiple testing object
#'
#' @examples
#'      sim_df <- du_ttest_sim(20000,0.95, 1.5)
#'      sim_df$group <- groups_by_filter(sim_df$filterstat, 20)
#'      obj <- clfdr(sim_df$pvalue, sim_df$group, .1)
#'      sum(rejected_hypotheses(obj))
#'
#' @references Cai, T. Tony, and Wenguang Sun. "Simultaneous testing of grouped hypotheses: Finding needles in multiple haystacks." 
#'           Journal of the American Statistical Association 104.488 (2009).
#' @export
clfdr <- function(unadj_p, groups, alpha, lfdr_estimation="fdrtool"){
  
  # estimate local fdr within each stratum first
  
  lfdr_res <- lfdr_fit(unadj_p, groups, lfdr_estimation=lfdr_estimation)
  lfdrs <- lfdr_res$lfdr
  
  # now use the rejection rule described in Cai's paper
  
  # Remark:
  # When sorting lfdrs, we break ties by pvalues so that in the end within each stratum
  # we get monotonic adjusted p-values as a function of the p-values
  # This is mainly needed for grenander based lfdrs, with most other
  # lfdr estimation methods lfdr ties are not a problem usually
  
  o <- order(lfdrs, unadj_p)
  lfdrs_sorted <- lfdrs[o]
  fdr_estimate <- cumsum(lfdrs_sorted)/(1:length(unadj_p))
  adj_p <- rev(cummin(rev(fdr_estimate)))
  adj_p <- adj_p[order(o)]
  
  
  obj <- list(adj_p = adj_p, alpha=alpha, lfdr_estimation=lfdr_estimation)
  class(obj) <- "Clfdr"
  obj
}

attr(clfdr, "testing covariate") <- "stratified" # i.e. covariates can be considered by stratifying based on them
attr(clfdr, "fdr_method")        <- "Clfdr"

rejected_hypotheses.Clfdr <- function(object, alpha= object$alpha){
  object$adj_p <= alpha
}

setOldClass("Clfdr")
#setMethod("rejected_hypotheses", signature("Clfdr"), rejected_hypotheses.Clfdr)

