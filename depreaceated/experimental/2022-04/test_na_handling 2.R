wasserman_normal_sim <- function(m, pi0, xi_min, xi_max, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  X <- runif(m, min = xi_min, max = xi_max)
  H <- rbinom(m, 1, 1 - pi0)
  Z <- rnorm(m, H * X)
  pvalue <- 1 - pnorm(Z)
  simDf <- data.frame(pvalue = pvalue, filterstat = X, H = H, Z = Z)
}
sim <- wasserman_normal_sim(10000, 0.85, 0, 3, seed = 1)

sim$group <- as.factor(IHW:::groups_by_filter(sim$filterstat, 10))
mgroups <- table(sim$group)

hist(sim$pvalue)

sim_filt <- subset(sim, sim$pvalue <= 0.5)

sim_replaced <- sim %>%
  dplyr::mutate(pvalue = ifelse(pvalue <= 0.5,
    pvalue,
    NA
  ))

ihw_res_full <- ihw(sim$pvalue, sim$filterstat, 0.1, nfolds = 1)
ihw_res_filt <- ihw(sim_filt$pvalue, sim_filt$group, 0.1, nfolds = 1, m_groups = mgroups)#
ihw_res_replaced <- ihw(sim$pvalue, sim$filterstat, 0.1, nfolds = 1)


rejections(ihw_res_full)
rejections(ihw_res_filt)
rejections(ihw_res_replaced)
