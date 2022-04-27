sim <- wasserman_normal_sim(10000,0.85, 0, 3, seed=1)


sim$group <- as.factor(IHW:::groups_by_filter(sim$filterstat, 10))

set.seed(1)
expect_message(ihw_res1_single_fold <- ihw(sim$pvalue, sim$filterstat, .1, nbins=10, nfolds=1))

mgroups <- table(sim$group)
sim_filt <- subset(sim, sim$pvalue <= 0.5)

sim_replaced <- sim %>%
  dplyr::mutate(pvalue = ifelse(pvalue <= 0.5,
                         pvalue,
                         1))

ihw_res1_replaced_single_fold <- ihw(sim_replaced$pvalue, sim_replaced$group,.1, 
                                     nfolds=1, m_groups=mgroups)
set.seed(1)
ihw_res1_replaced_single_fold2 <- ihw(sim_replaced$pvalue, sim_replaced$filterstat,.1, 
                                     nfolds=1, nbins=10)

ihw_res1_filtered_single_fold <- ihw(sim_filt$pvalue, sim_filt$group,.1, 
                                     nfolds=1, m_groups=mgroups)

expect_equal(rejections(ihw_res1_single_fold), rejections(ihw_res1_filtered_single_fold))

rejections(ihw_res1_single_fold)
rejections(ihw_res1_filtered_single_fold)

rejections(ihw_res1_replaced_single_fold)

rejections(ihw_res1_replaced_single_fold2)

t1 <- thresholds(ihw_res1_filtered_single_fold, levels_only=T)
t2 <- thresholds(ihw_res1_single_fold, levels_only=T)
expect_equal(t1,t2)

#test sum groups