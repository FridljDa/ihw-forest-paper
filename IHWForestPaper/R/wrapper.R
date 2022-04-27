ihw_quantile_wrapper <- function(Ps, Xs, alpha){
  #TODO nfold?
  ihw_quantile <- IHW::ihw(Ps, Xs, .1, stratification_method = "quantiles", null_proportion = T)
  IHW::rejected_hypotheses(ihw_quantile)
}

ihw_forest_wrapper <- function(Ps, Xs, alpha, forest_par){
  ihw_forest <- IHW::ihw(Ps, Xs, .1, stratification_method = "forest", null_proportion = T,
                         ntrees = forest_par$ntrees, n_censor_thres = forest_par$n_censor_thres, maxdepth = forest_par$maxdepth, 
                         nodesize = forest_par$nodesize, nsplit = forest_par$nsplit)
  IHW::rejected_hypotheses(ihw_forest)
}

# install.packages("devtools")
#devtools::install_github("ryurko/adaptMT")
adapt_xgboost_cv_wrapper <- function(Ps, Xs, alpha){
  Xs <- runif(20000, min=0, max=2.5) # covariate
  Hs <- rbinom(20000,1,0.1) # hypothesis true or false
  Zs <- rnorm(20000, Xs*Hs) # Z-score

  Ps <- 1-pnorm(Zs) # pvalue
  
  args_search <- list("nrounds100md2" = list("nrounds" = 5,
                                             "max_depth" = 2,
                                             #"min_child_weight" = 1,
                                             "verbose" = 0,
                                             "nthread" = 2))
  
  res <- adaptMT::adapt_xgboost_cv(
    as.matrix(Xs),
    Ps,
    verbose = list(print = FALSE,
                   fit = FALSE,
                   ms = FALSE),
    piargs = args_search,
    muargs = args_search,
    n_folds = 5,
    niter_ms = 10,
    nms = as.integer(2)
    #s0 = rep(0.05, nrow(bmi_esnps_data))
  )
  #adaptMT::adapt_xgboost, 
  #adaptMT::adapt_xgboost_cv
  #TODO 
  #https://github.com/ryurko/AdaPT-GWAS-manuscript-code/blob/master/R/bmi/create_gtex_adapt_results.R#L68
}
