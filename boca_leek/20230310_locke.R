#analyse locke data set 
library(ggplot2)
library(here)
library(dplyr)

library(doRNG)
library(doParallel)
library(parallel)

set.seed(4)
options(bitmapType ="cairo")

registerDoParallel(cores=6)

#load IHW
#devtools::load_all("../../IHW")
devtools::load_all("~/R/IHW")

#library(IHW)

#load Locke data set 
load(here("boca_leek/BMI_GIANT_GWAS.RData"))

#----handler--------
error_handler <- function(x) { # TODO
  if (inherits(x, "try-error")) {
    x <- NA
  }
  x
}

rejections_na <- function(ihw) { # TODO
  if (is.na(ihw)) {
    return(NA)
  } else {
    rejections(ihw)
  }
}

#formulas
formulas_mt <- c(
  #"p ~ Freq_MAF_Hapmap",
  #"p ~ N",
  "p ~ Freq_MAF_Hapmap + N",
  "p ~ Freq_MAF_Hapmap + N +minor_allele_freq+chrom_start"
)

formulas_mt <- sapply(formulas_mt, as.formula)

#-----parameters for runs-----

parameters_run_quantile <- expand.grid(
  formulas = formulas_mt,
  alphas = seq(0.01, 0.1, length.out = 4),
  lambdas = Inf,
  stratification_method = c("quantiles"),
  #parameters for quantile
  nbins_quantile = 2^5, 
  #parameters for forest 
  nodedepth_forest = NA,
  n_censor_thres = NA, 
  ntrees = NA,  
  nodesize = NA
)

parameters_run_forest <- expand.grid(
  formulas = formulas_mt,
  alphas = seq(0.01, 0.1, length.out = 4),
  lambdas = Inf,
  stratification_method = "forest",
  #parameters for quantile
  nbins_quantile = "auto", 
  #parameters for forest 
  nodedepth_forest = 5,
  n_censor_thres = 3, 
  ntrees = 3,  
  nodesize = 3000
)

parameters_run <- rbind(parameters_run_quantile, parameters_run_forest)

#merge
parameters_run <- parameters_run %>%
  mutate(
    number_covariates = 1 + stringr::str_count(formulas, "\\+"),
    formula_string = as.character(parameters_run$formulas), 
    stratification_method = as.character(stratification_method)
  )

##----- benjamini hochberg ------
# run for all alpha-formula combinations (in parallel)
alpha_levels <- parameters_run %>%
  select(alphas) %>%
  distinct()

bh_rejections <- foreach(i = seq_len(nrow(alpha_levels))) %dorng% { 
  print(paste0("run:", i))
  
  alpha_i <- alpha_levels$alphas[[i]]
  
  bh_rejections_i <- sum(p.adjust(na.exclude(BMI_GIANT_GWAS$p), method = "BH") <= alpha_i)
  # evaluate number of rejections
  data.frame(
    method = "BH",
    formula = "pvalue ~ 1",
    number_covariates = 0,
    alpha = alpha_i,
    rejections = bh_rejections_i,
    nbins_covariates = 0
  )
}
bh_rejections <- bind_rows(bh_rejections)
head(bh_rejections)

##-------ihw--------
# parameters_run <- parameters_run[2, ]
# only rerun, if changes to global variables or code
#ihw_rej <- xfun::cache_rds(
#  {
# run for all alpha-formula combinations (in parallel)
ihw_rej <- foreach(i = seq_len(nrow(parameters_run))) %dorng% {
  #i <- 27
  print(paste0("run:", i, "/", nrow(parameters_run)))
  #cat("run:", i)
  
  nbins = parameters_run$nbins_quantile[[i]]
  if(nbins != "auto") nbins <- as.numeric(nbins)
  
  ihw_i <- error_handler(try(
    ihw(
      formula = parameters_run$formulas[[i]],
      #formula = as.formula("p ~ Freq_MAF_Hapmap + N"),
      data = BMI_GIANT_GWAS,
      alpha = parameters_run$alphas[[i]],
      lambdas = parameters_run$lambdas[[i]],
      stratification_method = parameters_run$stratification_method[[i]],
      #quantile
      nbins = nbins,
      #nbins = 5,
      #forest
      n_censor_thres = parameters_run$n_censor_thres[[i]],
      ntrees = parameters_run$ntrees[[i]],
      nodedepth = parameters_run$nodedepth[[i]],
      nodesize = parameters_run$nodesize[[i]]
    )
  ))
  #TODO
  #effective_nbins <- mean(unlist(IHW::nbins(ihw_i)))
  
  res_i <- parameters_run[i, ]
  res_i <- res_i %>%
    mutate(rejections = rejections_na(ihw_i)#,
           #nbins_covariates = effective_nbins
    )
  
}
ihw_rej <- bind_rows(ihw_rej)
# },
#  file = "locke_ihw_rej.rds",
# hash = list(parameters_run)
#)
ihw_rej
#save 
saveRDS(ihw_rej, paste0("boca_leek/data/", Sys.Date(), "_boca_leek_analysis.RDS"))
