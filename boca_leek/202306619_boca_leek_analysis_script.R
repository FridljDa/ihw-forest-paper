## ---- echo=FALSE, message = FALSE-------------------------------------------
library(ggplot2)
library(here)
library(dplyr)
library(magrittr)
library(R.utils)
library(tictoc)

set.seed(4)
options(bitmapType ="cairo")


## ---------------------------------------------------------------------------
library(doRNG)
library(doParallel)
library(parallel)
 
registerDoParallel(cores=3)


## ---------------------------------------------------------------------------
##list.files("../../IHW")
if(Sys.info()["sysname"] == "Darwin"){
  devtools::load_all("/Users/default/Google Drive/currentDocumants/research/2022_IHW-Forest/Code/IHW")
}else{
  devtools::load_all(here("../IHW"))
}


## ---------------------------------------------------------------------------
error_handler <- function(x) { # TODO
  if (inherits(x, "try-error")) {
    x <- NA
  }
  x
}


rejections_na <- function(ihw) { 
  if (isS4(ihw)) {
    rejections(ihw)
  } else {
    return(NA)
  }
}



## ---------------------------------------------------------------------------
load(here("boca_leek/BMI_GIANT_GWAS.RData"))
load(here("boca_leek/data/locke_ucsc_additional_covariates.RData"))

BMI_GIANT_GWAS <- BMI_GIANT_GWAS %>%
  filter(chr_name %in% c("1","10"))
#BMI_GIANT_GWAS <- BMI_GIANT_GWAS %>% rename(pvalue = p)
#
folds <- BMI_GIANT_GWAS$chr_name %>%
  as.factor() %>%
  as.integer()

cat("BMI_GIANT_GWAS")
head(BMI_GIANT_GWAS)


## ---------------------------------------------------------------------------
BMI_GIANT_GWAS <- inner_join(BMI_GIANT_GWAS, ucsc_additional_covariates,
                   by = c("refsnp_id","chr_name"))


## ---------------------------------------------------------------------------
ggplot(BMI_GIANT_GWAS, aes(x = N)) +
  geom_histogram()


## ---------------------------------------------------------------------------
formula_mt <- c(
  #"Freq_MAF_Hapmap",
  "N",
  "Freq_MAF_Hapmap + N",
  "Freq_MAF_Hapmap + N +minor_allele_freq",
   "Freq_MAF_Hapmap + N +minor_allele_freq+covariate1",
  "Freq_MAF_Hapmap + N +minor_allele_freq+covariate1+covariate2",
  "Freq_MAF_Hapmap + N +minor_allele_freq+covariate1+covariate2+covariate3",
  "Freq_MAF_Hapmap + N +minor_allele_freq+covariate1+covariate2+covariate3+covariate4",
  "Freq_MAF_Hapmap + N +minor_allele_freq+covariate1+covariate2+covariate3+covariate4+covariate5"
)

formula_mt <- paste0("p ~ ", formula_mt)
formula_mt <- sapply(formula_mt, as.formula)




## ---------------------------------------------------------------------------
parameters_run_quantile <- expand.grid(
  formula = formula_mt,
  alphas = seq(0.01, 0.1, length.out = 4), #
  lambdas = "auto",
  stratification_method = "quantiles",
  #parameters for quantile
  nbins_quantile = NA,  #2^8
  #parameters for forest 
  nodedepth_forest = NA,
  n_censor_thres = NA, 
  ntrees = NA,  
  nodesize = NA
)

parameters_run_forest <- expand.grid(
  formula = formula_mt,
  alphas = seq(0.01, 0.1, length.out = 4), #2
  lambdas = "Inf",
  stratification_method = "forest",
  #parameters for quantile
  nbins_quantile = "auto", 
  #parameters for forest 
  nodedepth_forest = NA, #8
  n_censor_thres = 3,# 
  ntrees = 10, #3  
  nodesize = 3000
)

parameters_run <- rbind(parameters_run_quantile, parameters_run_forest)


## ---------------------------------------------------------------------------
parameters_run <- parameters_run %>%
  mutate(
    number_covariates = 1 + stringr::str_count(formula, "\\+"),
    formula_string = as.character(parameters_run$formula), 
    stratification_method = as.character(stratification_method)
  )

per_covariate_bins <- 5
#make sure we have the same number of total bins between forest and quantiles
parameters_run <- parameters_run %>%
  mutate(
    nodedepth_forest = ifelse(
      stratification_method == "forest",
      number_covariates * log2(per_covariate_bins) %>% 
        ceiling() %>%
        as.integer(),
      NA
    ),
    
    nbins_quantile = ifelse(
      stratification_method == "quantiles",
      per_covariate_bins^number_covariates,
      "auto"
    ) 
  )

cat("parameters_run")
head(parameters_run)


## ---- eval = TRUE-----------------------------------------------------------
#parameters_run_copy <- parameters_run
#BMI_GIANT_GWAS <- BMI_GIANT_GWAS %>% 
#    group_by(chr_name) %>%
#    sample_n(20000)# %>%
#    ungroup()

#---dry run---
parameters_run <- parameters_run %>%
  filter(#alphas == 0.04 #& number_covariates %in% c(4) #,2,3,4
         #& 
           stratification_method == "quantiles" &
     number_covariates %in% c(1) & 
       alphas == 0.04 
         )
#parameters_run


## ---------------------------------------------------------------------------
# run for all alpha-formula combinations (in parallel)
alpha_levels <- parameters_run %>%
  select(alphas) %>%
  distinct()

bh_rejections <- foreach(i = seq_len(nrow(alpha_levels)), 
                   .combine = rbind,
                   .packages = c('magrittr','dplyr')) %dopar%  { 
  print(paste0("run:", i))

  alpha_i <- alpha_levels$alphas[[i]]

  bh_rejections_i <- sum(p.adjust(na.exclude(BMI_GIANT_GWAS$p), method = "BH") <= alpha_i)
  # evaluate number of rejections
  data.frame(
    stratification_method = "BH",
    formula = "pvalue ~ 1",
    formula_string = "pvalue ~ 1",
    number_covariates = 0,
    alphas = alpha_i,
    rejections = bh_rejections_i,
    nbins_covariates = 0
  )
}
bh_rejections <- bind_rows(bh_rejections)

cat("bh_rejections")
head(bh_rejections)


## ---------------------------------------------------------------------------
# Set the timeout duration in seconds
timeout <- 60*50


## ---- eval = TRUE-----------------------------------------------------------
cat("running IHW")
ihw_rej <- foreach(i = seq_len(nrow(parameters_run))
                   , .combine = rbind
                   ) %dopar% {
#ihw_rej <- purrr::map_dfr(seq_len(nrow(parameters_run)),
#                          function(i){
#for(i in seq_len(nrow(parameters_run))){
  cat('Starting ', i, 'th job.\n', sep = '')
  tic("running IHW")
    
      nbins = parameters_run$nbins_quantile[[i]]
      if(nbins != "auto") nbins <- as.numeric(nbins)
      lambdas = parameters_run$lambdas[[i]]
      if(lambdas == "Inf") lambdas <- Inf
      
     # Define the function call parameters
  formula <- parameters_run$formula[[i]]
  data <- BMI_GIANT_GWAS
  alpha <- parameters_run$alphas[[i]]
  stratification_method <- parameters_run$stratification_method[[i]]
  n_censor_thres <- parameters_run$n_censor_thres[[i]]
  ntrees <- parameters_run$ntrees[[i]]
  nodedepth <- parameters_run$nodedepth[[i]]
  nodesize <- parameters_run$nodesize[[i]]

# Define the tryCatch block with time out
ihw_i <- tryCatch(
  {
    withTimeout(
      {
        ihw(
          formula = formula,
          data = data,
          alpha = alpha,
          lambdas = lambdas,
          stratification_method = stratification_method,
          nbins = nbins,
          n_censor_thres = n_censor_thres,
          ntrees = ntrees,
          nodedepth = nodedepth,
          nodesize = nodesize
        )
      },
      timeout = timeout
    )
  },
  error = function(e) {
    NULL
  }
)
      # handle unfinished jobs
      res_i <- parameters_run[i, ]
      
      
      if (isS4(ihw_i)) {
        #effective number of bins
        effective_nbins <- mean(unlist(IHW::nbins(ihw_i)))
       
        res_i <- res_i %>%
          mutate(
            effective_nbins_covariates = effective_nbins,
            rejections = rejections(ihw_i)
          )
      } else {
       res_i <- res_i %>%
        mutate(
          effective_nbins_covariates = NA,
          rejections = NA
        )
      }
  
  #save elapsed time
  tic_toc_object <- toc()
  elapsed_time <- tic_toc_object[["toc"]][["elapsed"]]-tic_toc_object[["tic"]][["elapsed"]]
  res_i <- res_i %>%
        mutate(elapsed_time = elapsed_time)

  cat('Finishing ', i, 'th job.\n', sep = '')
  res_i # this will become part of the out object
}#)

head(ihw_rej)



## ---------------------------------------------------------------------------
number_covariates_all <- unique(ihw_rej$number_covariates)

bh_rejections_copied <- replicate(length(number_covariates_all), 
                          bh_rejections, 
                          simplify = FALSE) %>%
  data.table::rbindlist() %>%
  select(-c(number_covariates))

number_covariates_all_rep <- rep(number_covariates_all, each = nrow(bh_rejections))

bh_rejections_copied <- cbind(bh_rejections_copied, number_covariates = number_covariates_all_rep)

rm(number_covariates_all, number_covariates_all_rep)


## ---------------------------------------------------------------------------
ihw_bh_rej <- plyr::rbind.fill(bh_rejections_copied, ihw_rej)


## ---- eval=TRUE-------------------------------------------------------------
saveRDS(ihw_bh_rej, paste0("data/", Sys.Date(), "_boca_leek_analysis.RDS"))

