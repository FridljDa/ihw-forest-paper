## ---- echo=FALSE, message = FALSE-------------------------------------------
library(here)
library(dplyr)
library(magrittr)
library(tictoc)

## ---------------------------------------------------------------------------
library(doRNG)
library(doParallel)
library(parallel)
 
registerDoParallel(cores=3)

set.seed(4)
options(bitmapType ="cairo")

## ---------------------------------------------------------------------------
##list.files("../../IHW")
if(Sys.info()["sysname"] == "Darwin"){
  devtools::load_all("/Users/default/Google Drive/currentDocumants/research/2022_IHW-Forest/Code/IHW")
}else{
  devtools::load_all(here("../IHW"))
}
devtools::load_all(here::here("IHWForestPaper"))

## ---------------------------------------------------------------------------

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

## ---------------------------------------------------------------------------
BMI_GIANT_GWAS <- inner_join(BMI_GIANT_GWAS, ucsc_additional_covariates,
                   by = c("refsnp_id","chr_name"))

## ---------------------------------------------------------------------------
summands_list <- list(
  c("N"),
  c("Freq_MAF_Hapmap", "N"),
  c("Freq_MAF_Hapmap", "N", "minor_allele_freq"),
  c("Freq_MAF_Hapmap", "N", "minor_allele_freq", "covariate1"),
  c("Freq_MAF_Hapmap", "N", "minor_allele_freq", "covariate1", "covariate2"),
  c("Freq_MAF_Hapmap", "N", "minor_allele_freq", "covariate1", "covariate2", "covariate3"),
  c("Freq_MAF_Hapmap", "N", "minor_allele_freq", "covariate1", "covariate2", "covariate3", "covariate4"),
  c("Freq_MAF_Hapmap", "N", "minor_allele_freq", "covariate1", "covariate2", "covariate3", "covariate4", "covariate5")
)

# apply the anonymous function to each element of summands_list
formulas_mt <- sapply(summands_list, function(x) paste(x, collapse = " + "))

# print the formulas
print(formulas_mt)

formula_mt <- paste0("p ~ ", formula_mt)
formula_mt <- sapply(formula_mt, as.formula)

## ---------------------------------------------------------------------------
# create a data frame
parameters_run <- data.frame(
  formula = formulas_mt,
  summands = I(summands_list),
  number_covariates = sapply(summands_list, length),
  formula_string = sapply(formulas_mt, as.character)  
)

parameters_run <- parameters_run %>%
  merge(data.frame(alphas = seq(0.01, 0.1, length.out = 8)))

cat("parameters_run\n")
head(parameters_run)

cat("\n")
cat(nrow(parameters_run))
## ---- eval = TRUE-----------------------------------------------------------
#---dry run---
#parameters_run_copy <- parameters_run
BMI_GIANT_GWAS <- BMI_GIANT_GWAS %>% 
    #group_by(chr_name) %>%
    sample_n(20000)# %>%
    #ungroup()

parameters_run <- parameters_run %>%
  filter(alphas == 0.01 #& number_covariates %in% c(4) #,2,3,4
         #& 
          # stratification_method == "quantiles" &
     #number_covariates %in% c(1) & 
#       alphas == 0.04 
         )
#parameters_run

## ---------------------------------------------------------------------------
# Set the timeout duration in seconds
timeout <- 60*50


## ---- eval = TRUE-----------------------------------------------------------
pvalue <- BMI_GIANT_GWAS$p

#result <- foreach(i = seq_len(nrow(parameters_run))
#                   , .combine = rbind
#                   ) %dopar% {
for(i in seq_len(nrow(parameters_run))){
  cat('Starting ', i, 'th job.\n', sep = '')
  tic("running IHW")
    
  # Define the function call parameters
  formula <- parameters_run$formula[[i]]
  alpha_i <- parameters_run$alphas[[i]]
  summands_i <- parameters_run$summands[[i]]
  
  covariate_i <- BMI_GIANT_GWAS[,summands_i]
  
  # Run the function
  res_i <- run_sim(Ps = pvalue,
                   X = covariate_i,
                   H = NULL,
                   seed = 1, 
                   alpha = alpha_i, 
                   #methods = methods, 
                   forest_par = NULL, 
                   null_proportion = TRUE,
                   folds = folds)
  
  #save elapsed time
  tic_toc_object <- toc()
  elapsed_time <- tic_toc_object[["toc"]][["elapsed"]]-tic_toc_object[["tic"]][["elapsed"]]
  res_i <- res_i %>%
        mutate(elapsed_time = elapsed_time)

  cat('Finishing ', i, 'th job.\n', sep = '')
  res_i # this will become part of the out object
}#)


## ---- eval=TRUE-------------------------------------------------------------
saveRDS(result, paste0("boca_leek/data/", Sys.Date(), "_boca_leek_analysis.RDS"))

