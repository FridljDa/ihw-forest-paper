## ---- echo=FALSE, message = FALSE-------------------------------------------
library(here)
library(dplyr)
library(magrittr)

## ---------------------------------------------------------------------------

set.seed(4)
options(bitmapType = "cairo")

## ---------------------------------------------------------------------------
## list.files("../../IHW")
# if(Sys.info()["sysname"] == "Darwin"){
#  devtools::load_all("/Users/default/Google Drive/currentDocumants/research/2022_IHW-Forest/Code/IHW")
# }else{
#  devtools::load_all(here("../IHW"))
# }
devtools::load_all(here::here("IHWForestPaper"))

## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
load(here("boca_leek/BMI_GIANT_GWAS.RData"))
load(here("boca_leek/data/locke_ucsc_additional_covariates.RData"))

BMI_GIANT_GWAS <- BMI_GIANT_GWAS %>%
  filter(chr_name %in% c("1", "10"))
# BMI_GIANT_GWAS <- BMI_GIANT_GWAS %>% rename(pvalue = p)
#

## ---------------------------------------------------------------------------
BMI_GIANT_GWAS <- inner_join(BMI_GIANT_GWAS, ucsc_additional_covariates,
  by = c("refsnp_id", "chr_name")
)

## ---------------------------------------------------------------------------
summands_list <- list(
  c("Freq_MAF_Hapmap"),
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

formulas_mt <- paste0("p ~ ", formulas_mt)
formulas_mt <- sapply(formulas_mt, as.formula)

## ---------------------------------------------------------------------------
# create a data frame
parameters_run <- data.frame(
  # formula = formulas_mt,
  summands = I(summands_list),
  number_covariates = sapply(summands_list, length) # ,
  # formula_string = sapply(formulas_mt, as.character)
)

parameters_run <- parameters_run %>%
  merge(data.frame(alphas = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4)))

## ---- eval = TRUE-----------------------------------------------------------
#---dry run---

methods = c("BH", "AdaPT", "Clfdr-EM", "IHW-quantile", "IHW-forest", "Boca-Leek", "AdaPT-xgboost")

dry_run <- FALSE
if (dry_run) {
  # parameters_run_copy <- parameters_run
  BMI_GIANT_GWAS <- BMI_GIANT_GWAS %>%
    # group_by(chr_name) %>%
   sample_n(2000) # %>%
  # ungroup()

  parameters_run <- parameters_run %>%
    filter(
      alphas == 0.1 & number_covariates %in% c(1) # ,2,3,4
      # &
      # stratification_method == "quantiles" &
      # number_covariates %in% c(1) &
      #       alphas == 0.04
    )
  # parameters_run
  #methods = c("Clfdr-EM")
  methods = c("IHW-forest", "AdaPT-xgboost")
  parallel = FALSE
}else{
  parallel = TRUE
}

cat("parameters_run\n")
head(parameters_run)

cat("\n")
cat(nrow(parameters_run))

folds <- BMI_GIANT_GWAS$chr_name %>%
  as.factor() %>%
  as.integer()

##---set up configuration---
simulation_list <- lapply(
  seq_len(nrow(parameters_run)),
  function(i) {
    formula_i <- parameters_run$formula[[i]]
    alpha_i <- parameters_run$alphas[[i]]
    number_covariates_i <- parameters_run$number_covariates[[i]]
    summands_i <- parameters_run$summands[[i]]

    covariate_i <- BMI_GIANT_GWAS[, summands_i]

    list(
      seed = NULL,
      pvalue = BMI_GIANT_GWAS$p,
      covariate = covariate_i,
      Hs = NULL,
      alpha = alpha_i,
      number_covariates = number_covariates_i,
      formula = formula_i
    )
  }
)

#-----run multiple testing----

result <- eval_sim_parallel(simulation_list,
                              methods = methods,
                              null_proportion = TRUE,
                              folds = folds,
                              parallel = parallel)


## ---- eval=TRUE-------------------------------------------------------------
saveRDS(result, paste0("boca_leek/data/", Sys.Date(), "_boca_leek_analysis_adapt_ihw_forest.RDS"))
