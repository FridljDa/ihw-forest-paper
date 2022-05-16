# download MAF data using Biomart
library(magrittr)
library(dplyr)
library(tidyr)

#load snps
snp_chr1 <- readRDS("data/hqtl_chrom1_chrom2/snppos_chr1.Rds") 
snp_chr2 <- readRDS("data/hqtl_chrom1_chrom2/snppos_chr2.Rds") 

ensembl <- biomaRt::useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp")

chr1_maf <- biomaRt::getBM(
  attributes = c("refsnp_id", "minor_allele_freq"),
  filters = "snp_filter", values = unique(snp_chr1$snp),
  mart = ensembl, uniqueRows = TRUE
)
saveRDS(chr1_maf, file = "data/downloaded_covariates/chr1_maf.Rds")

chr2_maf <- biomaRt::getBM(
  attributes = c("refsnp_id", "minor_allele_freq"),
  filters = "snp_filter", values = unique(snp_chr2$snp),
  mart = ensembl, uniqueRows = TRUE
)
saveRDS(chr2_maf, file = "data/downloaded_covariates/chr2_maf.Rds")