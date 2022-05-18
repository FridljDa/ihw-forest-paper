# download MAF data using Biomart
library(magrittr)
library(dplyr)
library(tidyr)

#load snps
snp_chr1 <- readRDS("data/hqtl_chrom1_chrom2/snppos_chr1.Rds") 
snp_chr2 <- readRDS("data/hqtl_chrom1_chrom2/snppos_chr2.Rds") 

#create biomart session
ensembl <- biomaRt::useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp")

#download data
chr1_maf <- biomaRt::getBM(
  attributes = c("refsnp_id", "minor_allele_freq"),
  filters = "snp_filter", values = unique(snp_chr1$snp),
  mart = ensembl, uniqueRows = TRUE
)

#identify missing rows
chr1_maf_missing <- anti_join(snp_chr1, chr1_maf, by = c("snp" = "refsnp_id")) %>%
  transmute(refsnp_id = snp, minor_allele_freq = NA)

#chr1_maf_missing_filled <- biomaRt::getBM(
#  attributes = c("refsnp_id", "minor_allele_freq"),
#  filters = "snp_filter", values = unique(chr1_maf_missing$refsnp_id),
#  mart = ensembl, uniqueRows = TRUE
#)

chr1_maf <- rbind(chr1_maf, chr1_maf_missing)

#save
saveRDS(chr1_maf, file = "data/downloaded_covariates/chr1_maf.Rds")

#download data
chr2_maf <- biomaRt::getBM(
  attributes = c("refsnp_id", "minor_allele_freq"),
  filters = "snp_filter", values = unique(snp_chr2$snp),
  mart = ensembl, uniqueRows = TRUE
)

#identify missing rows
chr2_maf_missing <- anti_join(snp_chr2, chr2_maf, by = c("snp" = "refsnp_id")) %>%
  transmute(refsnp_id = snp, minor_allele_freq = NA)
chr2_maf <- rbind(chr2_maf, chr2_maf_missing)

#save
saveRDS(chr2_maf, file = "data/downloaded_covariates/chr2_maf.Rds")
