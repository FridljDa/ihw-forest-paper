# download MAF data using Biomart

library(biomaRt)
library(magrittr)
library(dplyr)
library(tidyr)

file_loc <- "data/hqtl_chrom1_chrom2/"
download_loc <- "data/downloaded_covariates"

chr1_df <- readRDS("data/hqtl_chrom1_chrom2/chr1_subset.Rds")
chr2_df <- readRDS("data/hqtl_chrom1_chrom2/chr2_subset.Rds")

chr1_maf <- data.frame(
  SNP = unique(chr1_df$SNP),
  minor_allele_freq = NA
)

chr2_maf <- data.frame(
  SNP = unique(chr2_df$SNP),
  minor_allele_freq = NA
)
# rm(chr1_df, chr2_df)
# snppos_chr1 <- snppos_chr1[1:1000,]
# snppos_chr2 <- snppos_chr2[1:1000,]
# TODO delete, loop over bits
# chr1_df <- chr1_df %>% slice(1:1000) #slice_sample(n = 1000)
# chr2_df <- chr2_df %>% slice(1:1000) #nope!

ensembl <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp")

if(FALSE){
  
  chunk <- split(seq_len(nrow(chr1_maf)), ceiling(seq_len(nrow(chr1_maf)) / 1000))
  
  pb <- txtProgressBar(min = 0, max = length(chunk), initial = 0)
  #for (i in 1:3) {
  for (i in seq_along(chunk)) {
    setTxtProgressBar(pb, i)
    # i <- 2
    indices_i <- chunk[[i]]
    refsnp_id <- chr1_maf$SNP[indices_i]
    tmp <- biomaRt::getBM(
      attributes = c("refsnp_id", "minor_allele_freq"),
      filters = "snp_filter", values = refsnp_id,
      mart = ensembl, uniqueRows = TRUE
    )
    tmp <- tmp %>% 
      rename(SNP = refsnp_id) %>%
      mutate(minor_allele_freq = replace_na(minor_allele_freq, 0))
    
    chr1_maf <- merge(chr1_maf[, "SNP", drop = FALSE], tmp, by = c("SNP"), all.x = TRUE)
  }
  saveRDS(chr1_maf, file = "data/downloaded_covariates/chr1_maf.Rds")
  print("downloaded MAF for first chromosome")
  
  chunk <- split(seq_len(nrow(chr2_maf)), ceiling(seq_len(nrow(chr2_maf)) / 1000))
  
  pb <- txtProgressBar(min = 0, max = length(chunk), initial = 0)
  #for (i in 1:3) {
  for (i in seq_along(chunk)) {
    setTxtProgressBar(pb, i)
    # i <- 2
    indices_i <- chunk[[i]]
    refsnp_id <- chr2_maf$SNP[indices_i]
    tmp <- biomaRt::getBM(
      attributes = c("refsnp_id", "minor_allele_freq"),
      filters = "snp_filter", values = refsnp_id,
      mart = ensembl, uniqueRows = TRUE
    )
    tmp <- tmp %>% 
      rename(SNP = refsnp_id) %>%
      mutate(minor_allele_freq = replace_na(minor_allele_freq, 0))
    
    chr1_maf <- merge(chr2_maf[, "SNP", drop = FALSE], tmp, by = c("SNP"), all.x = TRUE)
  }
  saveRDS(chr2_maf, file = "data/downloaded_covariates/chr2_maf.Rds")
  print("downloaded MAF for first chromosome")
}