#download MAF data using Biomart

library(biomaRt)
library(magrittr)

file_loc <- "data/hqtl_chrom1_chrom2/"
download_loc <- "data/downloaded_covariates"

chr1_df <- readRDS("data/hqtl_chrom1_chrom2/chr1_subset.Rds") 
chr2_df <- readRDS("data/hqtl_chrom1_chrom2/chr2_subset.Rds") 


#snppos_chr1 <- snppos_chr1[1:1000,]
#snppos_chr2 <- snppos_chr2[1:1000,]
#TODO delete, loop over bits
chr1_df <- chr1_df %>% slice(1000) #slice_sample(n = 1000)
chr2_df <- chr2_df %>% slice(1000)

ensembl <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp")

chr1_maf <- getBM(
  attributes=c("refsnp_id", "minor_allele_freq"),
  filters="snp_filter", values=c(chr1_df$SNP),
  mart=ensembl, uniqueRows=TRUE)
saveRDS(chr1_maf, file = "data/downloaded_covariates/chr1_maf.Rds")

print("downloaded maf for chr1")
chr2_maf <- getBM(
  attributes=c("refsnp_id", "minor_allele_freq"),
  filters="snp_filter", values=c(chr1_df$SNP),
  mart=ensembl, uniqueRows=TRUE)


saveRDS(chr2_maf, file = "data/downloaded_covariates/chr2_maf.Rds")
#not available for all...