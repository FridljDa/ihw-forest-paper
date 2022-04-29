#download MAF data using Biomart

library(biomaRt)

file_loc <- "data/hqtl_chrom1_chrom2/"
download_loc <- "data/downloaded_covariates"

snppos_chr1 <- readRDS("data/hqtl_chrom1_chrom2/snppos_chr1.Rds")
snppos_chr2 <- readRDS("data/hqtl_chrom1_chrom2/snppos_chr2.Rds")


ensembl <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp")

chr1_maf <- getBM(
  attributes=c("refsnp_id", "minor_allele_freq"),
  filters="snp_filter", values=c(snppos_chr1$snp),
  mart=ensembl, uniqueRows=TRUE)
saveRDS(chr1_maf, file = "data/downloaded_covariates/chr1_maf.Rds")

print("downloaded maf for chr1")
chr2_maf <- getBM(
  attributes=c("refsnp_id", "minor_allele_freq"),
  filters="snp_filter", values=c(snppos_chr2$snp),
  mart=ensembl, uniqueRows=TRUE)


saveRDS(chr2_maf, file = "data/downloaded_covariates/chr2_maf.Rds")
#not available for all...