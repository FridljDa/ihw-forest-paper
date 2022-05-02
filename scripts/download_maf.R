# download MAF data using Biomart

library(biomaRt)
library(magrittr)
library(dplyr)

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
rm(chr1_df, chr2_df)
# snppos_chr1 <- snppos_chr1[1:1000,]
# snppos_chr2 <- snppos_chr2[1:1000,]
# TODO delete, loop over bits
# chr1_df <- chr1_df %>% slice(1:1000) #slice_sample(n = 1000)
# chr2_df <- chr2_df %>% slice(1:1000) #nope!

ensembl <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp")

chunk <- split(seq_len(nrow(chr1_maf)), ceiling(seq_len(nrow(chr1_maf)) / 1000))

pb <- txtProgressBar(min = 0, max = length(chunk), initial = 0)
# for (i in seq_along(chunk)){
#  setTxtProgressBar(pb,i)
i <- 2
indices_i <- chunk[[i]]
refsnp_id <- chr1_maf$SNP[indices_i]
tmp <- biomaRt::getBM(
  attributes = c("minor_allele_freq"),
  filters = "snp_filter", values = "rs4660616",
  mart = ensembl, uniqueRows = TRUE
)
if (nrow(tmp) == 1) chr1_maf$minor_allele_freq[indices_i] <- tmp$minor_allele_freq
# }
chr1_maf <- chr1_maf %>% mutate(minor_allele_freq = replace_na(minor_allele_freq, 0)) # TODO valid?

saveRDS(chr1_maf, file = "data/downloaded_covariates/chr1_maf.Rds")
print("downloaded MAF for first chromosome")
# source data for first chromosome
pb <- txtProgressBar(min = 0, max = nrow(chr2_maf), initial = 0)
for (i in seq_len(nrow(chr2_maf))) {
  setTxtProgressBar(pb, i)
  refsnp_id <- c(chr2_maf$SNP[i])
  tmp <- getBM(
    attributes = c("refsnp_id", "minor_allele_freq"), #
    filters = "snp_filter", values = refsnp_id,
    mart = ensembl, uniqueRows = TRUE
  )
  if (nrow(tmp) == 1) chr2_maf$minor_allele_freq[i] <- tmp$minor_allele_freq
}
chr2_maf <- chr2_maf %>% mutate(minor_allele_freq = replace_na(minor_allele_freq, 0)) # TODO valid

saveRDS(chr2_maf, file = "data/downloaded_covariates/chr2_maf.Rds")
print("downloaded MAF for second chromosome")
