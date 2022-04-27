## ----warning=FALSE, message=FALSE---------------------------------------------
library(dplyr)

library(fdrtool)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(tidyr)
library(scales)
#load my stuff
#devtools::load_all("../IHW-1")
#devtools::load_all("../RFCDE")
# library(latex2exp)
# library(biomaRt)
devtools::install_github("FridljDa/IHW1", ref = "ihw_forest2")
library(IHW)

#JuliaCall::julia_setup(JULIA_HOME = "/Applications/Julia-1.6.app/Contents/Resources/julia/bin/")
#JuliaCall::julia_command("cd(\"/Users/default/Google Drive/currentDocumants/Studium/Master/3.Semester/Masterarbeit/Code/IndependentHypothesisWeightingTrees.jl\")")
#JuliaCall::julia_source("/Users/default/Google Drive/currentDocumants/Studium/Master/3.Semester/Masterarbeit/Code/IndependentHypothesisWeightingTrees.jl/example/wrapper.jl")

## -----------------------------------------------------------------------------
#file_loc <- system.file("extdata", "real_data", "hqtl_chrom1_chrom2", package = "IHWpaper")
#file_loc <- "/Users/default/Google Drive/currentDocumants/Studium/Master/3.Semester/Masterarbeit/Code/IHWpaper/inst/extdata/real_data/hqtl_chrom1_chrom2"
 file_loc <- "~/R/IHW real data/hqtl_chrom1_chrom2/"
#download_loc <- "/Users/default/Google Drive/currentDocumants/Studium/Master/3.Semester/Masterarbeit/Code/IHW realdata/downloaded files/"
 download_loc <- "~/R/IHW real data/downloaded files/"

### -----------------------------------------------------------------------------
chr1_df <- readRDS(file.path(file_loc, "chr1_subset.Rds"))
chr2_df <- readRDS(file.path(file_loc, "chr2_subset.Rds"))
# rs115505656

## -----------------------------------------------------------------------------
#pval_threshold <- 10^(-4)

## -----------------------------------------------------------------------------
snp_chr1 <- readRDS(file.path(file_loc, "snppos_chr1.Rds"))
snp_chr2 <- readRDS(file.path(file_loc, "snppos_chr2.Rds"))

all_peaks <- readRDS(file.path(file_loc, "peak_locations.Rds"))
peaks_chr1 <- dplyr::filter(all_peaks, chr == "chr1")
peaks_chr2 <- dplyr::filter(all_peaks, chr == "chr2")
# gene SNP
## -----------------------------------------------------------------------------
chr1_df <- left_join(chr1_df, select(snp_chr1, snp, pos), by = (c("SNP" = "snp"))) %>%
  left_join(peaks_chr1, by = (c("gene" = "id"))) %>%
  mutate(cov_dist = pmin(abs(pos - start), abs(pos - end)))

chr2_df <- left_join(chr2_df, select(snp_chr2, snp, pos), by = (c("SNP" = "snp"))) %>%
  left_join(peaks_chr2, by = (c("gene" = "id"))) %>%
  mutate(cov_dist = pmin(abs(pos - start), abs(pos - end)))

## ---read new covariates -----
# value
snp_chr1_cov1 <- readRDS(file.path(download_loc, "snp_chr1_cov1.Rds"))
snp_chr1_cov2 <- readRDS(file.path(download_loc, "snp_chr1_cov2.Rds"))
snp_chr1_cov3 <- readRDS(file.path(download_loc, "snp_chr1_cov3.Rds"))
snp_chr1_cov4 <- readRDS(file.path(download_loc, "snp_chr1_cov4.Rds"))
snp_chr1_cov5 <- readRDS(file.path(download_loc, "snp_chr1_cov5.Rds"))
snp_chr1_cov6 <- readRDS(file.path(download_loc, "snp_chr1_cov6.Rds"))
snp_chr1_cov7 <- readRDS(file.path(download_loc, "snp_chr1_cov7.Rds"))
snp_chr1_cov8 <- readRDS(file.path(download_loc, "snp_chr1_cov8.Rds"))
snp_chr1_cov9 <- readRDS(file.path(download_loc, "snp_chr1_cov9.Rds"))
# signalValue
snp_chr1_cov10 <- readRDS(file.path(download_loc, "snp_chr1_cov10.Rds"))
snp_chr1_cov11 <- readRDS(file.path(download_loc, "snp_chr1_cov11.Rds"))
# value
snp_chr1_cov12 <- readRDS(file.path(download_loc, "snp_chr1_cov12.Rds")) # dubious, check
# signalValue
snp_chr1_cov13 <- readRDS(file.path(download_loc, "snp_chr1_cov13.Rds"))
snp_chr1_cov14 <- readRDS(file.path(download_loc, "snp_chr1_cov14.Rds"))
# value
snp_chr1_cov15 <- readRDS(file.path(download_loc, "snp_chr1_cov15.Rds"))


snp_chr2_cov1 <- readRDS(file.path(download_loc, "snp_chr2_cov1.Rds"))
snp_chr2_cov2 <- readRDS(file.path(download_loc, "snp_chr2_cov2.Rds"))
snp_chr2_cov3 <- readRDS(file.path(download_loc, "snp_chr2_cov3.Rds"))
snp_chr2_cov4 <- readRDS(file.path(download_loc, "snp_chr2_cov4.Rds"))
snp_chr2_cov5 <- readRDS(file.path(download_loc, "snp_chr2_cov5.Rds"))
snp_chr2_cov6 <- readRDS(file.path(download_loc, "snp_chr2_cov6.Rds"))
snp_chr2_cov7 <- readRDS(file.path(download_loc, "snp_chr2_cov7.Rds"))
snp_chr2_cov8 <- readRDS(file.path(download_loc, "snp_chr2_cov8.Rds"))
snp_chr2_cov9 <- readRDS(file.path(download_loc, "snp_chr2_cov9.Rds"))
snp_chr2_cov10 <- readRDS(file.path(download_loc, "snp_chr2_cov10.Rds"))
snp_chr2_cov11 <- readRDS(file.path(download_loc, "snp_chr2_cov11.Rds"))
snp_chr2_cov12 <- readRDS(file.path(download_loc, "snp_chr2_cov12.Rds"))
snp_chr2_cov13 <- readRDS(file.path(download_loc, "snp_chr2_cov13.Rds"))
snp_chr2_cov14 <- readRDS(file.path(download_loc, "snp_chr2_cov14.Rds"))
snp_chr2_cov15 <- readRDS(file.path(download_loc, "snp_chr2_cov15.Rds"))

## ----join new covariates-------
chr1_df <- left_join(chr1_df, select(snp_chr1_cov1, snp, cov1 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov2, snp, cov2 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov3, snp, cov3 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov4, snp, cov4 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov5, snp, cov5 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov6, snp, cov6 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov7, snp, cov7 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov8, snp, cov8 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov9, snp, cov9 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov10, snp, cov10 = signalValue), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov11, snp, cov11 = signalValue), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov12, snp, cov12 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov13, snp, cov13 = signalValue), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov14, snp, cov14 = signalValue), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr1_cov15, snp, cov15 = value), by = (c("SNP" = "snp")))

chr2_df <- left_join(chr2_df, select(snp_chr2_cov1, snp, cov1 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov2, snp, cov2 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov3, snp, cov3 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov4, snp, cov4 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov5, snp, cov5 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov6, snp, cov6 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov7, snp, cov7 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov8, snp, cov8 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov9, snp, cov9 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov10, snp, cov10 = signalValue), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov11, snp, cov11 = signalValue), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov12, snp, cov12 = value), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov13, snp, cov13 = signalValue), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov14, snp, cov14 = signalValue), by = (c("SNP" = "snp"))) %>%
  left_join(select(snp_chr2_cov15, snp, cov15 = value), by = (c("SNP" = "snp")))

## -----------------------------------------------------------------------------
# my_breaks <- c(-1,
#               seq(from=10000,to=290000, by=10000) ,
#               seq(from=300000, to=0.9*10^6, by=100000),
#               seq(from=10^6, to=25.1*10^7, by=10^7))
# myf1 <- cut(chr1_df$cov_dist, my_breaks)
# myf2 <- cut(chr2_df$cov_dist, my_breaks)

## -----------------------------------------------------------------------------
ms_chr1 <- readRDS(file.path(file_loc, "m_groups_chr1.Rds"))
ms_chr2 <- readRDS(file.path(file_loc, "m_groups_chr2.Rds"))

## -----------------------------------------------------------------------------
chr1_chr2_df <- rbind(chr1_df, chr2_df)
chr1_chr2_cov <- chr1_chr2_df %>% select(starts_with("cov"))
chr1_chr2_cov <- as.matrix(chr1_chr2_cov)

# chr1_chr2_groups <- as.factor(c(myf1,myf2))
folds_vec <- as.factor(c(rep(1, nrow(chr1_df)), rep(2, nrow(chr2_df))))
# m_groups <- cbind(ms_chr1, ms_chr2)

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
devtools::load_all("../IHW-1")
m <- nrow(chr1_chr2_df)
alpha <- .01 / (log(m) + 1)
## hyper parameters--
#tau <- 0.5 # censoring parameter for BocaLeek estimator
#tau <- quantile(chr1_chr2_df$pvalue, probs = 0.999999) #TODO technically not allowed, but pretty academic...
#n_basis <- 20L
#nbins <- 10L
lambda <- Inf

taus <- NULL

ntrees <- 5L # number of trees per forest
n_censor_thres <- 10
taus <- NULL

nsplit <- 7
nodedepth <- 10 # 3 worked well!
min.node.size <- 1200

## -----------------------------------------------------------------------------
#ihw_cut <- ihw(chr1_chr2_df$pvalue, chr1_chr2_cov, alpha = alpha, folds = folds_vec, strat = "cut", lambda = Inf)
chr1_chr2_cov_test <- chr1_chr2_cov[,1, drop = F] #TODO 
ihw_bocaleek <- ihw(chr1_chr2_df$pvalue, chr1_chr2_cov_test, alpha = alpha, folds = folds_vec, strat = "BocaLeek", ntrees = ntrees,nbins = nbins, 
                    taus = taus, nsplit=nsplit, n_censor_thres = n_censor_thres, nodedepth = nodedepth, min.node.size = min.node.size, lambda = Inf,
                     null_proportion = T)
#ihw_RFCDE <- ihw(chr1_chr2_df$pvalue, chr1_chr2_cov, alpha = alpha, folds = folds_vec, strat = "RFCDE", n_basis = n_basis, ntrees = ntrees,nbins = nbins, lambda = Inf)
#ihw_Julia <- ihw(chr1_chr2_df$pvalue, chr1_chr2_cov, alpha = alpha, folds = folds_vec, strat = "Julia", n_basis = n_basis, ntrees = ntrees,nbins = nbins, lambda = Inf)

ihw_df <- IHW::as.data.frame(ihw_bocaleek)

ihw_freq <- table(
  fold = ihw_df$fold,
  group = ihw_df$group2
)
ihw_freq

## -----------------------------------------------------------------------------
rej_bh <- sum(p.adjust(chr1_chr2_df$pvalue, n = m, method = "BH") <= alpha)

## -----------------------------------------------------------------------------
#rejections(ihw_cut)
rej_bocaleek <- rejections(ihw_bocaleek)
rej_bh
rej_bocaleek
rej_bh < rej_bocaleek
#rejections(ihw_RFCDE)
