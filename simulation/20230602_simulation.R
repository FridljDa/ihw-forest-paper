
#library(doRNG)
#library(doParallel)
#library(parallel)
library(dplyr)
library(magrittr)
library(ggplot2)
devtools::load_all("../IHW")
#devtools::load_all("/Users/default/Google Drive/currentDocumants/research/2022_IHW-Forest/Code/IHW")
devtools::load_all("IHWForestPaper")

n.cores <- parallel::detectCores()
#doParallel::registerDoParallel(cores = min(3, n.cores - 1))
library(foreach)
cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)

## ------Simulation------
prop_alt <- function(cov_row) {
  #high-dimensional equal-sized cube of constant volume
  #nvar <- length(cov_row)
  #r_bound <- 0.1^(1/nvar)
  #r <- sum(cov_row)
  #ifelse(r <= r_bound, 0.9, 0)
  
  #ifelse(cov_row[length(cov_row)] <= 0.1, 0.9, 0)
 # r^t = 0.1
  
  #r <- sum(cov_row^2)
  # exp(-5*r)
  # 1 / (1 + exp(-cov1))
  # 1 / (1 + exp(-1 * (3 * cov_row[1]+cov_row[2] - 5)))
  # browser()
  # ifelse(sum(cov_row^2) <= 0.5, 0.9, 0)
  # ifelse(r <= 0.1, 0.9, 0)
  # 1 / (1 + exp(-cov_row[1]))
  # ifelse(sum(cov_row^2)  <= 1, 0.02, 0.4)
  # ifelse(cov_row[1]^2+cov_row[2]^2  <= 1, 0.02, 0.4)
  
 # pi1s <- ifelse(Xs[ ,1] <= 0.1, 0.9, 0)
  
   #ifelse(r <= 0.1, 0.9, 0) #This works well
  #last dimension
  ifelse(cov_row[length(cov_row)] <= 0.1, 0.9, 0)
}

noise_sim <- function(m, r, dimensions){
  sim_combs <- expand.grid(
    m = m,
    dimensions = dimensions,
    seed = seq_len(r)
  )
  
  # simple_sim <- foreach(i = seq_len(r)) %dorng% {
  simple_sim <- lapply(seq_len(nrow(sim_combs)), function(i) {
    m_i <- sim_combs$m[i]
    dimension_i <- sim_combs$dimensions[i]
    seed_i <- sim_combs$seed[i]
    covariate_i <- matrix(runif(m_i * dimension_i, 0, 1), nrow = m_i)
    
    prop_alt_i <- apply(covariate_i, 1, prop_alt)
    #browser()
    Hs_i <- rbinom(m, size = 1, prop_alt_i)
    
    # mus <- pmax(1.3, sqrt(covariate_i[,c(1,2),drop = F]) %*% c(1,1)*1.5)
    # mu_alphas <- 1/mus
    # pvalue_i <- stats::runif(m)*(1-Hs_i) + stats::rbeta(m, mu_alphas, 1)*Hs_i
    
    pvalue_i <- ifelse(Hs_i,
                       rbeta(m_i, 0.25, 1),
                       runif(m_i)
    )
    
    return(list(covariate = covariate_i, prop_alt = prop_alt_i, Hs = Hs_i, pvalue = pvalue_i, 
                dimension = dimension_i, seed = seed_i, m_i = m))
  })
  simple_sim
}





sim <- noise_sim(m = 10000, r = 5, dimensions = 1:5)

sapply(sim, function(sim_i) mean(sim_i$Hs))

eval <- purrr::map_dfr(seq_along(sim), function(i){
#eval <- foreach(i = seq_along(sim), .combine = 'rbind') %do% {
#  sqrt(i)
#}
#eval

#eval <- foreach(i = seq_along(sim), .combine = 'rbind') %dopar% {
    #i <- 1
    print(paste0("simulation run:", i))
    sim_i <- sim[[i]]
    dimension_i <- sim_i$dimension
    seed_i <- sim_i$seed
    
    Ps_i <- sim_i$pvalue
    Xs_i <- sim_i$covariate
    Hs_i <- sim_i$Hs
    
    per_covariate_bin <- 10
    nbins_quantile <- per_covariate_bin^dimension_i
    
    if(TRUE){
    ihw_quantile <- IHW::ihw(Ps_i, 
                             Xs_i, 
                             alpha = 0.1, 
                             stratification_method = "quantiles", nbins = nbins_quantile#,
    #lambdas = Inf
    )
  
    rejected_hypotheses_quantile <- IHW::rejected_hypotheses(ihw_quantile)

    effective_nbins_quantile <- mean(unlist(IHW::nbins(ihw_quantile)))
    }
  
    if(TRUE){
      nodedepth <- 2 * dimension_i * log2(per_covariate_bin)
      nodesize <- ceiling(length(Ps_i)/nbins_quantile)
  
  ihw_forest <- IHW::ihw(Ps_i, Xs_i, alpha = 0.1,
    stratification_method = "forest",
    ntrees = 1, 
    n_censor_thres = 1, 
    nodesize = nodesize,
    lambdas = Inf
  )
    
  rejected_hypotheses_forest <- IHW::rejected_hypotheses(ihw_forest)
  
  effective_nbins_forest <- mean(unlist(IHW::nbins(ihw_forest)))
    }      
    
    tribble(
  ~stratification_method, ~rejections, ~dimension, ~effective_nbins,~p1,
  "forest", sum(rejected_hypotheses_forest),   dimension_i, effective_nbins_forest, mean(Hs_i),
  "quantile", sum(rejected_hypotheses_quantile),   dimension_i,  effective_nbins_quantile, mean(Hs_i)
  )
})



eval <- data.table::rbindlist(eval)

saveRDS(eval, paste0("data/",Sys.Date(),"noise_sim.Rds"))



eval %>%
  group_by(dimension, stratification_method) %>%
  summarise(rejections = mean(rejections)) %>%
  ggplot(aes(x = dimension, y = rejections, color = stratification_method)) +
  geom_line() +
  ylim(0, NA)

eval %>%
  group_by(dimension, stratification_method) %>%
  summarise(effective_nbins = mean(effective_nbins)) %>%
  ggplot(aes(x = dimension, y = effective_nbins, color = stratification_method)) +
  geom_line() 

