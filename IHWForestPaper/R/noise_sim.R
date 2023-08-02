library(doRNG)
library(doParallel)
library(parallel)

## ------Simulation------
prop_alt <- function(cov_row) {
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
  #ifelse(cov_row[1] <= 0.1, 0.9, 0)
 # pi1s <- ifelse(Xs[ ,1] <= 0.1, 0.9, 0)
   #ifelse(r <= 0.1, 0.9, 0) #This works well
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

## -------evaluate-----
#library(doRNG) #TODO
#library(doParallel)

#' @import doRNG
#' @import doParallel
#' @import parallel
#' @export
eval_noise_sim <- function(m, r, dimensions, forest_par, alpha = 0.1, methods = c("IHW-quantile", "IHW-forest", "IHW-forest-drop-inbag", "BH", "AdaPT", "Boca-Leek", "Clfdr-EM"), null_proportion = T){
  sim <- noise_sim(m, r, dimensions)
  n.cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores = min(3, n.cores - 1))
  
  #eval <- lapply(seq_along(sim), function(i){
  eval <- foreach(i = seq_along(sim)) %dorng% {
    #i <- 1
    print(paste0("simulation run:", i))
    sim_i <- sim[[i]]
    dimension_i <- sim_i$dimension
    seed_i <- sim_i$seed
    
    Ps_i <- sim_i$pvalue
    Xs_i <- sim_i$covariate
    Hs_i <- sim_i$Hs
    
    sim_res_i <- run_sim(Ps_i, Xs_i, Hs_i, seed_i, alpha, m = m, methods = methods, forest_par, null_proportion = null_proportion)
    
    mutate(sim_res_i, dimension = dimension_i)
  }
  eval <- bind_rows(eval)
}
