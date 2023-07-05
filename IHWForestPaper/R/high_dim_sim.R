library(doRNG)
library(doParallel)
library(parallel)


inv_logit <- function(x) {exp(x) / (1 + exp(x))}

high_dim_sim <- function(m, r, dimensions){
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
    covariate_i <- matrix(runif(m_i * dimension_i), nrow = m_i)
    
    #
    pi1 <- 0.1
    beta.pi <- c(3, 3, rep(0, dimension_i-2))
    beta0.pi <- uniroot(function(b){
      mean(inv_logit(covariate_i %*% beta.pi + b)) - pi1
    }, c(-100, 100))$root
    pi <- inv_logit(covariate_i %*% beta.pi + beta0.pi)
    beta.mu <- c(2, 2, rep(0, dimension_i-2))
    beta0.mu <- 0
    mu <- pmax(1, covariate_i %*% beta.mu + beta0.mu)
    #'
    #' # Generate p-values
    Hs_i <- as.logical(ifelse(runif(m) < pi, 1, 0))
    y <- ifelse(Hs_i, rexp(m, 1/mu), rexp(m, 1))
    pvalue_i <- exp(-y)
    prop_alt_i <- mean(Hs_i)
    
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
eval_high_dim_sim <- function(m, r, dimensions, forest_par, alpha = 0.1, lfdr_only = FALSE, null_proportion = T){
  sim <- high_dim_sim(m, r, dimensions)
  n.cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores = min(3, n.cores - 1))

  #eval <- lapply(seq_along(sim), function(i){
  eval <- foreach(i = seq_along(sim), .combine = rbind) %dorng% {
    #i <- 1
    print(paste0("simulation run:", i))
    sim_i <- sim[[i]]
    dimension_i <- sim_i$dimension
    seed_i <- sim_i$seed
    
    Ps_i <- sim_i$pvalue
    Xs_i <- sim_i$covariate
    Hs_i <- sim_i$Hs
    
    sim_res_i <- run_sim(Ps_i, Xs_i, Hs_i, seed_i, alpha, m = m, lfdr_only = lfdr_only, forest_par, null_proportion = null_proportion)
    
    mutate(sim_res_i, dimension = dimension_i)
  }
  eval <- bind_rows(eval)
}

#' @import doRNG
#' @import doParallel
#' @import parallel
#' @export
eval_high_dim_sim_param <- function(
    m,
    r,
    dimensions,
    tau = 0.5,
    ntrees = 10,
    nodedepth = 3,
    nodesize = 1000,
    alpha = 0.1,
    null_proportion = T
){
  
  forest_param_grid <- data.frame(
    tau = tau,
    #n_censor_thres = 1,
    ntrees = ntrees,
    nodesize = nodesize
  )
  
  
  sim <- high_dim_sim(m, r, dimensions)
  n.cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores = min(6, n.cores))

  eval <- foreach(i = seq_along(sim), .combine = rbind) %do% {#TODO #dorng
    #i <- 1
      #j <- 1
            foreach(j = seq_len(nrow(forest_param_grid)),
                   .combine = rbind) %do% { 
                     
                   
      print(paste0("simulation run i: ", i,"/",length(sim),", j: ", j,"/ ",nrow(forest_param_grid)))
      sim_i <- sim[[i]]
      forest_param_i <- as.list(forest_param_grid[j,])
      dimension_i <- sim_i$dimension
      seed_i <- sim_i$seed
      
      Ps_i <- sim_i$pvalue
      Xs_i <- sim_i$covariate
      Hs_i <- sim_i$Hs
      
      ihw_forest <- IHW::ihw(Ps_i, Xs_i, alpha = 0.1,
                             stratification_method = "forest", null_proportion = F,
                             ntrees = forest_param_i$ntrees, 
                             #n_censor_thres = forest_par$n_censor_thres, 
                             #nodedepth = forest_par$nodedepth,
                             tau = forest_param_i$tau,
                             nodesize = forest_param_i$nodesize, 
                             #nodedepth = nodedepth,
                             #nodesize = nodesize,
                             lambdas = Inf
      )
      
      rej <- IHW::rejected_hypotheses(ihw_forest)
      
      sim_res_i <-  fdp_eval(Hs_i, rej) 
      
      sim_res_i <- sim_res_i %>% 
                    mutate(method="IHW-forest",
                           seed = seed_i,
                           pi0s = mean(1-Hs_i),
                           dimension = dimension_i,
                           m = m)
      
      sim_res_i <- cbind(sim_res_i, forest_param_grid[j,], row.names = NULL)
      sim_res_i
    }
  }
  
  #eval <- bind_rows(eval)
  eval
}
