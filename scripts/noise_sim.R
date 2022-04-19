## -----Parameters------
m <- 1e4
r <- 200
r <- 1 # number of monte carlo replicates, increases run time immensely!
alpha <- .1

folds_fdp_eval <- sample(1:3, m, replace = TRUE)

forest_par <- list(
  ntrees = 3,
  ntaus = 10,
  nsplit = 5,
  maxdepth = 3,
  nodesize = "auto"
)
dimensions <- seq(from = 2, to = 6, by = 2)
#dimensions <- 2

## ------Simulation------
prop_alt <- function(cov_row) {
  r <- sum(cov_row^2)
  # exp(-5*r)
  # 1 / (1 + exp(-cov1))
  # 1 / (1 + exp(-1 * (3 * cov_row[1]+cov_row[2] - 5)))
  # browser()
  # ifelse(sum(cov_row^2) <= 0.5, 0.9, 0)
  # ifelse(r <= 0.1, 0.9, 0)
  # 1 / (1 + exp(-cov_row[1]))
  # ifelse(sum(cov_row^2)  <= 1, 0.02, 0.4)
  # ifelse(cov_row[1]^2+cov_row[2]^2  <= 1, 0.02, 0.4)
  ifelse(cov_row[1] <= 0.1, 0.9, 0)
  # ifelse(r <= 0.1, 0.9, 0) #This works well
}

sim_combs <- expand.grid(
  m = m,
  dimensions = dimensions,
  seed = seq_len(r)
)

# simple_sim <- foreach(i = seq_len(r)) %dorng% {
simple_sim <- lapply(seq_len(nrow(sim_combs)), function(i) {
  dimension_i <- sim_combs$dimensions[i]
  covariate_i <- matrix(runif(m * dimension_i, 0, 1), nrow = m)
  prop_alt_i <- apply(covariate_i, 1, prop_alt)
  Hs_i <- rbinom(m, size = 1, prop_alt_i)

  # mus <- pmax(1.3, sqrt(covariate_i[,c(1,2),drop = F]) %*% c(1,1)*1.5)
  # mu_alphas <- 1/mus
  # pvalue_i <- stats::runif(m)*(1-Hs_i) + stats::rbeta(m, mu_alphas, 1)*Hs_i

  pvalue_i <- ifelse(Hs_i,
    rbeta(m, 0.25, 1),
    runif(m)
  )

  return(list(covariate = covariate_i, prop_alt = prop_alt_i, Hs = Hs_i, pvalue = pvalue_i))
})

## -------evaluate-----
eval <- lapply(seq_len(nrow(sim_combs)), function(i){
#eval <- foreach(i = seq_len(nrow(sim_combs))) %dorng% {
#i <- 1
  print(paste0("simulation run:", i))
  simple_sim_i <- simple_sim[[i]]
  dimension_i <- sim_combs$dimensions[i]
  seed <- sim_combs$seed[i]

  Ps_i <- simple_sim_i$pvalue
  Xs_i <- simple_sim_i$covariate
  Hs_i <- simple_sim_i$Hs

  sim_res_i <- run_sim(Ps_i, Xs_i, Hs_i, folds_fdp_eval, seed, alpha = 0.1, m = m, lfdr_only = FALSE, forest_par)

  mutate(sim_res_i, dimension = dimension_i)
})
eval <- bind_rows(eval)

saveRDS(eval, file = "precomputed_results/noise_sim.Rds")
