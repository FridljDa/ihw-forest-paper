
#' @import stats
#' @import randomForestSRC
get_forest <- function(pvalues, covariates, folds, ntrees = 2, n_censor_thres = 2, nodedepth = 4, nodesize = 1000, mtry = "auto", seed = 1) {
  m <- length(pvalues)
  nfolds <- length(unique(folds))
  
  if (mtry == "auto") mtry <- ceiling(0.9 * ncol(covariates)) # a lot of noise data => high mtry
  
  nodesize <- as.integer(nodesize)
  mtry <- as.integer(mtry)
  nodedepth <- as.integer(nodedepth)
  
  pvalues_boundaries <- range(pvalues)
  
  forests <- lapply(seq_len(nfolds), function(i) {
    pvalues_other_folds <- pvalues[folds != i]
    #remove boundaries from p-values
    pvalues_other_folds <- pvalues_other_folds[!pvalues_other_folds %in% pvalues_boundaries]
    
    # get quantile breaks, remove trivial tail and head
    quantile_seq <- seq(0, 1, length.out = n_censor_thres + 2)[2:(n_censor_thres + 1)]
    
    forest_i <- lapply(quantile_seq, function(quantile_seq_i) {
      # get represantative quantile breaks for he aus for good coverage
      tau <- stats::quantile(pvalues_other_folds, quantile_seq_i)
      # binary indicator from Boca and leek/storey
      data <- data.frame(
        indic = (pvalues >= tau),
        covariates = covariates
      )
      data_other_folds <- data[folds != i, ]
      
      # grow forest based on other folds
      forest_other_fold <- randomForestSRC::rfsrc(
        indic ~ . - indic,
        data = data_other_folds,
        ntree = ntrees,
        mtry = mtry,
        nodesize = nodesize,
        nodedepth = nodedepth,
        splitrule = "mse",
        block.size = FALSE,
        forest.wt = FALSE,
        seed = seed
      )
      
      quantile_seq_i_round <- round(100 * quantile_seq_i, 0)
      #forest_other_fold
      name_subforest <- paste0(quantile_seq_i_round, "%_tree")
      
      assign(name_subforest, forest_other_fold)
      
      return(forest_other_fold)
    })
    
    forest_i
  })
  
  return(forests)
}

#' @import stats
predict_group_forest <- function(data, forests){
  groups <- imap(forests, 
                 function(forests_i, i){
                   groups_i <- imap(forests_i,
                                    function(forests_i_j, j){
                                      predict_groups <- stats::predict(forests_i_j, data, membership = TRUE)
                                      
                                      groups_i_j <- predict_groups$membership
                                      
                                      return(groups_i_j)
                                    })
                   names(groups_i) <- paste0("fold", i, "_", names(groups_i))
                   return(groups_i)
                 })
  return(groups)
}


#' @import purrr
get_weights_trees_individual <- function(ihw_forest, groups){
  ind_i <- seq_along(groups)
  weights <- map(
    ind_i,
    function(i){
      ind_i_j <- seq_along(groups[[i]])
      map(
        ind_i_j,
        function(j){
          groups_i_j <- as.integer(groups[[i]][[j]])
          ihw_forest@weight_matrices_forest[[i]][[j]][groups_i_j]  
        }
      )
    }
  )
  weights
}

get_weight_forest_averaged <- function(folds, weights_indiv_trees){
  weight_sum <- imap_dbl(folds,
                          function(fold_i, i){
                            weights_indiv_trees[[fold_i]][[1]][i]
                            #TODO average out from different trees
                          })
  weight_sum
}


#get weights from forest 
get_weight_forest_averaged_full <- function(pvalues, 
                                covariates, 
                                folds, 
                                #new_covariates, #TODO
                                ntrees = 2, 
                                n_censor_thres = 2, 
                                nodedepth = 4, 
                                nodesize = 1000, 
                                mtry = "auto", 
                                seed = 1){
  
  
  forests <- get_forest(pvalue, as.matrix(X), folds = folds, 
                        ntrees = ntrees, n_censor_thres = n_censor_thres, 
                        nodedepth = nodedepth, seed = seed)
  
  ihw_forest <- ihw(pvalue, as.matrix(X), alpha = 0.1, stratification_method = "forest", 
                    folds = folds, ntrees = ntrees, n_censor_thres = n_censor_thres, 
                    nodedepth = nodedepth, nodesize = nodesize, seed = seed)

  
  data <- data.frame(covariates = covariates)
  
  groups <- predict_group_forest(data, forests)
  
  weights1_unsum <- get_weights_trees_individual(ihw_forest, groups)
  
  weight1_sum <- get_weight_forest_averaged(folds = folds, weights1_unsum)
}

###-----quantile----
#TODO delete
get_weight_quantile <- function(ihw_quantile, new_data, new_fold, seed = 1){
  weight_matrix <- ihw_quantile@weights
  nbins <- ihw_quantile@nbins
  groups_quantile <- IHW::groups_by_filter_multivariate(as.matrix(new_data), nbins, seed = seed)
  
  weight <- purrr::map2_dbl(new_fold, groups_quantile, 
                            function(folds_i, groups_quantile_i){
                              weight_matrix[groups_quantile_i,folds_i]
                            })
  weight
}

groups_by_filter_multivariate_eval <- function(covariates_train, covariates_eval, nbins) {
  nvar <- ncol(covariates_train)
  nbin_dim <- max(1, floor(nbins ^ (1 / nvar))) #does not change anything, if nvar = 1
  
  groups <- lapply(seq_len(nvar), function(i) {
    covariate_train_i <- covariates_train[, i, drop = TRUE]
    covariates_eval_i <- covariates_eval[, i, drop = TRUE]
    
    breaks <- quantile(covariate_train_i, probs = seq(0, 1, length.out = nbin_dim+1), na.rm = TRUE)
    #add interval open to the right
    
    covariates_eval_i_binned <- cut(covariates_eval_i, breaks, include.lowest = TRUE, right = FALSE)
  })
  
  if(nvar == 1){
    groups <- unname(unlist(groups))
  }else{
    groups <- do.call(cbind, groups)
    groups <- apply(groups, 1, paste, collapse = "-") # base R equivalent of tidyr::unite
  }
  # convert to factor
  groups <- as.factor(groups)
  
  groups
}

plot_weights_quantile_2d <- function(ihw_quantiles, m_eval = 100,  fold = 1){
  ihw_quantiles_df <- ihw_quantiles@df
  
  #build grid to evaluate for plotting
  data_eval <- expand.grid(
    covariate.1 = seq(min(ihw_quantiles_df$covariate.1), max(ihw_quantiles_df$covariate.1), length.out = m_eval),
    covariate.2 = seq(min(ihw_quantiles_df$covariate.2), max(ihw_quantiles_df$covariate.2), length.out = m_eval)
  )
  
  covariates_train <- ihw_quantiles_df %>% select("covariate.1","covariate.2")
  
  data_eval$groups <- groups_by_filter_multivariate_eval(
    covariates_train = covariates_train, 
    covariates_eval = data_eval, 
    nbins = ihw_quantiles@nbins
  )
  
  #relevel group labels
  levels(data_eval$groups) <- seq_len(nlevels(data_eval$groups))
  
  #obtain weights
  data_eval$weight <- sapply(data_eval$groups, function(group_i){
    ihw_quantiles@weights[group_i,fold]
  })
  
  #plot everything
  plot <- data_eval %>%
    ggplot(aes(x = covariate.1, y = covariate.2, color = weight))+
    geom_point()  +
    scale_colour_gradientn(colours = myPalette(100))
  
  plot
}