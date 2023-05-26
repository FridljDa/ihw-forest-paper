
# stats

#get_forest <- function(pvalues, covariates, folds, ntrees = 2, n_censor_thres = 2, nodedepth = 4, nodesize = 1000, mtry = "auto", seed = 1) {
#  m <- length(pvalues)
#  nfolds <- length(unique(folds))
#  
#  if (mtry == "auto") mtry <- ceiling(0.9 * ncol(covariates)) # a lot of noise data => high mtry
  
#  nodesize <- as.integer(nodesize)
#  mtry <- as.integer(mtry)
#  nodedepth <- as.integer(nodedepth)
#  
#  pvalues_boundaries <- range(pvalues)
#  
#  forests <- lapply(seq_len(nfolds), function(i) {
#    pvalues_other_folds <- pvalues[folds != i]
    #remove boundaries from p-values
#    pvalues_other_folds <- pvalues_other_folds[!pvalues_other_folds %in% pvalues_boundaries]
    
    # get quantile breaks, remove trivial tail and head
#    quantile_seq <- seq(0, 1, length.out = n_censor_thres + 2)[2:(n_censor_thres + 1)]
    
#    forest_i <- lapply(quantile_seq, function(quantile_seq_i) {
      # get represantative quantile breaks for he aus for good coverage
#      tau <- stats::quantile(pvalues_other_folds, quantile_seq_i)
      # binary indicator from Boca and leek/storey
#      data <- data.frame(
#        indic = (pvalues >= tau),
#        covariates = covariates
#      )
#      data_other_folds <- data[folds != i, ]
      
      # grow forest based on other folds
#      forest_other_fold <- randomForestSRC::rfsrc(
#        indic ~ . - indic,
#        data = data_other_folds,
#        ntree = ntrees,
#        mtry = mtry,
#        nodesize = nodesize,
#        nodedepth = nodedepth,
#        splitrule = "mse",
#        block.size = FALSE,
#        forest.wt = FALSE,
#        seed = seed
#      )
      
#      quantile_seq_i_round <- round(100 * quantile_seq_i, 0)
      #forest_other_fold
#      name_subforest <- paste0(quantile_seq_i_round, "%_tree")
      
#      assign(name_subforest, forest_other_fold)
      
#      return(forest_other_fold)
#    })
    
#    forest_i
#  })
  
#  return(forests)
#}

# stats
#predict_group_forest <- function(data, forests){
#  groups <- imap(forests, 
#                 function(forests_i, i){
#                   groups_i <- imap(forests_i,
#                                    function(forests_i_j, j){
#                                      predict_groups <- stats::predict(forests_i_j, data, membership = TRUE)
                                      
#                                      groups_i_j <- predict_groups$membership
                                      
#                                      return(groups_i_j)
#                                    })
#                   names(groups_i) <- paste0("fold", i, "_", names(groups_i))
#                   return(groups_i)
#                 })
#  return(groups)
#}


#'  purrr
#get_weights_trees_individual <- function(ihw_forest, groups){
#  ind_i <- seq_along(groups)
#  weights <- map(
#    ind_i,
#    function(i){
#      ind_i_j <- seq_along(groups[[i]])
#      map(
#        ind_i_j,
#        function(j){
#          groups_i_j <- as.integer(groups[[i]][[j]])
#          ihw_forest@weight_matrices_forest[[i]][[j]][groups_i_j]  
#        }
#      )
#    }
#  )
#  weights
#}

#get_weight_forest_averaged <- function(folds, weights_indiv_trees){
#  weight_sum <- imap_dbl(folds,
#                          function(fold_i, i){
#                            weights_indiv_trees[[fold_i]][[1]][i]
#                            #TODO average out from different trees
#                          })
#  weight_sum
#}


#get weights from forest 
#get_weight_forest_averaged_full <- function(pvalues, 
#                                covariates, 
#                                folds, 
#                                #new_covariates, #TODO
#                                ntrees = 2, 
#                                n_censor_thres = 2, 
#                                nodedepth = 4, 
#                                nodesize = 1000, 
#                                mtry = "auto", 
#                                seed = 1){
  
  
#  forests <- get_forest(pvalue, as.matrix(X), folds = folds, 
#                        ntrees = ntrees, n_censor_thres = n_censor_thres, 
#                        nodedepth = nodedepth, seed = seed)
#  
#  ihw_forest <- ihw(pvalue, as.matrix(X), alpha = 0.1, stratification_method = "forest", 
#                    folds = folds, ntrees = ntrees, n_censor_thres = n_censor_thres, 
#                    nodedepth = nodedepth, nodesize = nodesize, seed = seed)

  
#  data <- data.frame(covariates = covariates)
  
#  groups <- predict_group_forest(data, forests)
  
#  weights1_unsum <- get_weights_trees_individual(ihw_forest, groups)
#  
#  weight1_sum <- get_weight_forest_averaged(folds = folds, weights1_unsum)
#}