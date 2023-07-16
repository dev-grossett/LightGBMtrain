#' Parallelized random search
#'
#' @param bounds Lower and Upper bounds for selected hyperparameters. See
#' `scoring_function()` docs for params to train
#' @param niter Number of random search iterations to perform
#' @param training_data Training data for LightGBM model
#' @param n_cores How many cores to split search across
#'
#' @return DataFrame of results
#'
#' @importFrom foreach %dopar%
random_search_par <- function(bounds, niter, training_data,
                              n_cores = NULL) {

  #set up datatable of random hyperparameters within bounds specified
  df_vars <- data.frame(id = seq(1, niter, 1))

  df_vars <- df_vars %>%
    dplyr::mutate(
      num_leaves =
        if (length(bounds$num_leaves) > 1) {
          extraDistr::rdunif(niter, bounds$num_leaves[1],
                             bounds$num_leaves[2])
        } else { bounds$num_leaves },

      min_data_in_leaf =
        if (length(bounds$min_data_in_leaf) > 1) {
          extraDistr::rdunif(niter, bounds$min_data_in_leaf[1],
                             bounds$min_data_in_leaf[2])
        } else { bounds$min_data_in_leaf },

      num_trees =
        if (length(bounds$num_trees) > 1) {
          extraDistr::rdunif(niter, bounds$num_trees[1], bounds$num_trees[2])
        } else { bounds$num_trees },

      learning_rate =
        if (length(bounds$learning_rate) > 1) {
          stats::runif(niter, bounds$learning_rate[1], bounds$learning_rate[2])
        } else { bounds$learning_rate },

      bagging_freq =
        if (length(bounds$bagging_freq) > 1) {
          extraDistr::rdunif(niter, bounds$bagging_freq[1],
                             bounds$bagging_freq[2])
        } else { bounds$bagging_freq },

      bagging_fraction =
        if (length(bounds$bagging_fraction) > 1) {
          stats::runif(niter, bounds$bagging_fraction[1],
                       bounds$bagging_fraction[2])
        } else { bounds$bagging_fraction },

      feature_fraction =
        if (length(bounds$feature_fraction) > 1) {
          stats::runif(niter, bounds$feature_fraction[1],
                       bounds$feature_fraction[2])
        } else { bounds$feature_fraction },

      objective =
        if (length(bounds$objective) > 1) {
          bounds$objective[extraDistr::rdunif(niter, 0,
                                              length(bounds$objective))]
        } else { bounds$objective }
    )

  #set up parallel cluster
  if (missing(n_cores)) {
    n_cores <- parallel::detectCores() - 2
  }
  cluster <- parallel::makeCluster(n_cores, type = "PSOCK")
  doParallel::registerDoParallel(cl = cluster)
  parallel::clusterExport(cl = cluster, c("scoring_function",
                                          deparse(substitute(training_data))))
  parallel::clusterEvalQ(cluster, expr = {library(lightgbm)})

  #run through random search in parallel
  results <- foreach::foreach(i = 1:niter, combine = rbind) %dopar% {
    scoring_function(training_data,
                     num_leaves = df_vars$num_leaves[i],
                     min_data_in_leaf = df_vars$min_data_in_leaf[i],
                     num_trees = df_vars$num_trees[i],
                     learning_rate = df_vars$learning_rate[i],
                     bagging_freq = df_vars$bagging_freq[i],
                     bagging_fraction = df_vars$bagging_fraction[i],
                     feature_fraction = df_vars$feature_fraction[i],
                     objective = df_vars$objective[i])
  }

  #stop parallel cluster
  parallel::stopCluster(cl = cluster)

  #output results
  df <- cbind(df_vars, as.data.frame(do.call(rbind, results)))

  return(df)
}
