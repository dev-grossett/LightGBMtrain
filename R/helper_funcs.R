#' Prep for LightGBM training
#'
#' Creates the data matrix needed to construct a LightGBM dataset from the given
#' covariates 'x'
#'
#' @param dat dataframe
#' @param x vector of covariates i.e `c("DrivAge", "VehAge")`
#'
#' @return a data matrix
#' @export
prep_lgb <- function(dat, x) {
  data.matrix(dat[, x, drop = FALSE])
}

#' Plots exposures and frequency by given variables
#'
#' @param dat input data set
#' @param vars vector of variable names in character format i.e
#' `c("var1, "var2")`
#' @param claim_var column that represents the number of claims for an obs
#' @param exposure_var column that represents the number of exposures for an obs
#'
#'
#' @return ggplots of exposures and frequency, grouped by the variables in vars
#' @export
multi_plot <- function(dat, vars, claim_var = "ClaimNb",
                       exposure_var = "Exposure") {

  # Some initializing
  Claims <- NULL
  Exposures <- NULL
  Freq <- NULL

  # Loop thorugh variables to plot
  for (i in 1:length(vars)) {
    var = vars[i]

    claims <- rlang::ensym(claim_var)
    exposures <- rlang::ensym(exposure_var)

    # Group by current variable
    new_dat <- dat %>%
      dplyr::group_by(!! rlang::ensym(var)) %>%
      dplyr::summarise(Claims = sum((!! claims)),
                       Exposures = sum(!! exposures)) %>%
      dplyr::mutate(Freq = Claims / Exposures)

    # Plot exposures
    p1 <- ggplot2::ggplot(new_dat, ggplot2::aes(x = !! rlang::ensym(var),
                                                weight = Exposures)) +
      ggplot2::geom_bar() +
      ggplot2::ggtitle(var)

    # Plot Frequency
    p2 <- ggplot2::ggplot(new_dat, ggplot2::aes(x = !! rlang::ensym(var),
                                                y = Freq)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth()

    # Plot both on grid
    gridExtra::grid.arrange(p1, p2, nrow = 1)
  }
}

#' LightGBM training score
#'
#' Calculates the training score of a LightGBM model using n-fold cross
#' validation. Useful for tuning hyperparameters
#'
#' @param training_data Dataset to train model on
#' @param num_leaves Maximum number of leaves in one tree
#' @param min_data_in_leaf Minimum number of observations one leaf can be based
#'  off
#' @param num_trees Number of boosting iterations
#' @param learning_rate Shrinkage rate - determines how quickly the ensemble
#' learns
#' @param bagging_freq Frequency for bagging (boostrap aggregation)
#' @param bagging_fraction Proportion of data to use for training each tree
#' @param feature_fraction Proportion of variables to use for training each tree
#' @param objective Loss function - see LightGBM docs for full list available
#' @param nfolds number of folds for cross validation score
#' @param early_stopping_rounds number of rounds to stop if no improvement in
#' cross validation score
#'
#' @return List containing the Best Score against the `objective` and how many
#' iterations based on early stopping
#' @export
scoring_function <- function(training_data,
                             num_leaves = 2L,
                             min_data_in_leaf = 20L,
                             num_trees = 4000L,
                             learning_rate = 0.01,
                             bagging_freq = 1L,
                             bagging_fraction = 0.5,
                             feature_fraction = 0.6,
                             objective = "poisson",
                             nfolds = 3L,
                             early_stopping_rounds = 100L) {

  # Generate parameter list for LightGBM
  pars <- list(num_leaves = num_leaves,
               min_data_in_leaf = min_data_in_leaf,
               num_trees = num_trees,
               learning_rate = learning_rate,
               bagging_freq = bagging_freq,
               bagging_fraction = bagging_fraction,
               feature_fraction = feature_fraction,
               objective = objective,
               deterministic = TRUE) #attempting to make reproducible

  # Train using cross validation
  lgb_cv <- lightgbm::lgb.cv(params = pars,
                             data = training_data,
                             nfold = nfolds,
                             verbose = -100,
                             # early stopping instead of explicitly training for
                             # num_trees
                             early_stopping_rounds = early_stopping_rounds)

  # Return results
  return(list(Score = lgb_cv$best_score,
              Trees = lgb_cv$best_iter))
}

#' Parallelized random search
#'
#' @param bounds Lower and Upper bounds for selected hyperparameters. See
#' `scoring_function()` docs for params to train
#' @param niter Number of random search iterations to perform
#' @param training_data Training data for LightGBM model
#' @param n_cores How many cores to split search across
#'
#' @return DataFrame of results
#' @export
#'
#' @importFrom foreach %dopar%
random_search_par <- function(bounds, niter, training_data,
                              n_cores = NULL) {

  i <- NULL

  #set up datatable of random hyperparameters within bounds specified
  df_vars <- data.frame(id = seq(1, niter, 1))

  # For each hyperparameter, generate random point withing bounds using runif
  # for continuous params, or rdunif for discrete params. If bounds for that
  # parameter is of length 1, return the value instead.
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
