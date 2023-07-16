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
