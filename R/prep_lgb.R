#' Prep for LightGBM training
#'
#' Creates the data matrix needed to construct a LightGBM dataset from the given
#' covariates 'x'
#'
#' @param dat dataframe
#' @param x vector of covariates i.e `c("DrivAge", "VehAge")`
#'
#' @return a data matrix
prep_lgb <- function(dat, x) {
  data.matrix(dat[, x, drop = FALSE])
}
