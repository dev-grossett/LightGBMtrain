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
#'
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
