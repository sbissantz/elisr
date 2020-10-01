#' @title Multiple scaling in a disjoint manner
#'
#' @description \code{disjoint} returns a multiple, disjoint scaled version of
#'   the specified data frame. This so called \code{muscldf} is nothing more
#'   than a list of scales (in this case genuine subsets of that data frame).
#'
#' @param df a data frame object.
#'
#' @param rit_min a numerical constant to specify the (corrected item total)
#'   correlation. The value of this lower bound must in the range of 0 to 1. The
#'   default is set to \code{.3}.
#'
#' @param negative_too a logical constant indicating whether negatively
#'   correlating items should be included in the scaling process. The default is
#'   set to \code{FALSE}.
#'
#' @param sclvals a numerical vector of length 2 indicating the first and the
#'   full scale value. Consider using the shape \code{c(min,max)}.
#'
#' @param use an optional string indicating how to deal with missing values if
#'   necessary. See \code{use} in \code{\link[stats]{cor}} for details. The
#'   default is set to \code{"pairwise.complete.obs"}.
#'
#' @details The \code{use} argument takes control over the treatment of missing
#'   values when correlation matrices are build. In a scaling process this
#'   happens at least twice: first when determining the core (the two items of
#'   the correlation matrix with the highest linear relationship), and second
#'   when an item is considered to be part of this scale.
#'
#' @references Müller-Schneider, Thomas. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie. 30. 10.1515/zfsoz-2001-0404.
#'
#' @examples
#' # Using positive correlations (and `pairwise.complete.obs`)
#' disjoint(mtcars, rit_min = .4)
#'
#' # Including negative correlations (and `pairwise.complete.obs`)
#' disjoint(mtcars, rit_min = .4, negative_too = TRUE, sclvals = c(1,7))
#'
#' # Changing the treatment of missing values
#' disjoint(mtcars, rit_min = .4, use = "all.obs")

disjoint <- function(df, rit_min = .3, negative_too = FALSE,
                     sclvals = NULL, use = "pairwise.complete.obs") {

# Checks ------------------------------------------------------------------

  check_df(df)
  check_rit(rit_min)
  check_neg(negative_too)

# Procedure & Options -----------------------------------------------------

  if (negative_too) {
    check_sclvals(sclvals)
    scls <- disj_nci(df, rit_min, sclvals, use)
    new_muscldf(scls, df = match.call()$df, method = "disjoint",
                rit_min = rit_min, negative_too = TRUE, sclvals = sclvals)
    }else{
      scls <- disj_pci(df, rit_min, use)
      new_muscldf(scls, df = match.call()$df, method = "disjoint",
               rit_min = rit_min, negative_too = FALSE)
  }
}
