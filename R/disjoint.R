#' @title Multiple scaling -- the disjoint way
#'
#' @description \code{disjoint()} returns a multiple, disjointedly scaled
#'   version of the specified data frame. This so called \code{msdf} sets up the
#'   building block for further analysis with \code{overlap()} (type
#'   \code{?overlap}).
#'
#' @param df a data frame (with more than two items and unique, non-\code{NA}
#'   column names).
#'
#' @param mrit_min a numeric constant of length 1 to specify the marginal
#'   corrected item-total correlation. Its value is in the range of 0-1. The
#'   default is set to \code{.3}.
#'
#' @param negative_too a logical constant indicating whether reversed items are
#'   included in the analysis. The default is set to \code{FALSE}.
#'
#' @param sclvals a numeric vector of length 2 indicating the start- and
#'   endpoint of a scale. Use something like \code{c(min,max)}.
#'
#' @param use an optional string to specify how missing values enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details. The
#'   default is set to \code{pairwise.complete.obs}.
#'
#' @details \code{use} clarifies how to set up a correlation matrix in the
#'   presence of missing values. In a typical scaling process this happens at
#'   least twice. First, when determining the core items (the two items in the
#'   correlation matrix with the highest linear relationship). Second, when an
#'   item is proposed for an emerging scale.
#'
#'   Note that \code{disjoint()} uses \code{\link[stats]{cor}}'s default method
#'   \code{pearson}.
#'
#' @return A multiple scaled data frame (\code{msdf}).
#'
#' @references Müller-Schneider, T. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie, 30(4), 305-315.
#'   https://doi.org/10.1515/zfsoz-2001-0404
#'
#' @examples
#' # Use only positive correlations
#' disjoint(mtcars, mrit_min = .4)
#'
#' # Include negative correlations
#' disjoint(mtcars, mrit_min = .4, negative_too = TRUE, sclvals = c(1,7))
#'
#' # Change the treatment of missing values
#' disjoint(mtcars, mrit_min = .4, use = "all.obs")

#' @export
disjoint <- function(df, mrit_min = .3, negative_too = FALSE,
                     sclvals = NULL, use = "pairwise.complete.obs") {
  check_df(df)
  check_mrit(mrit_min)
  check_neg(negative_too)
  check_comp(df, mrit_min, use)
  if (negative_too) {
    check_sclvals(sclvals)
    scls <- disj_nci(df, mrit_min, sclvals, use)
    new_msdf(scls, df = match.call()$df, method = "disjoint",
                mrit_min = mrit_min, negative_too = TRUE, sclvals = sclvals)
    }else{
      scls <- disj_pci(df, mrit_min, use)
      new_msdf(scls, df = match.call()$df, method = "disjoint",
               mrit_min = mrit_min, negative_too = FALSE)
    }
}
