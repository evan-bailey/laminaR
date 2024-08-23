#' Pairwise correlations of count data by groups
#'
#' Calculates pairwise correlation matrices for all marker combinations within specified groups using the
#' `correlate()` function from the `corrr` package. The input count data should be in wide format.
#'
#' @param df A dataframe containing grouping variables and count data in wide format.
#' @param groups A character vector specifying the names of the grouping columns.
#' @param marker_cols A character vector specifying the names of the count/marker columns.
#' @param cor.method The correlation method to be applied. One of "pearson" (Pearson correlation, default),
#'   "kendall" (Kendall's tau), or "spearman" (Spearman's rho).
#'
#' @return A dataframe with rows corresponding to the grouping structure and a column 'CorDF' containing nested
#'   pairwise correlation dataframes for each group.
#'
#' @details
#' This function calculates pairwise correlation matrices for each group specified in `groups` using the
#' specified `cor.method` (correlation method). It uses the `correlate()` function from the `corrr` package
#' to compute correlations between all combinations of marker columns within each group.
#'
#' @seealso
#'   The `correlate()` function from the `corrr` package:
#'   \url{https://github.com/tidymodels/corrr}
#'
#' @examples
#'   df <- data.frame(
#'     Species = rep(c("A", "B", "C"), each = 3),
#'     PH3 = c(10, 20, 15, 5, 25, 18, 8, 30, 22),
#'     Tbr2 = c(15, 25, 20, 7, 28, 21, 10, 35, 27),
#'     tdT = c(12, 22, 18, 8, 32, 24, 11, 38, 29)
#'   )
#'   pairwise_correlations_by_group(df, groups = "Species", marker_cols = c("PH3", "Tbr2", "tdT"))
#'
#' @export
pairwise_correlations_by_group <- function(df,
                                           groups = c("Species",
                                                      "Construct"),
                                           marker_cols = c("PH3",
                                                           "Tbr2",
                                                           "tdT",
                                                           "PH3 + Tbr2",
                                                           "PH3 + tdT",
                                                           "PH3 + Tbr2 + tdT",
                                                           "Tbr2 + tdT"),
                                           cor.method = "pearson") {

  df %>%
    dplyr::select(c(groups,
                    marker_cols)) %>%
    dplyr::group_by(across(groups)) %>%
    tidyr::nest() %>%
    dplyr::summarise(CorDF = purrr::map(data,
                          corrr::correlate,
                          use = "all.obs",
                          method = cor.method))

}
