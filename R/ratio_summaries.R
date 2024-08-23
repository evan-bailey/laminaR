#' Pairwise counts ratios
#'
#' Computes pairwise ratios for columns in a dataframe containing counts data. This function is particularly
#' useful for co-labelling analyses that require ratios of co-labelled cells.
#'
#' This function uses `dplyr` and `tidyselect` verbs and tools to process data from a dataframe
#' with the specified column names given as parameters. To compute the ratios in a pairwise fashion
#' between all unique pairs, `across2x` from Tim Tiefenbach's `dplyover` package is utilized.
#'
#' NOTE: This function requires the `dplyover` package from GitHub to be installed. Please ensure
#' you have an up-to-date version of `devtools` to install `dplyover`. For more information on
#' this package and function, visit the GitHub repository: [dplyover GitHub Repo](https://github.com/TimTeaFan/dplyover).
#'
#' @param df A dataframe containing counts data and grouping variables.
#' @param groups A vector of character column names to group by (default: c("Species", "Construct", "Animal_ID", "Section_No", "Position", "Column")).
#' @param marker_cols A vector of character column names with count data, typically produced by `count_summaries()`.
#' @param ratio_comb Argument passed to `across2x` from `dplyover`.
#'
#'
#' @return A dataframe containing pairwise ratios computed for all combinations of columns in `marker_cols`. Controls which combinations of columns
#'  are to be created.
#'
#'   - `"all"`, the default, will create all pairwise combinations between columns
#'   in `.xcols` and `.ycols` *including all permutations* (e.g.
#'   `foo(column_x, column_y)` as well as `foo(column_y, column_x)`.
#'   - `"unique"` will only create all unordered combinations (e.g. creates
#'   `foo(column_x, column_y)`, while `foo(column_y, column_x)` *will not* be created)
#'   - `"minimal` same as `"unique"` and further skips all self-matches (e.g.
#'   `foo(column_x, column_x)` *will not* be created)
#'
#' @export
#'
#' @references
#' For more information on `across2x` function from `dplyover` package, see
#' \href{https://timteafan.github.io/dplyover/reference/across2.html}{dplyover::across2x}
#'
#' @seealso
#' Use `dplyr` verbs like \code{\link[dplyr]{group_by}} and \code{\link[dplyr]{summarise}}.
#' Utilize `tidyselect` functions such as \code{\link[tidyselect]{all_of}}.
#'
ratios_summaries <- function(df,
                             groups = c("Species",
                                        "Construct",
                                        "Animal_ID",
                                        "Section_No",
                                        "Position",
                                        "Column"),
                             marker_cols = c("PH3",
                                             "Tbr2",
                                             "tdT",
                                             "PH3 + Tbr2",
                                             "PH3 + tdT",
                                             "PH3 + Tbr2 + tdT"),
                             ratio_comb = "unique") {

  if (length(marker_cols) < 2) {
    stop("Multiple marker columns must be selected in order to compute pairwise ratios.")
  }

  # Check if dplyover package is available
  if (!requireNamespace("dplyover", quietly = TRUE)) {
    stop("The `dplyover` package is required but not installed. Please install `dplyover` from the GitHub repository. https://github.com/TimTeaFan/dplyover")
  }

  if (!(ratio_comb %in% c("all", "unique", "minimal"))) {
    stop("ratio_comb must be one of 'all', 'unique' or 'minimal'.  See ?across2 for details")
  }

  df %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(groups))) %>%
    dplyr::summarise(dplyover::across2x(.xcols = tidyselect::all_of(marker_cols),
                                        .ycols = tidyselect::all_of(marker_cols),
                                        .fns = ~ .x / .y,
                                        .names = "{xcol}_by_{ycol}",
                                        .comb = "unique"))
}

