#' Rescale Distance Measures
#'
#' This function rescales distance measures so that 0 corresponds to a pre-specified value.
#' In the context of column-based IF analysis in cortical sections, this often corresponds to the ventricular
#' zone, VZ, such that the transformed distance measures are given as 'distance relative to the VZ'.
#'
#' @param df A dataframe.
#' @param distance_cols A vector containing strings that correspond to columns containing distance data (default: global variable 'distance_columns').
#' @param min_col A string that matches the column name of the dataframe column containing the lower bounds of the range (default: 'VZ_base').
#' @param suffix A string to append to the end of the names of the output vectors/columns in the dataframe (default: '_um').
#'
#' @return A dataframe with additional modified distance columns/numeric vectors appended.
#'
#' @examples
#' # Example with custom columns and suffix
#' rescale_distance(df = my_data, distance_cols = c("dist_col1", "dist_col2"), min_col = "Base_value", suffix = "_scaled")
#'
#' @export
#'
rescale_distance <- function(df,
                             distance_cols = distance_columns,
                             min_col = "VZ_base",
                             suffix = "_um") {

  # Ensure that specified columns exist in the dataframe
  if (!all(c(min_col, distance_cols) %in% colnames(df))) {
    stop("Specified columns do not exist in the dataframe.")
  }

  # Rescale each distance column
  rescale_col <- function(distance_col) {
    df[[distance_col]] - df[[min_col]]
  }

  cols_list <- lapply(setNames(distance_cols, distance_cols),
                      rescale_col)

  # Append new columns to the dataframe
  names(cols_list) <- paste0(names(cols_list),
                             suffix)

  dplyr::bind_cols(df, cols_list)
}

