#' Clip Values to Limits
#'
#' This function clips values within a specified range for a given distance column in a dataframe.
#'
#' @param df A dataframe.
#' @param distance_col A string that matches a column name containing distance data.
#' @param min_limit_col A string that matches a column name containing the lower bounds to clip to (default: "VZ_base").
#' @param max_limit_col A string that matches a column name containing the upper bounds to clip to (default: "Cortex_top").
#'
#' @return A numeric vector of equal length to distance_col.
#' @export
#'
#' @examples
#' clip_to_limits(df = counts_with_sections_data,
#'                distance_col = "your_distance_column",
#'                min_limit_col = "your_min_limit_column",
#'                max_limit_col = "your_max_limit_column")
#'
clip_to_limits <- function(df,
                           distance_col,
                           min_limit_col = "VZ_base",
                           max_limit_col = "Cortex_top") {

  # Check if specified columns exist in the dataframe
  if (!(distance_col %in% colnames(df) && min_limit_col %in% colnames(df) && max_limit_col %in% colnames(df))) {
    stop("Specified columns do not exist in the dataframe.")
  }

  # Clip values within the specified range
  pmax(df[[min_limit_col]],
       pmin(df[[distance_col]],
            df[[max_limit_col]]))
}



