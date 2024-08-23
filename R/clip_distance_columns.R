#' Clip Distance Columns to Limits
#'
#' This function clips distance columns within a specified range defined by min_col and max_col.
#' It is a wrapper function that applies the \code{\link{clip_to_limits}} function to each distance column.
#'
#' @param df A dataframe.
#' @param distance_columns Character vector of column names containing distance data.
#' @param min_col A string that matches a column name containing the lower bounds to clip to (default: "VZ_base").
#' @param max_col A string that matches a column name containing the upper bounds to clip to (default: "Cortex_top").
#'
#' @return A dataframe with clipped distance columns.
#'
#' @seealso
#' \code{\link{clip_to_limits}}: The underlying function used for clipping individual distance columns.
#'
#' @importFrom magrittr %>%
#' @importFrom stat setNames
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#'
#' @export
#'
#' @examples
#' clip_distance_columns(df = counts_with_sections_data,
#'                        distance_columns = c("dist_col1", "dist_col2"),
#'                        min_col = "VZ_base",
#'                        max_col = "Cortex_top")
#'

clip_distance_columns <- function(df,
                                  distance_columns,
                                  min_col = "VZ_base",
                                  max_col = "Cortex_top") {



  purrr::map(.x = setNames(distance_columns,
                    distance_columns),
      .f = ~clip_to_limits(df = df,
                           distance_col = .x,
                           min_limit_col = min_col,
                           max_limit_col = max_col)) %>%
    dplyr::bind_cols(df)
}

