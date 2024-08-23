#' Convert Distance to Percentage
#'
#' This function converts individual distance values to a percentage scale based on individual minimum and maximum values.
#'
#' @param df A dataframe.
#' @param distance_col The column(s) containing individual distance values to be converted (default: 'Y').
#' @param max_col The column containing individual maximum value (default: 'Cortex_top').
#' @param min_col The column containing individual minimum value (default: 'VZ_base').
#' @param suffix The suffix to append to the new percentage column name (default: '_percent').
#' @param prefix The prefix to prepend to the new percentage column name (default: "").
#'
#' @return A dataframe with new columns representing the individual distance values converted to a percentage scale.
#'
#' @examples
#' # Example usage with multiple distance columns
#' convert_to_percentage(df = my_data, distance_col = c("dist_col1", "dist_col2"), max_col = "Cortex_top", min_col = "VZ_base", suffix = "_scaled", prefix = "scaled_")
#'
#' @export
#'
convert_to_percentage <- function(df,
                                  distance_col = 'Y',
                                  max_col = 'Cortex_top',
                                  min_col = 'VZ_base',
                                  suffix = "_percent",
                                  prefix = "") {

  # Ensure that specified columns exist in the dataframe
  if (!all(c(distance_col, max_col, min_col) %in% colnames(df))) {
    stop("Specified columns do not exist in the dataframe.")
  }

  # Handle a single distance column or multiple columns
  if (length(distance_col) == 1) {
    # Calculate percentage for a single column
    df[[paste0(prefix, distance_col, suffix)]] <- (df[[distance_col]] - df[[min_col]]) / (df[[max_col]] - df[[min_col]]) * 100
  } else {
    # Calculate percentage for multiple columns
    for (col in distance_col) {
      df[[paste0(prefix, col, suffix)]] <- (df[[col]] - df[[min_col]]) / (df[[max_col]] - df[[min_col]]) * 100
    }
  }

  df
}
