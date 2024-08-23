#' Classify Cell Compartment
#'
#' This function categorizes cells into different compartments based on their distance from specified boundaries.
#'
#' @param df A dataframe.
#' @param distance_col The column containing distance values (default: 'Y').
#' @param boundary_cols A vector of column names containing boundary distances.
#' @param compartment_mapping A named vector mapping compartment names to their corresponding index in boundary_cols.
#'
#' @return A character vector representing the assigned compartment for each cell.
#'
#' @examples
#' # Example usage
#' counts_with_sections_data$Compartment <- classify_cell_compartment(df = counts_with_sections_data)
#'
#' @importFrom dplyr case_when
#'
#' @export
#'
classify_cell_compartment <- function(df,
                                      distance_col = "Y",
                                      boundary_cols = c("VZ_base", "Tbr2_base", "Tbr2_top", "Cortex_top"),
                                      compartment_mapping =  c("VZ", "SVZ", "CP")) {

  # Ensure that specified columns exist in the dataframe
  if (!all(c(distance_col, boundary_cols) %in% colnames(df))) {
    stop("Specified columns do not exist in the dataframe.")
  }

  # Check for NA values in the distance or boundary columns
  if (any(is.na(df[[distance_col]]) | sapply(boundary_cols, function(col) any(is.na(df[[col]]))))) {
    warning("NA values detected in distance or boundary columns. Assigning 'Unknown' compartment to NA values.")
  }

  # Use a dynamic approach for handling varying number of compartments
  conditions <- lapply(seq_along(compartment_mapping),
                       function(i) {
                         if (i == 1) {
                           expr <- df[[distance_col]] <= df[[boundary_cols[i + 1]]]
                         }
                         if (i > 1) {
                           expr <- df[[distance_col]] <= df[[boundary_cols[i + 1]]] & df[[distance_col]] > df[[boundary_cols[i]]]
                         }
                         return(expr ~ compartment_mapping[[i]])})


  # Apply conditions dynamically using case_when
  df$Compartment <- dplyr::case_when(!!!conditions,
                              TRUE ~ NA)

  df$Compartment
}

