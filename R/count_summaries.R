#' Cell Count Summaries or Densities
#'
#' Computes count summaries or cell densities from a dataframe of cells in long format, based on specified parameters.
#'
#' @param df A dataframe containing cell data in long format.
#' @param summary_type A character string specifying the type of summary ("counts" or "density").
#' @param width Name of the column containing cell width measurements for density calculation.
#' @param height Name of the column containing cell height measurements for density calculation.
#' @param groups Character vector specifying grouping variables for summarisation.
#' @param marker Name of the column containing marker information.
#' @param compartments Name of the column containing region information.
#'
#' @return A dataframe with count summaries or cell densities based on the specified summary_type.
#'
#' @details
#' The function computes either counts per marker per group or cell densities per marker within specified regions,
#' depending on the summary_type parameter.
#'
#' For "counts" summary_type, it calculates the number of cells (counts) per marker per group.
#' For "density" summary_type, it calculates cell densities (cells per unit area) per marker within specified regions,
#' using the provided width and height measurements.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' df <- data.frame(
#'   Species = c("Mouse", "Mouse", "Dunnart", "Dunnart"),
#'   Construct = c("A", "B", "A", "B"),
#'   Animal_ID = c(1, 2, 1, 2),
#'   Section_No = c(1, 2, 1, 2),
#'   Position = c("Lateral", "Dorsal", "Lateral", "Dorsal"),
#'   Column = c("A1", "B1", "A2", "B2"),
#'   Marker = c("tdT", "tdT", "Tbr2", "Tbr2"),
#'   Region = c("VZ", "VZ", "SVZ", "SVZ"),
#'   Width = c(100.15, 99.55, 100.56, 101.23),
#'   Height = c(300, 500, 200, 300)
#' )
#'
#' # Compute counts per marker per group:
#' count_summaries(df, summary_type = "counts")
#'
#' # Compute cell densities per marker within specified regions using width and height measurements:
#' count_summaries(df, summary_type = "density", width = "Width", height = "Height")
#' }
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#'
#' @export
#'
count_summaries <- function(df,
                            summary_type,
                            width = NULL,
                            height = NULL,
                            groups = c("Species",
                                       "Construct",
                                       "Animal_ID",
                                       "Section_No",
                                       "Position",
                                       "Column"),
                            marker = "Marker",
                            compartments = "Region") {

  # Load required packages if not already loaded
  if (!requireNamespace("dplyr", quietly = TRUE) || !requireNamespace("rlang", quietly = TRUE)) {
    stop("Required packages (dplyr, rlang) are not installed.")
  }

  if (summary_type == "counts") {
    # Calculate counts per marker per group
    result <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups,
                                                     marker)))) %>%
      dplyr::count() %>%
      tidyr::pivot_wider(names_from = {{ marker }},
                         values_from = "n",
                         values_fill = 0)
  } else if (summary_type == "density") {
    # Calculate cell densities per marker within specified regions
    if (is.null(width) || is.null(height) || is.na(width) || is.na(height)) {
      stop("Must specify the variables containing column width and height.")
    }

    result <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, marker, width, height)))) %>%
      dplyr::summarise(n = n(),
                       area = !!rlang::sym(width) * !!rlang::sym(height),
                       n_per_area = n / area,
                       area_scalar = 100 / area,
                       n_per_100um2 = n / (area * area_scalar),
                       n_per_1mm2 = n_per_100um2*10000) %>%
      dplyr::distinct()
  } else {
    stop("Invalid summary_type. Must be either 'counts' or 'density'.")
  }

  return(result)
}

# Example usage:
# df <- data.frame(
#   Species = c("Mouse", "Mouse", "Dunnart", "Dunnart"),
#   Construct = c("A", "B", "A", "B"),
#   Animal_ID = c(1, 2, 1, 2),
#   Section_No = c(1, 2, 1, 2),
#   Position = c("Lateral", "Dorsal", "Lateral", "Dorsal"),
#   Column = c("A1", "B1", "A2", "B2"),
#   Marker = c("tdT", "tdT", "Tbr2", "Tbr2"),
#   Region = c("VZ", "VZ", "SVZ", "SVZ"),
#   Width = c(100.15, 99.55, 100.56, 101.23),
#   Height = c(300, 500, 200, 300)
# )
# count_summaries(df, summary_type = "counts")
# count_summaries(df, summary_type = "density", width = "Width", height = "Height")

