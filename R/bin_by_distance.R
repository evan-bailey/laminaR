#' Bin Data by Distance
#'
#' Bins distance data based on specified parameters and appends a cleaned-up binned distance column to the dataframe.
#'
#' This function bins distance values from the specified column into discrete intervals (bins) defined by the `bin_width` parameter,
#' grouped by specified columns (`groups`). The resulting binned distances are added as a new column with a suffix (`suffix`) to
#' the input dataframe (`data`).
#'
#' @param data A dataframe containing the data to be binned.
#' @param distance_col The name of the column containing distance values (default: 'Y_um').
#' @param bin_width The width of each bin (in the same unit as `distance_col`).
#' @param groups A character vector specifying grouping columns for binning (default: c("Species", "Animal_ID", "Position", "Section_No", "Column")).
#' @param min_col The name of the column containing the minimum value for binning (default: 'VZ_base_um').
#' @param max_col The name of the column containing the maximum value for binning (default: 'Cortex_top_um').
#' @param suffix The suffix to append to the new binned distance column name (default: "_bin").
#'
#' @return A dataframe with an additional column representing binned distance values.
#'
#' @examples
#' \dontrun{
#' # Bin data by distance with default parameters
#' bin_by_distance(data, bin_width = 10)
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang ensym
#' @importFrom rlang sym
#'
#' @export
#'
#'

bin_by_distance <- function(data,
                            distance_col = 'Y_um',
                            bin_width = 10,
                            groups = c("Species", "Animal_ID", "Position", "Section_No", "Column"),
                            min_col = 'VZ_base_um',
                            max_col = 'Cortex_top_um',
                            suffix = "_bin") {

  # Check for NA values in specified columns
  if (any(is.na(data[[distance_col]]) | is.na(data[[min_col]]) | is.na(data[[max_col]]) | sapply(groups, function(col) any(is.na(data[[col]]))))) {
    stop("NA values detected in input columns. Please handle or remove NA values before using the function.")
  }

  # Ensure that specified columns exist in the dataframe
  required_cols <- c(distance_col, min_col, max_col, groups)
  if (!all(required_cols %in% colnames(data))) {
    stop("Specified columns do not exist in the dataframe.")
  }

  # Use tidy evaluation to reference columns
  data <- data %>%
                dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
                dplyr::mutate(!!paste0(rlang::ensym(distance_col),
                                suffix) := cut(
                                        !!rlang::sym(distance_col),
                                        breaks = seq(
                                          from = min(!!rlang::sym(min_col),
                                                     na.rm = TRUE),
                                          to = max(!!rlang::sym(max_col),
                                                   na.rm = TRUE),
                                          by = bin_width
                                        ),
                                        include.lowest = TRUE,
                                        dig.lab = 10
                                        )) %>%
                                      dplyr::ungroup()

  # Clean up bin labels if cleanup_bin_labels is defined
  levels(data[[paste0(distance_col, suffix)]]) <- cleanup_bin_labels(levels(data[[paste0(distance_col, suffix)]]))

  return(data)
}


# bin_by_distance <- function(df,
#                             distance_col = 'Y_um',
#                             bin_width = 10,
#                             groups = c("Species", "Animal_ID", "Position", "Section_No", "Column"),
#                             min_col = 'VZ_base_um',
#                             max_col = 'Cortex_top_um',
#                             suffix = "_bin"){
#
#
#   # Check for NA values in specified columns
#   if (any(is.na(data[[distance_col]]) | is.na(data[[min_col]]) | is.na(data[[max_col]]) | sapply(groups, function(col) any(is.na(data[[col]]))))) {
#     stop("NA values detected in input columns. Please handle or remove NA values before using the function.")
#   }
#
#   # Ensure that specified columns exist in the dataframe
#   if (!all(c(distance_col, min_col, max_col, groups) %in% colnames(data))) {
#     stop("Specified columns do not exist in the dataframe.")
#   }
#
#   tmp <- group_by(df,
#                   across(all_of(groups))) %>%
#     mutate('{distance_col}{suffix}' := cut(
#       eval(parse(text = distance_col)),
#       breaks = seq(
#         from = min(eval(parse(text = min_col))),
#         to = max(eval(parse(text = max_col))),
#         by = bin_width
#       ),
#       include.lowest = TRUE)
#     ) %>%
#     ungroup()
#
#   levels(tmp[[paste0(distance_col, suffix)]]) <- cleanup_bin_labels(levels(tmp[[paste0(distance_col, suffix)]]))
#
#   tmp
#
# }
#
