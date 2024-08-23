#' Normalise Fluorescence by Background Value
#'
#' This function normalises fluorescence values by subtracting background fluorescence values from a specified column.
#' The background fluorescence value can be a measured value, calculated as the minimum fluorescence value within a sample group,
#' or calculated via min-max normalisation based upon minimum and maximum measures in the sample group
#'
#' @param df The input dataframe containing fluorescence values in tidy (long) format.
#' @param fluor_col The name of the column containing fluorescence values (default: "Gray_Value").
#' @param F0 The name of the column containing background fluorescence values, "min" to calculate background as the minimum fluorescence per sample group,
#' or min.max to calculate the min-max normalisation by the sample group (default: "BG").
#' @param marker The name of the column containing the channel information (a factor with levels for each marker) (default: "Marker").
#' @param groups Grouping variables to specify sample-level groupings for normalisation. Required only if `F0 = "min"` or if `F0 = "min.max"`
#' @param col.name Tha name of the resulting normalised fluorescence column (default: "deltaF.F0")
#'
#' @return A dataframe with an additional column for normalised fluorescence values.
#'
#' @examples
#' \dontrun{
#' # Specify columns containing measured data for background fluorescence.
#' normalise_fluorescence_by_background(df, fluor_col = "Gray_Value", F0 = "BG", marker = "Marker")
#'
#' # Calculate background fluorescence as the sample minimum with grouping columns specified.
#' normalise_fluorescence_by_background(df, fluor_col = "Gray_Value", F0 = "min", marker = "Marker", groups = c("Sample_ID"))
#'
#'#' # Calculate background fluorescence as the sample minimum with grouping columns specified.
#' normalise_fluorescence_by_background(df, fluor_col = "Gray_Value", F0 = "min.max", marker = "Marker", groups = c("Sample_ID"))
#'}
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#'
#' @export
normalise_fluorescence_by_background <- function(df,
                                                 fluor_col = "Gray_Value",
                                                 F0 = "BG",
                                                 marker = "Marker",
                                                 groups = NULL,
                                                 col.name = "deltaF.F0") {

  # Validate input parameters
  if (!is.data.frame(df)) {
    stop("Invalid dataframe provided.")
  }

  if (F0 == "min") {
    if (is.null(groups) || !is.character(groups) || length(groups) == 0 || any(!groups %in% names(df))) {
      stop("When F0 is set to 'min', valid grouping variables must be specified.")
    }

    # Calculate background fluorescence as the minimum per sample group
    df <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, marker)))) %>%
      dplyr::mutate(BGmin = min(!!rlang::sym(fluor_col),
                             na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate("{col.name}" := (!!rlang::sym(fluor_col) - BGmin) / BGmin)

  }

  if (F0 == "min.max"){

    if (is.null(groups) || !is.character(groups) || length(groups) == 0 || any(!groups %in% names(df))) {
      stop("When F0 is set to 'min.max', valid grouping variables must be specified.")
    }

    df <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, marker)))) %>%
      dplyr::mutate(BGmin = min(!!rlang::sym(fluor_col), na.rm = TRUE),
                    BGmax = max(!!rlang::sym(fluor_col), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate("{col.name}" := (!!rlang::sym(fluor_col) - BGmin) / (BGmax - BGmin))

  } else if (F0 != "min.max" & F0 != "min"){
    # Use specified background fluorescence column
    if (!is.character(F0) || is.na(F0) || !(F0 %in% names(df))) {
      stop("Invalid or missing background fluorescence column name.")
    }

    df <- df %>%
      dplyr::mutate("{col.name}" := (!!rlang::sym(fluor_col) - !!rlang::sym(F0)) / !!rlang::sym(F0))
  }

  return(df)
}
