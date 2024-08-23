#' Extract HIER Conditions from a DataFrame
#'
#' This function adds three new columns based on HIER conditions recorded in formatted text from a specified column
#' The format of the HIER conditions should be as follows:
#'
#'    Molarity-Buffer_TempC_Timemins
#'
#' For example, using a 0.01 M Sodium Citrate buffer at 110C for 4 mins would equate to:
#'
#'    0.01-SC_110C_4min
#'
#' These data fields will then be used to generate separate variable columns.
#'
#' Options for buffers include:
#'
#'    SC = Sodium Citrate
#'    THCl = Tris-HCl
#'    TEDTA = Tris-EDTA
#'
#' At present, buffer pH is not included as a field
#'
#' @param df The input dataframe.
#' @param hier_column The name of the column containing HIER conditions (default: "AgR").
#'
#' @return A modified dataframe with added columns.
#'
#' @examples
#' sections <- get_hier_conditions(sections)
#'
#' @export
get_hier_conditions <- function(df, hier_column = "AgR") {
  # Validate input column existence
  if (!(hier_column %in% colnames(df))) {
    stop(paste("Column",
               hier_column,
               "does not exist in the dataframe."))
  }

  df %>%
    dplyr::mutate(
      AgR_Buffer = paste0(
        stringr::str_extract(hier_column, '^(\\d+\\.\\d+)-\\w+_.*$', group = 1),
        "M ",
        dplyr::case_when(
          stringr::str_detect(hier_column, 'SC') ~ 'Sodium Citrate',
          stringr::str_detect(hier_column, 'THCl') ~ 'Tris-HCl',
          stringr::str_detect(hier_column, 'TEDTA') ~ 'Tris-EDTA'
        )
      ),
      AgR_Temp = stringr::str_extract(hier_column, '^.*_(\\d+C)_.*$', group = 1),
      AgR_Time = stringr::str_extract(hier_column, '^.*_(\\d+min)$', group = 1)
    ) %>%
    dplyr::select(-{{ hier_column }})
}

