#' Get Channel Columns
#'
#' This function extracts channel columns based on the provided column name and dataframe.
#'
#' @param column_name The name of the column.
#' @param df The input dataframe.
#'
#' @return A modified dataframe with the extracted channel column.
#'
#' @rdname extract_filename_variables
#' @export

get_channel_columns <- function(column_name,
                                df) {
  df %>%
    dplyr::mutate("{column_name}" := stringr::str_extract(File,
                                          pattern = paste0(
                                            '^.*_([A-z]*\\d?)-',
                                            stringr::str_extract(column_name,
                                                        pattern = '^(\\d{3})-.*$',
                                                        group = 1),
                                            '_.*$'),
                                          group = 1),
           .keep = "none")
}
