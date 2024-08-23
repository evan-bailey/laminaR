#' Extract Filename Variables
#'
#' This function extracts various variables from the 'File' column of a dataframe and adds them as new columns.
#'
#' @param df The input dataframe.
#' @param species A vector containing the species identifier strings, e.g. c("FTD", "Mm")
#' @param nuclear_stain A string containing the name of the nuclear stain ,e.g. DAPI
#'
#' @return A modified dataframe with added columns.
#'
#' @examples
#' df <- extract_filename_variables(df)
#'
#' @export
extract_filename_variables <- function(df,
                                       species = NULL,
                                       nuclear_stain = NULL,
                                       channels = NULL) {
      # Check if required columns exist
      required_columns <- c("File")

      species <- ifelse(c(!is.null(species[1]),
                          !is.null(species[2])),
                        species,
                        c("Mm", "FTD"))

      nuclear_stain <- ifelse(!is.null(nuclear_stain),
                              nuclear_stain,
                              "DAPI")

      channels <- ifelse(c(!is.null(channels[1]),
                           !is.null(channels[2]),
                           !is.null(channels[3])),
                         channels,
                         c("488",
                           "555",
                           "647"))

      if (!all(required_columns %in% colnames(df))) {
        stop("The required columns are missing in the input dataframe.")
      }

      ID_pattern <- "^\\d{6}_\\w*?_(.*?)_.*$"

      d_df <- df %>%
        dplyr::mutate(
          Date = lubridate::ymd(str_extract(File,
                                            "^[:digit:]*")),
          Animal_ID = sub(pattern = ID_pattern,
                          replacement = "\\1",
                          x = File),
          Mother_ID = stringr::str_extract(Animal_ID,
                                  pattern = "(.*)\\w$",
                                  group = 1),
          Young_ID = stringr::str_extract(Animal_ID,
                                 pattern = ".*(\\w)$",
                                 group = 1),
          Species = stringr::str_extract(File,
                                pattern = paste(species,
                                                collapse = "|")),
          Position = dplyr::case_when(
                        stringr::str_detect(File, "lateral") ~ "Lateral",
                        stringr::str_detect(File, "dorsal") ~ "Dorsal",
                        stringr::str_detect(File, "medial") ~ "Medial"
          ),
          Section_No = sub(pattern = "^.*_(\\d)_.*$",
                           replacement = "\\1",
                           x = File),
          Column = stringr::str_extract(File,
                               pattern = "(\\d+)(?!.*\\d)",
                               group = 1),
          Nuclear_Stain = nuclear_stain
        )

      secondary_channel_columns <- purrr::map(.x = secondary_columns(channels = channels),
                                       .f = get_channel_columns,
                                       df = d_df) %>%
        dplyr::bind_cols()

      dplyr::bind_cols(d_df,
                secondary_channel_columns)
}
