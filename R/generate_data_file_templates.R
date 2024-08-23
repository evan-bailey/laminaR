#' @title Generate CSV files
#' @description This function generates three .csv files in a user-specified directory. These files are in the format used for immunofluorescence data analysed using
#' this package.
#'
#' The first file, "counts.csv", has columns 'File', 'Marker', 'X', 'Y', and 'Mean_Gray'.
#'
#' The second file, "sections.csv", has columns 'File', 'VZ_base', 'Cortex_top', 'Microscope', 'Objective', and 'AgR'. This file can be customized with additional layer columns.
#'
#' The third file, "conditions.csv", has columns 'Mother_ID', 'Offspring', 'Condition', 'EP_Age', 'Collection_Age', 'EP_Stage', 'Collection_Stage', and 'Construct'.
#'
#'
#' @param directory The directory where the .csv files should be generated.
#' @param layer_cols A character vector of additional column names to include in the "sections.csv" file, situated between the 'VZ_base' and 'Cortex_top' columns. Default is an empty vector.
#' @return None
#'
#' @export

generate_data_file_templates <- function(directory, layer_cols = character()) {
                # Set the directory path
                dir_path <- file.path(directory)

                # Create a vector to store the data frames
                dfs <- list()

                # Define the data for each csv file
                counts_df <- data.frame(
                  File = numeric(0),
                  Marker = numeric(0),
                  X = numeric(0),
                  Y = numeric(0),
                  Mean_Gray = numeric(0)
                )



                sections_df <- data.frame(
                  File = numeric(0),
                  VZ_base = numeric(0),
                  cortex_layers = do.call(cbind, lapply(layer_cols, function(x) matrix(repeat(x), nrow = 10, ncol = 1))),
                  Cortex_top = numeric(0),
                  Microscope = numeric(0),
                  Objective = numeric(0),
                  AgR = numeric(0)
                )

              conditions_df <- data.frame(
                Mother_ID = numeric(0),
                Offspring = numeric(0),
                Condition = numeric(0),
                EP_Age = numeric(0),
                Collection_Age = numeric(0),
                EP_Stage = numeric(0),
                Collection_Stage = numeric(0),
                Construct = numeric(0)
              )

              # Add the data frames to the list
              dfs <- c(dfs, list(counts_df), list(sections_df), list(conditions_df))

              # Write each data frame to a csv file
              for (i in seq_along(dfs)) {
                filename <- switch(i,
                                   1, "counts.csv",
                                   2, "sections.csv",
                                   3, "conditions.csv"
                )
                write.csv(dfs[[i]], file.path(dir_path, filename), row.names = FALSE)
              }
}

