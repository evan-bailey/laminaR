#' Construct Dataframes
#'
#' This function reads and appends data from multiple directories based on the specified data type.  The datatype matches the name of the .csv file.
#'
#' @param data_dirs Vector of directory paths
#' @param data_type Type of data: 'counts', 'sections', 'conditions' or 'fluorescence'. (default: c('counts','sections','conditions'))
#'
#' Each of the datatypes should be stored in a .csv file of the same name.  Custom names may be used for these files.
#'
#' @return A dataframe with appended data from specified directories
#'
#' @examples
#' # Example:
#' data_dirs <- c("exp_path", "control_path")
#' construct_dataframes(data_dirs, data_type = "counts")
#'
#' @export

construct_dataframes <- function(data_dirs,
                                 data_type = c("counts", "sections", "conditions")) {
  if (is.null(data_type) || length(data_type) > 1 || !all(data_type %in% c("counts", "sections", "conditions", "fluorescence"))) {
    stop("'data_type' argument must be one of: 'sections', 'conditions', 'counts', 'fluorescence' ")
  }

  # Read all CSV files in the directories and bind them row-wise
  files <- lapply(data_dirs,
                  function(d) list.files(path = d,
                                         pattern = paste0(data_type, ".csv"),
                                         full.names = TRUE))
  files <- unlist(files,
                  recursive = FALSE)

  if (length(files) == 0) {
    warning("No files found for data type: ",
            data_type)
    return(NULL)
  }

  d_df <- lapply(files,
                 function(file) readr::read_csv(file,
                                         col_names = TRUE))

  # Bind rows and remove duplicates
  result <- dplyr::bind_rows(d_df) %>%
                dplyr::distinct()

  return(result)
}


