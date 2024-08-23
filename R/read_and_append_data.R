#' Read and Append Data
#'
#' This function acts as a wrapper, allowing the user to specify an assay and importing data for 'counts', 'sections', and 'conditions'.
#'
#' @param assay Assay identifier (default: NULL)
#' @param data_dirs Vector of directory paths for custom data (default: NULL)
#' @param ep Experimental stage (default: NULL)
#' @param collect Collection stage (default: NULL)
#' @param exp_con Experimental construct (default: NULL)
#' @param ctl_con Control construct (default: NULL)
#' @param assay IF assay (default: NULL)
#' @param data_types A vector containing the type of data that is being imported. Options are 'counts', 'sections', 'conditions' or 'fluorescence',
#' but can be modified to match custom names of the .csv files containing the data (default: c("counts","sections","conditions"))
#'
#' @return A list of dataframes for the specified datasets ('counts','sections','conditions','fluorescence')
#'
#' @examples
#' # Example 1: Using default parameters
#' result <- read_and_append_data(assay = "PH3-Tbr2-tdT")
#'
#' # Example 2: Providing custom parameters
#' custom_result <- read_and_append_data(assay = "CustomAssay", data_dirs = c("exp_path1", "exp_path2"), ep = "Stage1", c = "CollectionA")
#'
#' @export
read_and_append_data <- function(assay = NULL,
                                 data_dirs = NULL,
                                 ep = NULL,
                                 collect = NULL,
                                 exp_con = NULL,
                                 ctl_con = NULL,
                                 data_types = c("counts",
                                                "sections",
                                                "conditions")) {
  assay <- ifelse(!is.null(assay),
                  assay,
                  IF_assay)

  if (!is.null(data_dirs)) {
    data_dirs <- data_dirs
  } else {
    data_dirs <- construct_data_dirs(ep = ep,
                                     collect = collect,
                                     exp_con = exp_con,
                                     ctl_con = ctl_con,
                                     assay = assay)
  }

  result_list <- lapply(data_types,
                        FUN = construct_dataframes,
                        data_dirs = data_dirs)
  names(result_list) <- data_types

  return(result_list)
}
