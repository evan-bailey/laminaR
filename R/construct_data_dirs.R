#' Construct Data Directories
#'
#' This function constructs the paths for data directories based on the provided or default parameters.
#'
#' @param data_dir Vector of directory paths (default: NULL)
#' @param ep Experimental stage (default: NULL)
#' @param collect Collection stage (default: NULL)
#' @param exp_con Experimental construct (default: NULL)
#' @param ctl_con Control construct (default: NULL)
#' @param assay IF assay (default: NULL)
#'
#' @return A vector containing the paths for experimental and control directories
#'
#' @examples
#' # Example 1: Using default parameters
#' construct_data_dirs()
#'
#' # Example 2: Providing custom parameters
#' custom_dirs <- construct_data_dirs(data_dirs = c("exp_path", "control_path"), ep = "Stage1", c = "CollectionA")
#'
#' @export
construct_data_dirs <- function(data_dir = NULL,
                                ep = NULL,
                                collect = NULL,
                                exp_con = NULL,
                                ctl_con = NULL,
                                assay = NULL) {

  # Check which arguments are provided and use default global variables for missing ones
  data_dir <- ifelse(!is.null(data_dir),
                     data_dir,
                     file.path(here::here(),
                               '2_data'))

  ep <- ifelse(!is.null(ep),
               ep,
               ep_stage)

  collect <- ifelse(!is.null(collect),
                    collect,
                    collection_stage)

  exp_con <- ifelse(!is.null(exp_con),
                    exp_con,
                    experimental_construct)

  ctl_con <- ifelse(!is.null(ctl_con),
                    ctl_con,
                    control_construct)

  assay <- ifelse(!is.null(assay),
                  assay,
                  IF_assay)

  exp_dir <- file.path(data_dir,
                       exp_con,
                       paste0("S",
                              ep,
                              "-",
                              collect),
                       assay)

  ctl_dir <- file.path(data_dir,
                       ctl_con,
                       paste0("S",
                              ep,
                              "-",
                              collect),
                       assay)

  c(exp_dir,
    ctl_dir)
}

