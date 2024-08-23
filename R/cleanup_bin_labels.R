#` Cleanup Bin Labels
#'
#' This function cleans up labels from binning using `cut`. It uses `gsub` to do this.
#'
#' @param labels A set of labels produced by cut
#'
#' @return The same labels but formatted as "lower - upper" without additional brackets.
#'
#' @examples
#' # Example usage
#' labels <- cleanup_bin_labels(labels)
#'
#' # If replacing levels of a factor, as in `bin_by_distance`
#'
#' levels(factor) <- cleanup_bin_labels(levels(factor))
#'
#' @export


cleanup_bin_labels <- function(labels){

  gsub("[\\[\\(](\\d+),(\\d+)[\\]\\)]",
       "\\1-\\2",
       x = labels,
       perl = TRUE)

}
