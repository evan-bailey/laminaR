#' Secondary Channels
#'
#' This function generates channel names based on the provided secondary channels.
#'
#' @param channels The secondary channels.
#'
#' @return A character vector of channel names.
#'
#' @rdname extract_filename_variables
#' @export

secondary_columns <- function(channels = NULL){

  channels <- ifelse(c(!is.null(channels[1]),
                       !is.null(channels[2]),
                       !is.null(channels[3])),
                     channels,
                     c("488",
                       "555",
                       "647"))

  column_names <- paste0(channels,
                         "-channel")

  column_names

}
