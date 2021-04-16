#' Trim extra white-spaces from strings
#'
#' Removes unnecessary white-spaces from the left, right and/or interior of
#' strings.
#' @param x A character vector.
#'
#' @return A character vector.
#' @export
#' @examples
#' x. <- c(" San  Sebastian ")
#' remove_extra_spaces(x.)
remove_extra_spaces <- function(x) {
  stopifnot(is.character(x))

  x <- gsub("\\s{2,}", " ", x)
  x <- gsub("^\\s+|\\s$", "", x)
  x
}
