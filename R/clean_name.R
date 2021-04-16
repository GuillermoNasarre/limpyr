#' Homogenize personal names' syntax
#'
#' Removes impossible punctuation characters and sets the proper capitalization
#' and spacing of the names in \code{x}.
#'
#' @param x A character vector.
#'
#' @return A character vector of the same length as \code{x}.
#' @export
#' @importFrom stringi stri_trans_totitle stri_trim stri_opts_brkiter
#' @examples
#' x. <- c("Alberto De La Fue+nte Y Pastor", " JULIA O´DONNELL ?")
#' clean_name(x.)
clean_name <- function(x) {
  stopifnot(is.character(x))

  x <- wash_name(x)
  x <- dry_name(x)
  x
}
