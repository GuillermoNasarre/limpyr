#' Get rid of vowel diacritics
#'
#' Removes vowel diacritics without changing the underlying encoding.
#'
#' @param x A character vector.
#'
#' @return A character vector.
#' @export
#' @examples
#' x. <- "{Múñez} Muñana; Ávila; Castilla y León"
#' remove_vowel_diacritics(x.)
remove_vowel_diacritics <- function(x) {
  stopifnot(is.character(x))

  pat <- list(A = c("[\u00c1\u00c0\u00c4\u00c2\u00c3]"),
              a = c("[\u00e1\u00e0\u00e4\u00e2\u00e3]"),
              E = c("[\u00c9\u00c8\u00cb\u00ca]"),
              e = c("[\u00e9\u00e8\u00eb\u00ea]"),
              I = c("[\U00cd\u00cc\u00cf\u00ce]"),
              i = c("[\u00ed\u00ec\u00ef\u00ee]"),
              O = c("[\u00d3\u00d2\u00d6\u00d4\u00d5]"),
              o = c("[\u00f3\u00f2\u00f6\u00f4\u00f5]"),
              U = c("[\u00da\u00d9\u00dc\u00db]"),
              u = c("[\u00fa\u00f9\u00fc\u00fb]"))

  for (vow in seq_along(pat)) {
    x <- gsub(pat[[vow]], names(pat[vow]), x)
  }
  x
}
