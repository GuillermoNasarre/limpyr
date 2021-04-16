#' Find Spanish towns on a map
#'
#' Renders a map plotting all the towns of \code{x}. Their
#' official names and a list with further information can be
#' accessed by hovering and clicking on the markers, respectively.
#'
#' @param x A character or an integer vector with the names or the codes of the
#'   Spanish divisions to map.
#' @param division The type of territorial division in \code{x}: "municipio",
#'   "provincia" or "ccaa".
#' @return A HTML widget object, see \link[leaflet]{leaflet}.
#' @export
#' @import dplyr
#' @importFrom  leaflet leaflet addTiles addCircles
#' @examples
#' atlas("Navarra")
atlas <- function(x, division = "provincia") {
  stopifnot(
    is.character(x) || is.numeric(x),
    division %in% c("municipio", "provincia", "ccaa")
    )

  if (is.character(x)) {
    info <- codigator(x, division)
    x <- info$code
    if (any(is.na(x))) {
      warning("Some of the values in `x` couldn't be identified.",
              immediate. = TRUE)
    }
  } else {
    check_values(x, division)
    }

  if (division == "municipio") {
    div <- 3
  } else if (division == "provincia") {
    div <- 2
  } else {
    div <- 1
  }

  m <- municipios

  if (length(which(m[[div]] %in% x)) == 0) {
    stop("Unable to plot `x`.", call. = FALSE)
  }

  content <- m[m[[div]] %in% x, ] %>%
    distinct(division, .keep_all = TRUE) %>%
    mutate(pobl = gsub("; .*$", "", division))

  content %>%
    leaflet() %>%
    addTiles() %>%
    addCircles(lng = ~longitude,
               lat = ~latitude,
               label = ~pobl,
               popup = pop(content))
}
