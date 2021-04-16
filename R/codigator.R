#' @title Find Spanish territorial division codes
#'
#' @description Provides the
#'   \href{https://www.ine.es/daco/daco42/codmun/codmunmapa.htm}{INE}'s official
#'   code of the Spanish territorial divisions in x. It uses approximate string
#'   matching.
#' @usage codigator(x, division = "provincia")
#'
#' @param x A character or numeric vector (see \emph{Value}).
#' @param division The type of territorial division to look for:
#'   \code{"municipio"}, \code{"provincia"} or \code{"ccaa"}.
#'
#' @return A tibble with \code{x} and the corresponding matches and codes. If
#'   \code{x} is numeric the output will be the name of the matched territorial
#'   divisions.
#'
#' @export
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#' @examples
#' x <- c("Pontevedra", "Madrid", "Teruel", "Valencia")
#' codigator(x)
#'
codigator <- function(x, division = "provincia") {
  stopifnot(
    is.character(x) || is.numeric(x),
    division %in% c("municipio", "provincia", "ccaa")
  )

  switch(division,
         "municipio" = {
           m <- unique(municipios[c(3, 5)])
           col_names <- c("input", "municipio", "code")
           },
         "provincia" = {
           m <- unique(provincias[c(2, 6)])
           col_names <- c("input", "provincia", "code")
           },
         "ccaa" = {
           m <- provincias[c(1, 6)]
           m[2] <- gsub("^.*; ", "", m[[2]])
           m <- unique(m)
           col_names <- c("input", "ccaa", "code")
           })

  if (is.numeric(x)) {
    table <- setNames(tibble(x), names(m[1])) %>%
      left_join(m, by = names(m[1]))
    output <- setNames(table, col_names[c(3, 2)])
    output

  } else {
    result <- provinciator(x, division)[1]
    table <- tibble(x, result) %>%
      left_join(m, by = structure("division", names = col_names[2]))
    output <- setNames(table, col_names)
    output

    }
  }
