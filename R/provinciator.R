#'@title Locate and homogenize Spanish territorial divisions
#'@description Uses approximate string matching to infer the entity names in
#'  \code{x} and returns their official designation and geographical
#'  coordinates.
#'@usage provinciator(x, division = "provincia", scope = bias())
#'@param x A character vector.
#'@param division The type of territorial division in \code{x}:
#'  \code{"municipio"} (includes "unidades poblacionales"), \code{"provincia"}
#'  or \code{"ccaa"}.
#'@param scope A list with class \link{bias} used to favor the string matching
#'  process of \code{x} towards or against selected regional divisions.
#'@param .data A data frame.
#'@param .var A variable from \code{.data}.
#'@param ... Other arguments passed to \code{provinciator} (except \code{x}).
#'@details Geographical coordinates for \code{"provincia"} and \code{"ccaa"}
#'  refer to the capital of the chosen division type.
#'
#'  If \code{scope} is used, follow this example as a guide:
#'
#'  \code{scope = bias("provincia", c(1, 20, 48:49), 4)}
#'
#'  Basque provinces (1, 20 & 48) plus Zamora (49) will be prioritized when
#'  matching names in \code{x} and Almería (4) won't be used at all.
#'@return For \code{provinciator}, a tibble (i.e. \link[tibble]{tbl_df}) with
#'  the \href{https://www.ine.es/daco/daco42/codmun/codmunmapa.htm}{INE}'s
#'  official division names in \code{x} plus their geographical coordinates.
#'
#'  For \code{provinciate}, an object of the same class as \code{.data} with the
#'  results attached as new columns.
#'@note By default, tibbles print no more than 3 decimals. Use
#'  \code{options(pillar.sigfig = n)} to change this number.
#'@seealso Use \link{atlas} to learn about the town names used by
#'  \code{provinciator}.
#'@export
#'@importFrom dplyr bind_cols distinct left_join pull
#'@importFrom progress progress_bar
#'@importFrom stats setNames
#'@importFrom stringdist stringdist
#'@importFrom tibble tibble
#'@examples
#' x. <- c("Almería", "Teruel", "Lérida")
#' provinciator(x.)
#'
#'
#' library(tidyverse)
#'
#' edcn_towns <- edcn %>%
#'   provinciate(town_of_birth, "municipio") %>%
#'   select(town_of_birth, last_col(2:0), everything())
#'
provinciator <- function(x, division = "provincia", scope = bias()) {
  stopifnot(
    is.character(x),
    division %in% c("municipio", "provincia", "ccaa"),
    methods::is(scope, "bias")
    )

  params <- retrieve_params(division)

  col_names <- params[[1]]
  ref <- params[[2]]
  m <- params[[3]]
  mm <- NULL
  n <- params[[4]]
  dist <- params[[5]]

  switch(scope[[1]],
         "municipio" = {
           ind <- ref$c.muni
           },
         "provincia" = {
           ind <- ref$c.prov
           },
         "ccaa" = {
           ind <- ref$c.ccaa
           }
         )

  if (!is.null(scope[[2]])) {
    mm <- m[ind %in% scope[[2]]]
    nn <- n[ind %in% scope[[2]]]
    }

  if (!is.null(scope[[3]])) {
    m <- m[!ind %in% scope[[3]]]
    n <- n[!ind %in% scope[[3]]]
    }

  a <- rename_place(unique(x[!is.na(x)]))
  b <- strip_place(unique(x[!is.na(x)]))
  x <- rename_place(x)
  v <- as.character(rep(NA, length(a)))
  pb <- progress_bar$new(total = length(a))

  for (i in seq_along(a)) {
    pb$tick()

    if (length(mm) > 0) {
      b_dist <- stringdist(b[i], mm)
      b_min <- suppressWarnings(min(b_dist))

      if (b_min <= dist) {
        divs <- nn[which(b_dist == b_min)]
        c <- gsub(" \\(.*\\)", "", divs)

        if (nchar(b[i]) < nchar(a[i])) {
          a_dist <- stringdist(a[i], c, method = "jaccard")
          v[i] <- divs[which(a_dist == min(a_dist))][1]

        } else {
          v[i] <- first(c)

        }
      }
    }

    if (is.na(v[i])) {
      b_dist <- stringdist(b[i], m)
      b_min <- suppressWarnings(min(b_dist))

      if (b_min <= dist) {
        divs <- n[which(b_dist == b_min)]
        c <- gsub(" \\(.*\\)", "", divs)

        if (nchar(b[i]) < nchar(a[i])) {
          a_dist <- stringdist(a[i], c, method = "jaccard")
          v[i] <- divs[which(a_dist == min(a_dist))][1]

        } else {
          v[i] <- first(c)

        }
      }
    }
  }

  unique_matches <- setNames(unique(tibble(a, v)), col_names)
  all_matches <- left_join(tibble(x), unique_matches, by = c("x" = "a"))

  output <- left_join(all_matches[-1],
                      distinct(ref[5:7], division, .keep_all = TRUE),
                      by = structure("division", names = col_names[2]))
  output
  }

#' @rdname provinciator
#' @export
provinciate <- function(.data, .var, ...) {
  stopifnot(is.data.frame(.data))

  x <- pull(.data, {{ .var }})
  semi_output <- provinciator(x, ...)
  output <- bind_cols(.data, semi_output)
  output
  }
