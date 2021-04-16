#' Set bias parameters
#'
#' Bias parameters define the action scope of \link{provinciator} by favoring
#' its string matching process towards or against selected regional divisions.
#'
#' @param division Biased division type: "municipio, "provincia" or "ccaa".
#' @param towards Codes of the divisions to bias towards: integer vector.
#' @param against Codes of the divisions to bias against: integer vector.
#'
#' @details Use \link{codigator} to retrieve the requested codes or execute
#'   \link{codex} to print the full list.
#'
#' @return A three-element list with class \code{bias}.
#'
#' @export
bias <- function(division = "provincia", towards = NULL, against = NULL) {
  if (!division %in% c("municipio", "provincia", "ccaa") ||
      length(division) > 1) {
    stop("Invalid `division` type.")
  }

  if (any(towards %in% against)) {
    stop("Values cannot be repeated in both 'towards' and 'against' arguments.")
  }

  if (division == "municipio" &&
      !all(c(towards, against) %in% unique(municipios$c.muni))) {
    warning(paste("Some 'municipio' codes are not valid.",
                  "Type ?codigator for more information."))
  }

  if (division == "provincia" && !all(c(towards, against) %in% c(1:52))) {
    warning(paste("Some 'provincia' codes are not valid.",
                  "Type ?codigator for more information."))
  }

  if (division == "ccaa" && !all(c(towards, against) %in% c(1:19))) {
    warning(paste("Some 'comunidad aut\u00f3noma' codes are not valid.",
                  "Type ?codigator for more information."))
  }

  output <- structure(list("division" = division,
                           "towards" = towards,
                           "against" = against),
                      class = "bias")
  output
}
