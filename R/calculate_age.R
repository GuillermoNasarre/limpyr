#' Determine people's age
#'
#' Finds out the calendar year difference between two dates.
#'
#' @param givendate,birthdate POSIXt, Date or character vectors of equal length.
#'
#' @return An integer vector of the same length as \code{givendate}.
#' @details For \code{birthdate > givendate} \code{NA}s are returned .
#' @export
#' @import lubridate
#' @examples
#' calculate_age(Sys.Date(), "1988-02-29")
calculate_age <- function(givendate, birthdate) {
  stopifnot(all(is.Date(as.Date(c(givendate, birthdate)))))

  output <- as.period(interval(birthdate, givendate))$year
  output[output < 0] <- NA
  output
}
