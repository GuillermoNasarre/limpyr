#' Infer people's gender
#'
#' Looks up for the gender of the personal names in \code{x}.
#'
#' @param x A character vector.
#' @details Unidentified genders are set as \code{NA} values.
#' @return A character vector of the same length as \code{x}.
#' @export
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#'
#' @examples
#' x. = c("Carlos", "Martha", "not_a_name")
#' assign_gender(x.)
#'
#'
#' library(tidyverse)
#'
#' edcn_genders <- edcn %>%
#'  separate(subject_name,
#'           into = c("family_names", "personal_name"),
#'           sep = ",") %>%
#'  mutate(gender = assign_gender(personal_name)) %>%
#'  select(1:2, gender, everything())
assign_gender <- function(x) {
  stopifnot(is.character(x))

  x <- strip_name(x)
  tbl_x <- tibble(name = x)
  result <- left_join(tbl_x, genders, by = "name")
  result$gender
}
