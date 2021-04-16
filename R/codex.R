#' @title Print all main Spanish territorial division codes
#'
#' @description Prints the official codes of all the Spanish "provincias" and
#'   "comunidades autónomas".
#' @usage codex(order_by = "provincia", ascending = TRUE)
#'
#' @param order_by Arranges output either by "provincia" or "ccaa".
#' @param ascending Logical for whether to use ascending order or not.
#'
#' @return A tibble with all the "provincias" and "comunidades autónomas"
#'   along with their codes.
#'
#' @export
#' @examples
#' codex()
codex <- function(order_by = "provincia", ascending = TRUE) {
  stopifnot(
    order_by %in% c("provincia", "ccaa"),
    is.logical(ascending)
  )

  p <- unique(provincias[c(2, 1, 5)])
  lgth <- nrow(p)
  colnames(p)[3] <- "provincia"

  if (order_by == "provincia") {
    if (ascending) {
      print(p, n = lgth)
      } else {
        print(p[order(p$c.prov, decreasing = TRUE), ], n = lgth)
      }
     }
  else {
    if (ascending) {
      print(p[order(p$c.ccaa), ], n = lgth)
      } else {
        print(p[order(p$c.ccaa, decreasing = TRUE), ], n = lgth)
      }
    }
  }
