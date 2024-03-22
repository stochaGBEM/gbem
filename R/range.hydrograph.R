#' Event hydrograph timing
#'
#' Returns the time frame for which the event hydrograph occurs.
#' @param ... Event hydrograph (only one allowed).
#' @param na.rm Not used.
#' @returns Vector of length 2 indicating the start and end times of the
#' event hydrograph
#' @export
range.hydrograph <- function(..., na.rm = FALSE) {
  ellipsis <- rlang::list2(...)
  if (length(ellipsis) != 1) {
    stop("`range()` function only accepts a single hydrograph.")
  }
  attr(ellipsis[[1]], "times")
}
