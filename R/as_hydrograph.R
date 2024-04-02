#' Coerce an object to an event hydrograph
#'
#' Turn an object into an event hydrograph.
#'
#' @param x A data frame to coerce to a hydrograph.
#' @param ... Used for extensions to specific coercion methods.
#' @param times_from Column name containing timings of the hydrograph
#' change points, including the terminus times. Unquoted.
#' @param flows_from Column name containing the discharge values
#' corresponding to the timings in the `times_from` column. Unquoted.
#' @inheritParams hydrograph
#' @returns An event hydrograph, as in `hydrograph()`, made up of
#' linearly interpolated discharge-timing pairs.
#' @examples
#' df <- data.frame(when = 0:3, flows = c(2, 5, 4, 2))
#' h <- as_hydrograph(df, times_from = when, flows_from = flows)
#' plot(h)
#' @rdname as_hydrograph
#' @export
as_hydrograph <- function(x, ...) {
  UseMethod("as_hydrograph")
}

#' @rdname as_hydrograph
#' @export
as_hydrograph.data.frame <- function(x, ..., times_from, flows_from, unit = 1) {
  ellipsis::check_dots_empty()
  times <- rlang::enquo(times_from)
  flows <- rlang::enquo(flows_from)
  timecol <- tidyselect::eval_select(times, data = x)
  flowcol <- tidyselect::eval_select(flows, data = x)
  times <- x[[timecol]] * unit
  flows <- x[[flowcol]]
  f <- stats::approxfun(times, flows)
  h <- new_hydrograph(f, times = range(times), peak = max(flows))
  validate_hydrograph(h)
}

#' Test if an object is a hydrograph
#'
#' Returns `TRUE` if an object inherits the class `"hydrograph"`, and
#' `FALSE` otherwise.
#'
#' @param x An object.
#' @returns `TRUE` if an object inherits the class `"hydrograph"`.
#' @export
is_hydrograph <- function(x) {
  inherits(x, "hydrograph")
}
