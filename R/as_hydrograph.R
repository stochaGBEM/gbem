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
#' h <- as_hydrograph(df, when, flows)
#' plot(h)
#' @rdname as_hydrograph
#' @export
as_hydrograph <- function(x, ...) {
  UseMethod("as_hydrograph")
}

#' @rdname as_hydrograph
#' @export
as_hydrograph.data.frame <- function(x, times_from, flows_from, unit = 1) {
  times <- rlang::enquo(times_from)
  flows <- rlang::enquo(flows_from)
  times <- rlang::eval_tidy(times, data = x) * unit
  flows <- rlang::eval_tidy(flows, data = x)
  f <- approxfun(times, flows)
  h <- new_hydrograph(f, times = range(times), peak = max(flows))
  validate_hydrograph(h)
}
