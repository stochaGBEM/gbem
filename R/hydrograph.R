#' Construct an event hydrograph
#'
#' Construct an event hydrograph by linearly connecting pairs of
#' discharge and timing.
#'
#' @param ... Pairs of discharges and timing separated by `~`.
#' @param unit Conversion factor for the time values; single numeric.
#' Useful if, for example, the timing is specified as a proportion of
#' the total event time.
#' @param env Environment for which to search for bindings, in the
#' case that discharge-timing pairs are specified symbolically.
#' @returns A data frame of two columns: `time` into the event, and
#' `discharge` at that time. Returns a tibble if the tibble package is
#' installed.

#' @examples
#' # Hydrograph over 10 units of time:
#' my_hydrograph <- hydrograph(7 ~ 0, 25 ~ 1 / 3, 7 ~ 1, unit = 10)
#' plot(my_hydrograph, n = 1000)
#'
#' # Hydrograph spanning 24 hours:
#' my_hydrograph <- hydrograph(7 ~ 0, 25 ~ 10, 20 ~ 15, 7 ~ 24)
#' plot(my_hydrograph, n = 1000)
#' @export
hydrograph <- function(..., unit = 1, env = rlang::caller_env()) {
  args <- rlang::list2(...)
  if (length(args) == 0) {
    warning("No discharge-timing pairs found. Returning NULL.")
    return(NULL)
  }
  lefts <- vapply(args, function(x) rlang::eval_tidy(x[[2]], env = env),
                  FUN.VALUE = numeric(1L))
  rights <- vapply(args, function(x) rlang::eval_tidy(x[[3]], env = env),
                   FUN.VALUE = numeric(1L))
  flows <- lefts
  timing <- rights * unit
  if (any(duplicated(timing))) {
    stop("Each time point must be unique.")
  }
  f <- stats::approxfun(timing, flows)
  h <- new_hydrograph(f, times = range(timing), peak = max(flows))
  validate_hydrograph(h)
}

#' Validator function for hydrograph objects
#' @param hydrograph A hydrograph object.
validate_hydrograph <- function(hydrograph) {
  if (!is.function(hydrograph)) {
    stop("Base type of a hydrograph object must be a function.")
  }
  t <- range(hydrograph)
  if (any(is.na(t)) || length(t) != 2) {
    stop("Timing of hydrograph must consist of a start time and end time.")
  }
  if (t[1] >= t[2]) {
    stop("End time of hydrograph must occur after start time.")
  }
  if (any(is.infinite(t))) {
    stop("Start and end times of hydrograph cannot be infinite.")
  }
  hydrograph
}

#' Constructor function for (event) hydrograph objects
#'
#' @param fun Function of time that represents the hydrograph
#' @param times Vector of length 2 indicating the start and end time of the
#' event hydrograph (numeric).
#' @param peak Magnitude of the peak discharge in the event hydrograph;
#' single numeric.
#' @param ... Attributes to add.
#' @param class If making a subclass, specify its name here.
#' @returns Object of class "hydrograph".
new_hydrograph <- function(fun, times, peak, ..., class = character()) {
  structure(fun, ..., times = times, peak = peak, class = c(class, "hydrograph"))
}

