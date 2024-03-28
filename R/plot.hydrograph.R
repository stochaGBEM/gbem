#' Plot a hydrograph
#'
#' @param x Hydrograph object.
#' @param ylab,xlab Y-axis and x-axis labels for the plot.
#' @param ... Other arguments to pass to the `curve()` function.
#' @details Calls the `curve()` function on the hydrograph function,
#' plotted from the beginning to the end of the hydrograph event timeline.
#' @returns A base R plot of the hydrograph.
#' @export
plot.hydrograph <- function(x, ..., ylab = "Discharge", xlab = "Time") {
  r <- range(x)
  graphics::curve(x, from = r[1], to = r[2], xlab = xlab, ylab = ylab, ...)
}
