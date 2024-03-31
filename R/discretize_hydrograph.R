#' Discretize a Hydrograph
#'
#' Evaluates a hydrograph over a time grid.
#'
#' @param hydrograph A hydrograph object.
#' @param niter Number of time points in the grid.
#' @param times_to,flows_to Column names for the time and flow values.
#' @returns A data frame representation of a hydrograph.
#' @examples
#' hyd_snow(5, 2) |>
#'   discretize_hydrograph(100) |>
#'   plot()
#' @export
discretize_hydrograph <- function(hydrograph, niter, times_to = "time",
                                  flows_to = "flow") {
  dt <- range(hydrograph)
  t <- seq(dt[1], dt[2], length.out = niter)
  df <- data.frame(time = t, flow = hydrograph(t))
  names(df)[1] <- times_to
  names(df)[2] <- flows_to
  convert_dataframe_to_tibble(df)
}
