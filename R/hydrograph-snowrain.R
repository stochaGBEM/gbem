#' Pre-set Event Hydrographs
#'
#' Constructs event hydrographs with characteristic shapes:
#'
#' - `hyd_rain()` is characteristic of a rainfall-related event.
#' - `hyd_snow()` is characteristic of a snowmelt-related event.
#' - `hyd_const()` is a flat hydrograph.
#'
#' @param peak,baseflow Peak and baseflow discharges; single numerics.
#' @param duration Length of the event. Defaults to 3 days for
#' rainfall-related events, and 9 days for snowmelt-related events
#' (in hours).
#' @details Rainfall-related event hydrographs are triangular, rising
#' from baseflow to peak in 1/3 the duration, followed by a recession
#' to baseflow.
#'
#' Snowmelt-related event hydrographs are like the rainfall hydrographs,
#' but remain at the peak discharge until half of the event duration.
#' @examples
#' hyd_rain(5, 2) |> plot()
#' hyd_snow(5, 2) |> plot()
#' hyd_const(5) |> plot()
#'
#' @rdname event_hydrographs
#' @export
hyd_snow <- function(peak, baseflow, duration = 3 * 24) {
  hydrograph(
    baseflow ~ 0,
    peak ~ 1 / 3,
    peak ~ 1 / 2,
    baseflow ~ 1,
    unit = duration
  )
}

#' @rdname event_hydrographs
#' @export
hyd_rain <- function(peak, baseflow, duration = 9 * 24) {
  hydrograph(
    baseflow ~ 0,
    peak ~ 1 / 3,
    baseflow ~ 1,
    unit = duration
  )
}


#' @rdname event_hydrographs
#' @export
hyd_const <- function(peak, duration = 1) {
  hydrograph(
    peak ~ 0,
    peak ~ duration
  )
}
