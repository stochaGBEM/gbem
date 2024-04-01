#' Hydrograph peak
#'
#' Returns the peak of an event hydrograph.
#' @param hydrograph Object of class "hydrograph".
#' @returns The peak discharge of the event hydrograph; single numeric.
#' @export
peak <- function(hydrograph) {
  if (!is_hydrograph(hydrograph)) {
    stop("`peak()` is expecting a hydrograph; found class '",
         class(hydrograph)[1], "'.")
  }
  if ("peak" %in% names(attributes(hydrograph))) {
    return(attr(hydrograph, "peak"))
  }
  negh <- function(x) -hydrograph(x)
  r <- range(hydrograph)
  o <- optim(mean(r), negh, method = "Brent", lower = r[1], upper = r[2])
  -o$value
}
