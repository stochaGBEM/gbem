#' Uses gbem to estimate the channel change during a flood hydrograph
#' with Q values for every hour of the floods
#'
#' @param hydrograph Event hydrograph carried by the stream, with time units
#' in hours.
#' @param cross_section Cross section of the stream.
#' @param niter Number of iterations to run the algorithm over.
#' @returns A discretized event hydrograph with erosion widths and volumes
#' at each time step.
#' @examples
#' hg <- hydrograph_snow(5, 2)
#' cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01)
#' g <- gbem2(hg, cs)
#' erode(g)
#' @export
gbem2 <- function(hydrograph, cross_section, niter = 100){
  event <- discretize_hydrograph(hydrograph, niter)
  dt <- diff(event$time[1:2])
  erosion <- numeric()
  v_b <- numeric()
  cs <- list(cross_section)
  for (i in seq_len(niter)) {
    gbem_ <- gbem(event$flow[i], dt, cs[[i]])
    erosion[i] <- gbem_$dw_const
    v_b[i] <- gbem_$v_b
    cs[[i + 1]] <- erode(gbem_)  #widen the channel
  }
  event$erosion <- erosion
  event$v_b <- v_b
  peak <- max(event$flow)
  dw_pred <- gbem(peak, dt, cross_section)$dw_pred  #find
  dw_const <- sum(erosion)
  v_b <- sum(v_b)
  list(
    dw_pred = dw_pred,
    dw_const = dw_const,
    v_b = v_b,
    cross_section = cross_section,
    event = event
  )
}
