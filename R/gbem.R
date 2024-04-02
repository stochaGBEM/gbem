#' Gravel-Bed River Bank Erosion Model
#'
#' Runs the GBEM algorithm to determine erosion of a channel cross section
#' from a supplied event hydrograph.
#'
#' @param hydrograph Event hydrograph carried by the stream, with time units
#' in hours.
#' @param cross_section A `"cross_section"` object representing a stream's
#' cross section.
#' @param niter Number of iterations to run the algorithm over.
#' @details The hydrograph is first discretized into `niter` constant flows,
#' and the GBEM algorithm is iterated on those flows.
#' @returns A list of the following components:
#'
#' - `dw_pred`: predicted widening.
#' - `dw_const`: change in width constrained by transport capacity, the most
#    important thing here.
#' - `v_b`: transport capacity * time. Volume of transport that can be moved
#'   by the river.
#' - `cross_section`: The original cross section.
#' - `event`: A discretized event hydrograph with erosion widths and volumes
#' at each time step.
#' @examples
#' hg <- hyd_snow(5, 2)
#' cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01)
#' g <- gbem(hg, cs)
#' erode(g)
#' @export
gbem <- function(hydrograph, cross_section, niter = 1000){
  event <- discretize_hydrograph(hydrograph, niter)
  dt <- diff(event$time[1:2])
  erosion <- numeric()
  v_b <- numeric()
  cs <- list(cross_section)
  for (i in seq_len(niter)) {
    gbem_ <- gbem0(event$flow[i], dt, cs[[i]])
    erosion[i] <- gbem_$dw_const
    v_b[i] <- gbem_$v_b
    cs[[i + 1]] <- erode(gbem_)  #widen the channel
  }
  event$erosion <- erosion
  event$v_b <- v_b
  peak <- max(event$flow)
  dw_pred <- gbem0(peak, dt, cross_section)$dw_pred  #find
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
