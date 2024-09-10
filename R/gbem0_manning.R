#' Single iteration of the GBEM algorithm
#'
#' Run the gravel-bed river bank erosion model to determine channel
#' changes for a constant flow over a small duration.
#'
#' @param flow Discharge carried by the stream.
#' @param duration Time for which flow acts on the stream channel (hrs).
#' @param width Cross section width
#' @inheritParams sx_manning
#' @returns A list of the following components:
#'
#' - `dw_pred`: predicted widening.
#' - `dw_const`: change in width constrained by transport capacity, the most
#    important thing here.
#' - `v_b`: transport capacity * time. Volume of transport that can be moved
#'   by the river.
#' @seealso [erode()]
gbem0_manning <- function(flow, duration, width, grad, d50, d84, roughness,
                          rootdepth) {
  stopifnot(length(flow) == 1)
  stopifnot(length(duration) == 1)
  # Step 0: get the cross section properties.
  n <- roughness
  d84 <- d84
  d50 <- d50
  w <- width
  S <- grad
  H <- rootdepth
  #step 1: calculate the critical threshold for channel widening
  t_c84 <- t_c84(d84)
  d_crit <- find_d_crit(H, t_c84, S)
  v_crit <- d_crit^(2 / 3) * sqrt(S) / n
  #step 2: determine if channel will widen and calculate transp, widening
  d <- ((n * flow) / (w * sqrt(S)))^(3 / 5)
  stable <- d < d_crit
  if (stable) {
    dw_pred <- 0
    dw_const <- 0
    q_b <- find_q_b(d, n, d50, S)
    v_b <- q_b * duration * hour_2_seconds
  } else{
    W_stable <- flow / (d_crit * v_crit)
    dw_pred <- W_stable - w
    q_b <- mean(c(find_q_b(d, n, d50, S), find_q_b(d_crit, n, d50, S)))
    v_b <- q_b * duration * hour_2_seconds
    dw_const <- min(c(dw_pred, v_b / tan(travel_angle * pi / 180)))
    #important note: the relevant volume of transport is transport in the bank
    #zone.  We can define the width of the bank zone as having a width that is
    #proportional to the bank height using the travel angle for small landslides
    #  so V_b = v_b * d_crit / tan(travel_angle).  To figure out
    #how much bank erosion could occur, we divide that volume by the bank
    #height so dw = v_b / tan(travel_angle)
  }
  list(
    dw_pred = dw_pred,
    dw_const = dw_const,
    v_b = v_b
  )
}
