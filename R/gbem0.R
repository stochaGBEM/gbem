#' Single iteration of the GBEM algorithm
#'
#' Run the gravel-bed river bank erosion model to determine channel
#' changes for a constant flow over a small duration.
#'
#' @param Q Discharge carried by the stream
#' @param t Time for which Q acts on the stream channel (hrs)
#' @param cross_section A `"cross_section"` object.
#' @returns A list of the following components:
#'
#' - `dw_pred`: predicted widening.
#' - `dw_const`: change in width constrained by transport capacity, the most
#    important thing here.
#' - `v_b`: transport capacity * time. Volume of transport that can be moved
#'   by the river.
#' - `cross_section`: The original cross section.
#' @seealso [erode()]
gbem0 <- function(Q, t, cross_section) {
  # Step 0: get the cross section properties.
  n <- cross_section$roughness
  d84 <- cross_section$d84
  d50 <- cross_section$d50
  W <- ch_width(cross_section)
  S <- cross_section$grad
  H <- cross_section$rootdepth
  #step 1: calculate the critical threshold for channel widening
  t_c84 <- shields_c84 * g * (rho_s - rho) * (d84 / 1000)
  d_crit <- find_d_crit(H, t_c84, S)
  v_crit <- d_crit^(2 / 3) * sqrt(S) / n
  #step 2: determine if channel will widen and calculate transp, widening
  d <- ((n * Q) / (W * sqrt(S)))^(3 / 5)
  stable <- d < d_crit
  if (stable) {
    dw_pred <- 0
    dw_const <- 0
    q_b <- find_q_b(d, n, d50, S)
    v_b <- q_b * t * hour_2_seconds
  } else{
    W_stable <- Q / (d_crit * v_crit)
    dw_pred <- W_stable - W
    q_b <- mean(c(find_q_b(d, n, d50, S), find_q_b(d_crit, n, d50, S)))
    v_b <- q_b * t * hour_2_seconds
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
    v_b = v_b,
    cross_section = cross_section
  )
}
