#' Single iteration of the GBEM algorithm
#'
#' Run the gravel-bed river bank erosion model to determine channel
#' changes for a constant flow over a small duration.
#'
#' @param flow Discharge carried by the stream.
#' @param w Active width. This is the bank-to-bank width, excluding
#' vegetated islands.
#' @param duration Time for which flow acts on the stream channel (hrs).
#' @param xs2d A single two-dimensional cross section object (doesn't
#' contain information on stream properties like d50 or roughness).
#' @inheritParams sx_manning
#' @returns A list of the following components:
#'
#' - `dw_max`: maximum widening.
#' - `dw_const`: change in width constrained by transport capacity, the most
#    important thing here.
#' - `v_b`: transport capacity * time. Volume of transport that can be moved
#'   by the river.
#' @seealso [erode()]
gbem0_manning_with_volume <- function(flow, duration, xs2d, w, grad, d50, d84, roughness,
                          rootdepth, side) {
  checkmate::assert_numeric(flow, 0, len = 1)
  checkmate::assert_numeric(duration, 0, len = 1)
  checkmate::assert_numeric(grad, 0, len = 1)
  checkmate::assert_numeric(d50, 0, len = 1)
  checkmate::assert_numeric(d84, d50, len = 1)
  checkmate::assert_numeric(w, 0, len = 1)
  checkmate::assert_numeric(roughness, 0, len = 1)
  checkmate::assert_numeric(rootdepth, 0, len = 1)
  checkmate::assert_character(side, len = 1)
  if (side == "left") {
    prop_left <- 1
  } else if (side == "right") {
    prop_left <- 0
  } else {
    prop_left <- 0.5
  }
  # Step 0: get the cross section properties.
  n <- roughness
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
    dw_max <- 0
    dw_const <- 0
    q_b <- find_q_b(d, n, d50, S)
    v_b <- q_b * duration * hour_2_seconds
    xs2d_max <- xs2d
    xs2d_const <- xs2d
  } else{
    # Constrained erosion
    q_b <- mean(c(find_q_b(d, n, d50, S), find_q_b(d_crit, n, d50, S)))
    v_b <- q_b * duration * hour_2_seconds
    vol_1 <- v_b * d_crit / tan(travel_angle * pi / 180)
    new_cross_section <- sxchan::xt_widen(xs2d, volume = vol_1, side = side)
    new_width <- sxchan::xt_width(new_cross_section)
    dw_const <- new_width - w

    # Maximum erosion at this flow
    W_stable <- flow / (d_crit * v_crit)
    dw_max <- W_stable - w
    vol_2 <- sxchan::xt_erosion_volume(
      xs2d, dw_max,
      prop_left = prop_left, error_on_overflow = FALSE
    )

    #dw_const <- min(c(dw_max, v_b / tan(travel_angle * pi / 180)))

    #important note: the relevant volume of transport is transport in the bank
    #zone.  We can define the width of the bank zone as having a width that is
    #proportional to the bank height using the travel angle for small landslides
    #  so V_b = v_b * d_crit / tan(travel_angle).  To figure out
    #how much bank erosion could occur, we divide that volume by the bank
    #height so dw = v_b / tan(travel_angle)
  }
  list(
    dw_max = dw_max,
    dw_const = dw_const,
    v_b = v_b,
    # xs2d_max = xs2d_max,
    xs2d_const = new_cross_section
  )
}
