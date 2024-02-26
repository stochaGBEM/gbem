#' Main GBEM Workhorse
#'
#' Core function of gravel-bed river bank erosion model,
#' this is intended to become the work horse function for STOCHASIM and SGBEM
#'
#' @param Q Discharge carried by the stream
#' @param t Time for which Q acts on the stream channel (hrs)
#' @param n Manning's n value for the main channel
#' @param d84 84th percentile of the surface grain size distribution (mm)
#' @param d50 50th percentile of the grain size distribution (mm)
#' @param W Water surface width at the beginning of time interval T (m)
#' @param S Energy gradient of the stream channel
#' @param H Effective rooting depth for vegetation -
#' grassy banks, no trees / shrubs H = 0.35
#' 1 to 5% tree / shrub cover H = 0.50
#' 5 to 50% tree / shrub cover H =  0.90
#' more than 50% tree / shrub cover H = 1.1 description
#'
#' @returns description
#' @export
gbem <- function(Q, t, n, d84, d50, W, S, H = 0) {

    #step 0: define constants and sub functions
    g <- 9.81
    rho_s <- 2650
    rho <- 1000
    shields_c84 <- 0.02
    sheilds_c50 <- 0.045
    hour_2_seconds <- 60 * 60
    travel_angle <- 30 #the fahrboschung angle for small failures in sand/gravel (ranges from 30 to 35 most likely)
    tol = 0.001


    #step 1: calculate the critical threshold for channel widening
    t_c84 <- shields_c84 * g * (rho_s - rho) * (d84 / 1000)
    d_crit <- find_d_crit(Q, H, t_c84, S)
    v_crit <- d_crit^(2/3) * S^(1/2) / n

    #step 2: determine if channel will widen and calculate transp, widening
    d <- ( (n * Q) / (W * S^(1/2)) )^(3/5)
    stable <- d < d_crit
    if(stable) {
      dw_pred <- 0
      dw_const <- 0
      q_b <- find_q_b(d, n, d50, S)
      v_b <- q_b * t * hour_2_seconds
    } else{
      W_stable <- Q / (d_crit * v_crit)
      dw_pred <- W_stable - W
      q_b <- mean(c(find_q_b(d, n, d50, S), find_q_b(d_crit, n, d50, S)))
      v_b <- q_b * t * hour_2_seconds
      dw_const <- min(c(dw_pred, v_b/tan(travel_angle * pi / 180) ))
      #important note: the relevant volume of transport is transport in the bank
      #zone.  We can define the width of the bank zone as having a width that is
      #proportional to the bank height using the travel angel for small landslides
      #  so V_b = v_b * d_crit / tan(travel_angle).  To figure out
      #how much bank erosion could occur, we divide that volume by the bank
      #height so dw = v_b / tan(travel_angle)
    }

    # step 3: return the predicted widening, widening constrained by vb,
    # transp. volume, stable depth
    return(c(dw_pred, dw_const, v_b, d_crit))
}






