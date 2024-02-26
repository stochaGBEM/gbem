#' Calculates the critical threshold for channel widening
#'
#' @param Q Discharge carried by the stream, single numeric
#' @param H Effective rooting depth for vegetation, single numeric
#' @param t_c84
#' @param S Energy gradient of the stream channel, single numeric
#' @examples
#' gbem:::find_d_crit(100, 0.35, 0.5, 0.01)
#'
#' @returns Critical threshold for channel widening
find_d_crit <- function(Q, H, t_c84, S){

  #step 0: define constants and sub functions
  g <- 9.81
  rho <- 1000
  tol = 0.001

  d_crit <- t_c84 / (g * rho * S)  # max depth that can be maintained (no veg)

  if(H > 0){   #if mu > 0, adjust threshold and depth
    #bounds <- c(1, est_mu(H, d_crit)) * d_crit
    bounds <- c(1, 4) * d_crit
    d_test <- mean(bounds)
    d_target <- t_c84 * est_mu(H, d_test) / (g * rho * S)
    converg <- (d_test - d_target) / d_target
    while(abs(converg) > tol){
      if(converg > 0){
        bounds[2] <- d_test #do this if depth.test > depth.target
      }else{
        bounds[1] <- d_test #do this if depth.target > depth.test
      }
      d_test <- mean(bounds)
      d_target <- t_c84 * est_mu(H, d_test) / (g * rho * S)
      converg <- (d_test - d_target) / d_target
    }
    d_crit <- d_test
  }
  return(d_crit)
}
