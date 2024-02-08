#' @export
find_d_crit <- function(Q, H, t_c84, S){

  d_crit <- t_c84 / (g * rho * S)  #max depth that can be maintained (no veg)

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
