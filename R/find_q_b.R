#' @export
find_q_b <- function(d, n, D50, S){
  D50 <- D50 / 1000
  Gs <- (rho_s - rho) / rho
  d_threshold <- sheilds_c50 * Gs * D50 / S
  velocity <- d^(2/3) * S^(1/2) / n
  om_star <- ( g * rho * d^(5/3) * S^(3/2) / n ) / (rho * (g * Gs * D50)^(3/2))
  om_crit <- sheilds_c50^(3/2) * d_threshold^(1/6)/(n * g^(1/2))
  E_star <- (0.92 - 0.25 * sqrt(om_crit / om_star))^9
  qb <- E_star * d * velocity * S / Gs
  return(qb)
}
