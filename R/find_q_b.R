#' Determine if channel will widen and calculate transp, widening
#'
#' @param d Flow depth (m).
#' @param n Manning's n value for the main channel.
#' @param d50 50th percentile of the grain size distribution (mm).
#' @param S Energy gradient of the stream channel.
#' @returns FILL_THIS_IN
find_q_b <- function(d, n, d50, S) {
  d50 <- d50 / 1000
  Gs <- (rho_s - rho) / rho
  d_threshold <- sheilds_c50 * Gs * d50 / S
  velocity <- d^(2 / 3) * sqrt(S) / n
  om_star <- (g * rho * d^(5 / 3) * S^(3 / 2) / n) /
    (rho * (g * Gs * d50)^(3 / 2))
  om_crit <- sheilds_c50^(3 / 2) * d_threshold^(1 / 6) / (n * sqrt(g))
  E_star <- (0.92 - 0.25 * sqrt(om_crit / om_star))^9
  E_star * d * velocity * S / Gs
}
