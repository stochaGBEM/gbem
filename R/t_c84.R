#' @inheritParams cross_section
t_c84 <- function(d84) {
  shields_c84 * g * (rho_s - rho) * (d84 / 1000)
}
