#' Flow Threshold for Channel Erosion
#'
#' For a given channel, calculates the largest flow a channel can accommodate
#' without eroding.
#' @inheritParams gbem
#' @returns Discharge; single numeric.
#' @examples
#' cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
#' eroding_flow(cs)
#' @export
eroding_flow <- function(cross_section) {
  n <- cross_section$roughness
  d84 <- cross_section$d84
  W <- ch_width(cross_section)
  S <- cross_section$grad
  H <- cross_section$rootdepth
  t_c84 <- t_c84(d84)
  d_crit <- find_d_crit(H, t_c84, S)
  d_crit^(5 / 3) * W * sqrt(S) / n
}
