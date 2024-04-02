#' Minimum-Width Stable Channel
#'
#' Calculates the channel with the smallest width that remains stable
#' under a given flow.
#' @inheritParams gbem0
#' @returns The original cross section with width modified to be the minimum
#' width to accommodate the flow without eroding.
#' @examples
#' cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
#' min_stable_channel(cs, 5)
#' @export
min_stable_channel <- function(cross_section, flow) {
  if (!is.na(flow) && flow < 0) {
    stop("Flow must be positive.")
  }
  n <- cross_section$roughness
  d84 <- cross_section$d84
  W <- ch_width(cross_section)
  S <- cross_section$grad
  H <- cross_section$rootdepth
  t_c84 <- t_c84(d84)
  d_crit <- find_d_crit(H, t_c84, S)
  stable_width <- n * flow / d_crit^(5 / 3) / sqrt(S)
  ch_width(cross_section) <- stable_width
  cross_section
}
