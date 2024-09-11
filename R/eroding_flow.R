#' Flow Threshold for Channel Erosion
#'
#' For a given channel, calculates the largest flow a channel can accommodate
#' without eroding.
#' @inheritParams gbem
#' @returns Vector of discharges for each cross section.
#' @examples
#' library(sxchan)
#' sxc <- xt_sxc(c(12, 15, 18))
#' x <- sx_manning(sxc, grad = 0.02, d50 = 65, d84 = 100, roughness = 0.01)
#' eroding_flow(x, resistance = "manning")
#' @export
eroding_flow <- function(sx, resistance = c("ferguson", "manning")) {
  resistance <- rlang::arg_match(resistance)
  validate_sx(sx, resistance = resistance)
  if (resistance == "ferguson") {
    stop("FERGUSON NOT AVAILABLE YET")
  }
  if (resistance == "manning") {
    n <- sx$roughness
    d84 <- sx$d84
    W <- sxchan::xt_width(sx)
    S <- sx$grad
    H <- sx$rootdepth
    t_c84 <- t_c84(d84)
    d_crit <- find_d_crit(H, t_c84, S)
    return(d_crit^(5 / 3) * W * sqrt(S) / n)
  }
}
