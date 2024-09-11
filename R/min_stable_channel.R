#' Minimum-Width Stable Channel
#'
#' Calculates the channel with the smallest width that remains stable
#' under a given flow.
#' @param flow Discharge carried by the stream; single numeric.
#' @inheritParams gbem
#' @returns The original `sx` channel object, with cross section widths
#' modified to match the minimum width required to accommodate the flow
#' without eroding.
#' @examples
#' library(sxchan)
#' x <- sx_manning(
#'   xt_sxc(c(1, 1, 1)), grad = 0.01, d50 = 45, d84 = 80,
#'   roughness = c(0.01, 0.02, 0.03)
#' )
#' y <- min_stable_channel(x, flow = 5, resistance = "manning")
#'
#' xt_width(x)
#' xt_width(y)
#' @export
min_stable_channel <- function(sx, flow,
                               resistance = c("ferguson", "manning")) {
  resistance <- rlang::arg_match(resistance)
  validate_sx(sx, resistance = resistance)
  sxc <- sf::st_geometry(sx)
  if (length(flow) != 1) {
    stop("Expecting a single flow value.")
  }
  if (!is.na(flow) && any(flow < 0)) {
    stop("Flow must be positive.")
  }
  if (resistance == "ferguson") {
    stop("FERGUSON NOT AVAILABLE YET")
  }
  if (resistance == "manning") {
    n <- sx$roughness
    d84 <- sx$d84
    S <- sx$grad
    H <- sx$rootdepth
    t_c84 <- t_c84(d84)
    d_crit <- find_d_crit(H, t_c84, S)
    stable_width <- n * flow / d_crit^(5 / 3) / sqrt(S)
    W <- sxchan::xt_width(sxc)
    sxc <- sxchan::xt_widen_by(sxc, by = stable_width - W)
    sf::st_geometry(sx) <- sxc
    return(sx)
  }
}
