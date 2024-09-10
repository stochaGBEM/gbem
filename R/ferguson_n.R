#' Function to use the Ferguson equation to calculate an equivalent n value
#'
#' @param d Depth
#' @param S Energy gradient of the stream channel
#' @param D84 84th percentile of the surface grain size distribution (mm)
#' @returns n value
ferguson_n <-  function(d, S, D84) {
  D84 <- D84 / 1000
  a1 <- 6.5
  a2 <- 2.5
  Res <- a1 * a2 * (d / D84) / sqrt(a1^2 + a2^2 * (d / D84)^(5 / 3))
  U <- Res * sqrt(g * d * S)
  d^(2 / 3) * sqrt(S) / U
}
