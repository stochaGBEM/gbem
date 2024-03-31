#' Estimate mu value
#'
#' @param H Bank height ??? (meters???); numeric vector.
#' @param d Water depth ??? (meters???); numeric vector.
#' @returns Vector of `mu` values.
est_mu <- function(H, d){
  Hd <- vctrs::vec_recycle_common(H, d)
  H <- Hd[[1]]
  d <- Hd[[2]]
  p <- pmin(H / d, 0.9374073)
  a <- 0.85
  b <- 0.87
  1 / (1 - a * p)^b
}
