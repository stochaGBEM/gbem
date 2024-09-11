#' Calculates the critical threshold for channel widening
#'
#' FILL_THIS_IN: THRESHOLD OF WHAT? `find_d_crit0()` is the workhorse
#' and only accepts single numerics; `find_d_crit()` vectorizes its inputs.
#'
#' @param H Effective rooting depth for vegetation; single numeric for
#' `find_d_crit0()`.
#' @param t_c84 FILL_THIS_IN; single numeric for `find_d_crit0()`.
#' @param S Energy gradient of the stream channel; single numeric
#' for `find_d_crit0()`.
#' @rdname find_d_crit
#' @returns Critical threshold for channel widening.
find_d_crit0 <- function(H, t_c84, S) {
  if (length(H) * length(t_c84) * length(S) != 1) {
    stop(
      "`find_d_crit0()` expects single numerics; perhaps you're looking ",
      "to use `find_d_crit()`?"
    )
  }
  d_crit <- t_c84 / (g * rho * S)  # max depth that can be maintained (no veg)
  if (H > 0) {   #if mu > 0, adjust threshold and depth
    #bounds <- c(1, est_mu(H, d_crit)) * d_crit
    bounds <- c(1, 4) * d_crit
    d_test <- mean(bounds)
    d_target <- t_c84 * est_mu(H, d_test) / (g * rho * S)
    converg <- (d_test - d_target) / d_target
    while (abs(converg) > tol) {
      if (converg > 0) {
        bounds[2] <- d_test #do this if depth.test > depth.target
      } else {
        bounds[1] <- d_test #do this if depth.target > depth.test
      }
      d_test <- mean(bounds)
      d_target <- t_c84 * est_mu(H, d_test) / (g * rho * S)
      converg <- (d_test - d_target) / d_target
    }
    d_crit <- d_test
  }
  d_crit
}

#' @rdname find_d_crit
find_d_crit <- function(H, t_c84, S) {
  l <- vctrs::vec_recycle_common(H, t_c84, S)
  H <- l[[1]]
  t_c84 <- l[[2]]
  S <- l[[3]]
  res <- numeric()
  for (i in seq_along(H)) {
    res[i] <- find_d_crit0(H = H[i], t_c84 = t_c84[i], S = S[i])
  }
  res
}
