#' GBEM, but on a channel.
#'
#' @inheritParams gbem
#' @returns Channel object, from sxchan.
#' @export
gbem_ch <- function(hydrograph, channel, niter = 1000) {
  xs <- sx_xs(channel)
  stopifnot(!is.null(xs))
  l <- lapply(xs, \(xs_) gbem(hydrograph, xs_, niter = niter))
  class(l) <- "gbem_ch"
  l
}
