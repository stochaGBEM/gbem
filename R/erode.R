#' Erode a channel from GBEM
#'
#' While `gbem()` calculates fluvial geomorphic change, `erode()`
#' executes the change to create a modified channel or cross section.
#' @param object The output of `gbem()`, containing the channel change.
#' @returns The original cross section supplied to gbem, with possibly
#' modified width based on the gbem run.
#' @export
erode <- function(object) UseMethod("erode")

#' @export
erode.gbem <- function(object) {
  sx <- object[["sx"]]
  dw <- object[["dw_const"]]
  sxc <- sf::st_geometry(sx)
  sxc_new <- sxchan::xt_widen_by(sxc, by = dw)
  sf::st_geometry(sx) <- sxc_new
  sx
}
