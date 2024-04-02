#' Erode a channel from GBEM
#'
#' While `gbem()` calculates fluvial geomorphic change, `erode()`
#' executes the change to create a modified channel or cross section.
#' @param gbem The output of `gbem()`, containing the channel change.
#' @returns The original cross section supplied to gbem, with possibly
#' modified width based on the gbem run.
#' @export
erode <- function(gbem) {
  cs <- gbem$cross_section
  dw <- gbem$dw_const
  ch_width(cs) <- ch_width(cs) + dw
  cs
}
