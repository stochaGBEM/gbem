#' Get and Set Channel Width
#'
#' @inheritParams gbem
#' @returns Cross section width; single numeric.
#' @examples
#' cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01)
#' ch_width(cs)
#' ch_width(cs) <- 10
#' cs
#' @export
ch_width <- function(cross_section) {
  cross_section$width
}

#' @export
`ch_width<-` <- function(cross_section, value) {
  cross_section$width <- value
  cross_section
}
