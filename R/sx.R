#' Add features to cross sections
#'
#' Cross sections from the sxchan package are a collection of line segments.
#' This function stacks these line segments into a data frame column,
#' so that features of the cross sections can be specified in other columns.
#' Entries are recycled to the number of cross sections.
#'
#' For downstream modification of columns, use your favourite method for
#' modifying data frames.
#'
#' @param object An object of class "sxc" from the sxchan package.
#' @param ... Other features to include as columns in the data frame. Since
#' this is passed to `sf::st_sf()`, not all column names are permitted;
#' see details.
#' @param grad Channel gradient at the cross section.
#' @param d50,d84 Grain size distribution quantities (50th and 84th percentile).
#' @param roughness Roughness value.
#' @param rootdepth Effective rooting depth; defaults to 0.
#' @returns An object of class "sf", with the geometry column being an "sxc"
#' object.
#' @note The `sx()` function is a simple wrapper around `sf::st_sf()`,
#' aside from some basic checks to try to be helpful.
#' It exists to support the pairing of specialized features required
#' for different erosion engines, like `sx_manning()`. Otherwise,
#' `sx()` may not be needed, with `sf::st_sf()` sufficing.
#' @details Examples of effective rooting depth for vegetation, `rootdepth`,
#' are:
#'
#' - grassy banks, no trees / shrubs: `rootdepth = 0.35`.
#' - 1 to 5% tree / shrub cover: `rootdepth = 0.50`.
#' - 5 to 50% tree / shrub cover: `rootdepth = 0.90`.
#' - more than 50% tree / shrub cover: `rootdepth = 1.10`.
#' @examples
#' library(sxchan)
#' x <- xt_sxc(1:5)
#' sx_manning(
#'   x, grad = 0.1, d50 = 1, d84 = 2, swimmability = 4,
#'   roughness = c(0.01, 0.02, 0.01, 0.01, 0.1), rootdepth = 0
#' )
#' @rdname sx
#' @export
sx_manning <- function(
    object, ..., grad, d50, d84, roughness,
    rootdepth = 0
) {
  sx(
    object, ..., grad = grad, d50 = d50, d84 = d84, roughness = roughness,
    rootdepth = rootdepth
  )
}

#' @rdname sx
#' @export
sx_ferguson <- function(
    object, ..., grad, d50, roughness,
    rootdepth = 0
) {
  sx(
    object, ..., grad = grad, d50 = d50, roughness = roughness,
    rootdepth = rootdepth
  )
}

#' @rdname sx
#' @export
sx <- function(object, ...) {
  if (!sxchan::is_sxc(object)) {
    if (inherits(object, "sf")) {
      stop(
        "Input object is already a channel. Are you wanting to modify its ",
        "features? Remember, this object is a data frame; consider using ",
        "`dplyr::mutate()` to modify it."
      )
    }
    stop("Expecting object of class 'sxc', but received ", class(object), ".")
  }
  sf::st_sf(cross_section = object, ...)
}
