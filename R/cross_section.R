#' Create a new channel cross section
#'
#' Create a channel cross section by specifying its properties.
#'
#' @param width Channel width in meters; single positive numeric.
#' @param grad Energy gradient of the stream channel; single positive numeric.
#' @param d50,d84 Grain size distribution's 50th and 84th quantiles in
#' millimeters; single positive numerics.
#' @param roughness Manning's roughness; positive single numeric.
#' @param rootdepth Effective rooting depth for vegetation; single non-negative
#' numeric.
#' @note Width is assumed constant as a function of depth.
#' @details
#' Examples of effective rooting depth for vegetation, `rootdepth`, are:
#'
#' - grassy banks, no trees / shrubs: `rootdepth = 0.35`.
#' - 1 to 5% tree / shrub cover: `rootdepth = 0.50`.
#' - 5 to 50% tree / shrub cover: `rootdepth = 0.90`.
#' - more than 50% tree / shrub cover: `rootdepth = 1.10`.
#' @examples
#' cross_section(3, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
#' @returns A `"cross_section"` object.
#' @export
cross_section <- function(width, grad, d50, d84, roughness, rootdepth = 0) {
  l <- list(width = width,
            grad = grad,
            d50 = d50,
            d84 = d84,
            roughness = roughness,
            rootdepth = rootdepth)
  res <- new_cross_section(l)
  validate_cross_section(res)
}

#' Validator function for cross_section objects
#'
#' Checks that an object of class `"cross_section"` has a valid
#' structure and valid cross section properties.
#'
#' @param cross_section Object of class `"cross_section"`.
#' @returns Returns the original `cross_section` object. Note that this function is
#' intended to be run for its side effects: namely, throwing an error if the
#' cross_section is invalid.
validate_cross_section <- function(cross_section) {
  lengths <- vapply(cross_section, length, FUN.VALUE = integer(1L))
  if (any(lengths != 1)) {
    stop("Cross Section properties must be single numerics. The following are not:",
         paste(names(cross_section)[lengths != 1], collapse = ", "), ".")
  }
  if (cross_section$width <= 0) {
    stop("Cross Section must have a postive width.")
  }
  if (cross_section$grad <= 0) {
    stop("Cross Section must have a postive energy gradient (`grad`).")
  }
  if (cross_section$d84 < cross_section$d50) {
    stop("Invalid grain size distribution: cannot have d84 < d50.")
  }
  if (cross_section$d84 < cross_section$d50) {
    stop("Invalid grain size distribution: cannot have d84 < d50.")
  }
  if (cross_section$d50 <= 0) {
    stop("Invalid grain size distribution: grain size must be positive.")
  }
  if (cross_section$roughness <= 0) {
    stop("Manning's roughness must be positive.")
  }
  if (cross_section$rootdepth < 0) {
    stop("Effective rooting depth for vegetation (`rootdepth`) cannot be ",
         "negative.")
  }
  cross_section
}

#' Constructor function for cross_section objects
#'
#' @param l List containing the components of a cross_section object.
#' @param ... Attributes to add to the object.
#' @param class If making a subclass, specify its name here.
#' @returns An object of class `"cross_section"`, although not necessarily with
#' valid properties.
new_cross_section <- function(l, ..., class = character()) {
  structure(l, ..., class = c(class, "cross_section"))
}

#' @exportS3Method base::print
print.cross_section <- function(x, ...) {
  ellipsis::check_dots_empty()
  cat(paste0("Channel with width ", x$width))
}
