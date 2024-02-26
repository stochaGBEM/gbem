#' Create a new Channel
#'
#' Create a channel by specifying its properties.
#'
#' @param width Channel width in meters; single positive numeric.
#' @param grad Energy gradient of the stream channel; single positive numeric.
#' @param d50,d84 Grain size distribution's 50th and 84th quantiles in
#' millimeters; single positive numerics.
#' @param roughness Manning's roughness; positive single numeric.
#' @param rootdepth Effective rooting depth for vegetation; single non-negative
#' numeric.
#' @note Width is assumed constant as a function of depth (might eventually
#' create a subclass for these simple channels).
#' @details
#' Examples of effective rooting depth for vegetation, `rootdepth`, are:
#'
#' - grassy banks, no trees / shrubs: `rootdepth = 0.35`.
#' - 1 to 5% tree / shrub cover: `rootdepth = 0.50`.
#' - 5 to 50% tree / shrub cover: `rootdepth = 0.90`.
#' - more than 50% tree / shrub cover: `rootdepth = 1.10`.
#' @returns A `"channel"` object.
#' @export
channel <- function(width, grad, d50, d84, roughness, rootdepth = 0) {
  l <- list(width = width,
            grad = grad,
            d50 = d50,
            d84 = d84,
            roughness = roughness,
            rootdepth = rootdepth)
  res <- new_channel(l)
  validate_channel(res)
}

#' Validator function for channel objects
#'
#' Checks that an object of class `"channel"` has a valid structure and valid
#' channel properties.
#'
#' @param channel Object of class `"channel"`.
#' @returns Returns the original `channel` object. Note that this function is
#' intended to be run for its side effects: namely, throwing an error if the
#' channel is invalid.
validate_channel <- function(channel) {
  lengths <- vapply(channel, length, FUN.VALUE = integer(1L))
  if (any(lengths != 1)) {
    stop("Channel properties must be single numerics. The following are not:",
         paste(names(channel)[lengths != 1], collapse = ", "), ".")
  }
  if (channel$width <= 0) {
    stop("Channel must have a postive width.")
  }
  if (channel$grad <= 0) {
    stop("Channel must have a postive energy gradient (`grad`).")
  }
  if (channel$d84 < channel$d50) {
    stop("Invalid grain size distribution: cannot have d84 < d50.")
  }
  if (channel$roughness <= 0) {
    stop("Manning's roughness must be positive.")
  }
  if (channel$rootdepth < 0) {
    stop("Effective rooting depth for vegetation (`rootdepth`) cannot be ",
         "negative.")
  }
  channel
}

#' Constructor function for channel objects
#'
#' @param l List containing the components of a channel object.
#' @param ... Attributes to add to the object.
#' @param class If making a subclass, specify its name here.
#' @returns An object of class `"channel"`, although not necessarily with
#' valid properties.
new_channel <- function(l, ..., class = character()) {
  structure(l, class = c(class, "channel"))
}
