#' Validator function for sx objects
#'
#' Checks that an object is a valid `sx` object. When an erosion engine is
#' provided, additional checks are made to ensure the provided features are
#' sufficient for running the erosion engine. This function is run for its
#' side effects, throwing an error if invalid.
#'
#' @param sx Object of class `"sx"`.
#' @param resistance Optional specification of a resistance paradigm to
#' check against; see `gbem()` for options. Keep `NULL` to only check
#' the overall structure of the input `sx` object.
#' @returns Returns the original `sx` object, invisibly.
validate_sx <- function(sx, resistance = NULL) {
  if (!inherits(sx, "sf")) {
    stop(
      "Supplied `sx` object is not an 'sf' object, as required. ",
      "See `?sx` for creating these objects."
    )
  }
  sxc <- sf::st_geometry(sx)
  if (!sxchan::is_sxc(sxc)) {
    stop("Object does not contain cross section geometries 'sxc'.")
  }
  if (is.null(resistance)) {
    return(invisible(sx))
  }
  if (resistance == "ferguson") {
    return(validate_sx_ferguson(sx))
  }
  if (resistance == "manning") {
    return(validate_sx_manning(sx))
  }
  stop("Unexpected resistance paradigm: ", resistance)
}

validate_sx_ferguson <- function(sx) {
  check_grad(sx$grad)
  check_roughness(sx$roughness)
  check_rootdepth(sx$rootdepth)
  check_grainsize(d50 = sx$d50, d84 = Inf)
  invisible(sx)
}

validate_sx_manning <- function(sx) {
  check_grad(sx$grad)
  check_roughness(sx$roughness)
  check_rootdepth(sx$rootdepth)
  check_grainsize(d50 = sx$d50, d84 = sx$d84)
  invisible(sx)
}

check_grad <- function(grad = NULL) {
  if (is.null(grad)) {
    stop("Energy gradient is missing.")
  }
  if (any(grad <= 0)) {
    stop("Cross Section must have a postive energy gradient (`grad`).")
  }
  invisible()
}

check_grainsize <- function(d50 = NULL, d84 = NULL) {
  if (is.null(d84)) {
    stop("D84 grain size is missing.")
  }
  if (is.null(d50)) {
    stop("D50 grain size is missing.")
  }
  if (any(d50 <= 0)) {
    stop("Invalid grain size distribution: D50 must be positive.")
  }
  if (any(d84 <= 0)) {
    stop("Invalid grain size distribution: D84 must be positive.")
  }
  if (any(d84 < d50)) {
    stop("Invalid grain size distribution: cannot have D84 < D50.")
  }
  invisible()
}

check_roughness <- function(roughness = NULL) {
  if (!is.null(roughness) && any(roughness <= 0)) {
    stop("Roughness must be positive.")
  }
  invisible()
}

check_rootdepth <- function(rootdepth = NULL) {
  if (!is.null(rootdepth) && any(rootdepth < 0)) {
    stop("Effective rooting depth for vegetation cannot be negative.")
  }
  invisible()
}
