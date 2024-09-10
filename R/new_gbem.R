#' Create a gbem object
#'
#' Internal function for making an object of class "gbem".
#' @param x Object to add gbem class to.
#' @param ... Other attributes to add to the object.
#' @param class If specifying a subclass, indicate it here.
#' @return Object with class "gbem" and attributes specified in `...`.
new_gbem <- function(x, ..., class = character()) {
  structure(x, ..., class = c(class, "gbem"))
}
