#' Possibly convert a data.frame to a tibble
#'
#' If a user has the tibble package installed, convert a data frame
#' to a tibble.
#' @param res Data frame to possibly convert to tibble.
#' @returns Returns `res` if the tibble package is not installed,
#' and `as_tibble(res)` if the tibble package is installed.
convert_dataframe_to_tibble <- function(res) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    res <- tibble::as_tibble(res)
  }
  res
}
