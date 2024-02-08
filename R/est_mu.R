#' @param H height
#'
#' @param d depth
#'
#' @export
est_mu <- function(H, d){
  if(H/d > 0.94){
    mu <- 4  #set an upper threshold of 4
  } else {
    a = 0.85
    b = 0.87
    mu = 1 / (1 - a*(H/d))^b  #from Eaton and Millar 2017
  }
  return(mu)
}
