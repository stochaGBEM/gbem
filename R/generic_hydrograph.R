#' Uses gbem to estimate the channel change during a flood hydrograph
#' with Q values for every hour of the floods
#'
#' @param Q Discharge carried by the stream
#' @param n Manning's n value for the main channel
#' @param d84 84th percentile of the surface grain size distribution (mm)
#' @param d50 50th percentile of the grain size distribution (mm)
#' @param W Water surface width at the beginning of time interval T
#' @param S Energy gradient of the stream channel
#' @param H Effective rooting depth for vegetation
#'
#' @export
generic_hydrograph <- function(Q, n, d84, d50, W, S, H = 0){

  event <- cbind(seq_along(Q), Q)

  #make a storage matrix
  extra <- matrix(data = NA,
                  nrow = nrow(event),
                  ncol = 4)
  event <- cbind(event, extra)

  #loop through all flows in the hydrograph
  wi <- W
  for( i in seq(1, nrow(event), 1) ){
    event[i, 3:6] <- gbem(event[i,2],1, n, d84, d50, wi, S, H)
    wi <- wi + event[i, 4]  #widen the channel
  }

  event <- as.data.frame(event[,c(1,2,4,5,6)])
  colnames(event) <- c('time', 'Q',  'erosion', 'vb', 'dc')

  return(event)

}
