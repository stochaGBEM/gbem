generic_hydrograph <- function(Q, n, D84, D50, W, S, H = 0){
  #this function uses gbem to estimate the channel change during a flood hydrograph
  #with Q values for every hour of the floods

  event <- cbind(seq_along(Q), Q)

  #make a storage matrix
  extra <- matrix(data = NA,
                  nrow = nrow(event),
                  ncol = 4)
  event <- cbind(event, extra)

  #loop through all flows in the hydrograph
  wi <- W
  for( i in seq(1, nrow(event), 1) ){
    event[i, 3:6] <- gbem(event[i,2],1, n, D84, D50, wi, S, H)
    wi <- wi + event[i, 4]  #widen the channel
  }

  event <- as.data.frame(event[,c(1,2,4,5,6)])
  colnames(event) <- c('time', 'Q',  'erosion', 'vb', 'dc')

  return(event)

}
