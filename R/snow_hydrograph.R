snow_hydrograph <- function(Q, baseflow, n, D84, D50, W, S, H = 0, raw = FALSE){
  #this function uses gbem to estimate the channel change during a simulated flood
  #hydrograph due to a snowmelt flood

  #simulate a hydrograph with peak Q
  duration <- 9 * 24 # in hours
  time_peak <- (1/3) * duration
  time_recession <- (1/2) * duration
  flows <- c(baseflow, Q, Q, baseflow)
  times <- c(1, time_peak, time_recession, duration)
  event <- cbind(seq(1, duration),
                 approx(x = times, y = flows, xout = seq(1, duration)
                 )[[2]]
  )

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

  if(raw){
    #return the raw data from the analysis
    return(event)
  } else {
    #calculate the output variables
    dw_const <- sum(event[,4])
    v_b <- sum(event[,5])
    d_crit <- event[1,6]
    dw_pred <- gbem(Q, 1, n, D84, D50, W, S, H)[1]  #find
    return(c(dw_pred, dw_const, v_b, d_crit))
  }
}
