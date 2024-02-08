
# a function to run stochasim using snow hydrographs
stochasim_snow <- function(floods_snow, Q_base, n, d84, d50, width_0, slope, H) {
  rv_rate <- 0.1
  #calculate some constants
  nsim <- length(floods_snow)
  width_base <- 3 * Q_base ^ 0.5  #estimate the low flow width based on base flow using std hydraulic geometry eq

  results <- matrix(data = NA, nrow = nsim, ncol = 4)

  #initialize the simulation by making calculations for the first year
  i = 1
  #analyse the effects of the first snowmelt flood
  r_snow <- snow_hydrograph(floods_snow[i], Q_base, n, d84, d50, width_0, slope, H)
  width_s <- width_0 + r_snow[2] #widen the channel

  #account for revegetation and net widening during hydrographs
  if(r_snow[2] > 0) {
    reveg <- 0  #no revegetation if bank erosion occurs
  } else {
    reveg <- rv_rate * (width_s - width_base)
  }
  width_v <- width_s - reveg

  #write the results Q_snow, Vb_snow, E_snow, new_width
  results[i,] <- c(floods_snow[i], r_snow[3], r_snow[2], width_v)

  # loop through all the floods generated above
  for (i in seq(2, nsim)) {
    #use the final width from the previous run to start the next one
    width_0 <- results[(i-1), 4]

    #analyse the effects of the next snowmelt flood
    r_snow <- snow_hydrograph(floods_snow[i], Q_base, n, d84, d50, width_0, slope, H)
    width_s <- width_0 + r_snow[2] #widen the channel

    #account for revegetation and net widening during hydrographs
    if(r_snow[2] > 0) {
      reveg <- 0  #no revegetation if bank erosion during fall/storm
    } else {
      reveg <- rv_rate * (width_s - width_base)
    }
    width_v <- width_s - reveg

    #write the results Q_snow, Vb_snow, E_snow, Q_rain, Vb_rain, E_rain, new_width
    results[i,] <- c(floods_snow[i], r_snow[3], r_snow[2], width_v)

  }

  #once the simulation is complete, transform to a data frame
  results <- as.data.frame(results)

  #give the columns meaningful names
  colnames(results) <- c('Q_snow',
                         'Vb_snow',
                         'E_snow',
                         'new_width')
  return(results)
}
