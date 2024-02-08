#the functions used to set up stochasim
make_floods <- function(gev, n_floods){
  #specify a GEV
  floods <- extRemes::revd(
    n = n_floods,
    loc = gev[1],
    scale = gev[2],
    shape = gev[3],
    type = "GEV"
  )
  # get rid of any negative numbers
  floods[floods < 0] <- mean(floods)
  return(floods)
}
