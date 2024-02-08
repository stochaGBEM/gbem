ferguson_n <-  function(d, S, D84){
  #a function to use the Ferguson equation to calculate an equivalent n value
  g <- 9.81
  D84 <- D84/1000
  a1 <- 6.5
  a2 <- 2.5
  Res <- a1*a2*(d/D84) / (a1^2 + a2^2*(d/D84)^(5/3))^(1/2)
  U <- Res * sqrt(g * d * S)
  n <- d^(2/3) * S^(1/2) / U
  return(n)
}
