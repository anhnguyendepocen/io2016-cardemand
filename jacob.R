#testing
sigma <- 0.005
x2 <- size
mval <- mvalold

jacob <- function(x2, sigma){
  expmu <- exp(mufunc(x2, sigma))
  shares <- ind_sh(mval, expmu)
  rm(expmu)
  
  f1 <- matrix(0, nrow = JT, ncol = 1)
  
  # computing (partial share)/(partial sigma)
  xv <- (x2 %*% rep(1,ns))*v
  temp1 <- cumsum(xv*shares)
  sum1 <- temp1[yrindex]
  sum1[2:length(sum1)] <- diff(sum1)
  f1 <- rowMeans(shares*(xv - sum1[yrid]))
  rm(xv, temp1, sum1)
  
  # computing (partial delta)/(partial theta2)

  f <- matrix(0, nrow = length(yrid), ncol = 1)
  n <- 1
  for(t in 1:length(yrindex)){
    temp1 <- shares[n:yrindex[t],]
    H1 <- temp1 %*% t(temp1)
    H <- (diag(rowSums(temp1)) - H1)/ns
    f[n:yrindex[t]] <- - solve(H) %*% f1[n:yrindex[t]]
    n <- yrindex[t] + 1
  }
  rm(n, H1, temp1, H)
  return(f)
}