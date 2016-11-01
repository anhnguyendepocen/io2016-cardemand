
var_cov <- function(theta2, x1, x2, IV, v){
  N <- dim(x1)[1]
  Z <- dim(IV)[2]
  invA <- solve(t(IV) %*% IV)
  temp <- jacob(mvalold, theta2, x1, x2, IV, v)
  a <- t(cbind(x1, temp)) %*% IV
  rm(temp)
  
  IVres <- IV*(gmmresid %*% matrix(1, nrow = 1, ncol = Z))
  b <- t(IVres) %*% IVres
  
  c <- solve(a %*% invA %*% t(a)) %*% a %*% invA
  f <- c %*% b %*% t(c)
  return(f)
}
