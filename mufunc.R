## Building the expmu function/matrix (non-linear of the utility, individual specific) dim(expmu) = 501 20
mufunc<-function(theta2, x1, x2, IV, v){
  mu <- theta2 * x2 %*% t(v)
  #x2 is of dim JT 1
  return(mu)
  #dim(mu) # JT ns
}

# mufunc <- function(theta2){
#   mu <- x2 %*% theta2 %*% rep(1, ns) %*% t(v)
#   return(mu)
# }