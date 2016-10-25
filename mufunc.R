## Building the expmu function/matrix (non-linear of the utility, individual specific) dim(expmu) = 501 20

mufunc<-function(x2,sigma){
  mu <- sigma*x2%*%t(v)
  #x2 is of dim JT 1
  return(mu)
  #dim(mu) # JT ns
}