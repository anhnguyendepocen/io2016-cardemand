## ind_sh returns a {JT, ns} matrix with elements s_{jt, i} the individual probabilities of choosing product j, in market t (row) by individual i (column)
# expmval is a {JT, 1} column : this column of exp(delta_jt) will be repeated ns times in the expmval matrix within the function
# expmu is a {JT, ns} matrix : exp(mufunc(theta2)); its elements are exp(sum_k x_2,{jt}^k * theta_2^k * v_i)

ind_sh <- function(expmval, expmu){
  # computing the numerator of the individual shares s_{jt,i}
  expmval <- expmval %*% t(rep(1, ns))
  num <- expmu * expmval
  
  # computing the denominator of the individual shares s_{jt,i}
  denom <- matrix(0, nrow = length(yrindex), ncol = dim(num)[2])
  for (t in 1:length(yrindex)){ #compute denominator separately for each market, by summing over the products in the same market, in the numerator
    denom[t,] <- 1 + colSums(num[yrid == t,])
  }
  denom <- denom[yrid,]
  
  return(num/denom)
}

## TESTING
# sigma <- 2
# expmu<-exp(mufunc(sigma))
# expmval <- exp(delta.est) %*% t(rep(1, ns))

# ind_sh<-function (expmval,expmu){
#   
#   # Building the 6ynumerator of the indiv market share (eg matrix) dim(eg)= JT ns
#   
#   eg<-expmu*matrix(kronecker(rep(1,ns),expmval),ncol=ns)
#   #head(eg)
#   
#   # Building the denominator of the individual mkt share functions (proba to chose product j, for indiv i in market t)
#   J.Y<-rep(0,dim(Yr)[2])
#   for (i in 1:5){
#     J.Y[i]<-sum(Yr[,i])
#   }
#   #J.Y
#   NJ.col<-cumsum(J.Y)
#   #NJ.col
#   ind.NJ.col<-c(0,NJ.col)
#   ind.NJ.col
#   
#   sum1<-matrix(rep(0,ns*501),nrow=ns)
#   
#   for (i in 2:6){
#     sum1[,(ind.NJ.col[i-1]+1):(ind.NJ.col[i])]<-t(eg)%*%Yr[,i-1]
#   }
#   
#   denom<-1/(1+sum1)
#   
#   # Multiplying denominator and numerator in a element-wise fashion
#   
#   return(eg*t(denom)) # the output should be of dim = 501 20
#   
# }
