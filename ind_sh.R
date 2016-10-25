
## Building the ind_sh function (individual market shares)

ind_sh<-function (expmval,expmu){
  
  # Building the numerator of the indiv market share (eg matrix) dim(eg)= JT ns
  
  eg<-expmu*matrix(kronecker(rep(1,ns),expmval),ncol=ns)
  #head(eg)
  
  # Building the denominator of the individual mkt share functions (proba to chose product j, for indiv i in market t)
  J.Y<-rep(0,dim(Yr)[2])
  for (i in 1:5){
    J.Y[i]<-sum(Yr[,i])
  }
  #J.Y
  NJ.col<-cumsum(J.Y)
  #NJ.col
  ind.NJ.col<-c(0,NJ.col)
  ind.NJ.col
  
  sum1<-matrix(rep(0,ns*501),nrow=ns)
  
  for (i in 2:6){
    sum1[,(ind.NJ.col[i-1]+1):(ind.NJ.col[i])]<-t(eg)%*%Yr[,i-1]
  }
  
  denom<-1/(1+sum1)
  
  # Multiplying denominator and numerator in a element-wise fashion
  
  return(eg*t(denom)) # the output should be of dim = 501 20
  
}