
# Recovering optimal expval (exponential of delta). Delta is obtained through a contraction mapping. This will use the mkt.shares function defined above.

meanval<-function(sigma){
  
  # Convergence structure
  # The tolerance is decreasing with the level of the steps of the sigma
  if (max(abs(sigma-old.sigma))<crit) {
    tol <- 1e-9
    flag<-0
  } else {
    tol<- 1e-6
    flag<-1
  }
  expmu<-exp(mufunc(size,sigma))
  #dim (expmu) # 501 20
  norm<-1
  avgnorm<-1
  
  # While loop, performed until we find a almsot fix point for delta. The tolerance of this 'almost' fix point will depend on the level of steps of sigma as well as on the number of iterations already processed.
  
  i <- 0
  while (norm>tol*10^(flag*floor(i/50)) & avgnorm>1e-3*tol*10^(flag*floor(i/50))) {
    mval<-mvalold*y.MS.vec/mktsh(mvalold,expmu) # Bear in mind that here the mval and mvalold are actually exponentials, ie expmval & expmvalold. We keep this more simple notation (without exp) to not do any typos.
    # Remember that y.MS.vec is the vector of observed market shares (per product per market)
    
    t<-abs(mval-mvalold)
    norm<-max(t)
    avgnorm<-mean(t)
    mvalold <<- mval
    i<-i+1
  }
  
  
  
  # print("# of iteration for delta convergence: ", i)
  
  if (flag== 1 & max (is.nan(mval))<1) {
    mvalold <<- mval
    old.sigma <<- sigma
  }
  
  nb.it <<- i
  return(log(mval))    # here we remove the 'exp' that was implied in the loop.  
}
