gmmobj <- function (sigma) {
  delta <- meanval(sigma)
  if (max(is.nan(delta))==1) {
    f <-1e+10
  }
  else {
    x1.i <- cbind(rep(1,501), x1)
    temp1 <- t(x1.i) %*% IV
    temp2 <- t(delta) %*% IV
    W <- temp1 %*% invA %*% t(temp1)
    B <- temp1 %*% invA %*% t(temp2)
    theta1 <- solve(W,B)
    #theta1<-inv(temp1%*%invA%*%t(temp1))%*%temp1%*%invA%*%t(temp2)
    gmmresid <- delta - x1.i %*% theta1
    #gmmresid
    temp3 <- t(gmmresid) %*% IV
    f <- temp3 %*% invA %*% t(temp3)
  }
  return(as.numeric(f))
  
  #coef1 <<- theta1
  #store gmmresid
}

