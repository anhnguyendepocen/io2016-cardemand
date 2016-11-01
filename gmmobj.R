gmmobj <- function (theta2, x1, x2, IV, v) {
  delta <- meanval(theta2, x1, x2, IV, v)
  if (max(is.nan(delta))==1) {
    f <- 1e+10
  } else {
    temp1 <- t(x1) %*% IV
    temp2 <- t(delta) %*% IV
    invA <- solve(t(IV) %*% IV)
    theta1<- solve(temp1%*%invA%*%t(temp1)) %*% (temp1%*%invA%*%t(temp2))
    rm(temp1, temp2)
    gmmresid <- delta - x1 %*% theta1
    temp1 <- t(gmmresid) %*% IV
    f <- temp1 %*% invA %*% t(temp1)
    rm(temp1)
  }
  gmmresid <<- gmmresid
  #print('GMM objective:  ', as.numeric(f))
  return(as.numeric(f))
}

