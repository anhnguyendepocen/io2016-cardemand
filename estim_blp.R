estim_blp <- function(x1, x2, IV, v){
  gmmobj_loc<- function(theta2_loc){
    gmmobj(theta2_loc, x1, x2, IV, v)
  }
  opt <- optimize(gmmobj_loc, lower = 0, upper = 10)
  
  delta_loc <- meanval(opt$minimum, x1, x2, IV, v)
  temp1 <- t(x1) %*% IV
  temp2 <- t(delta_loc) %*% IV
  invA <- solve(t(IV) %*% IV)
  theta1 <- solve(temp1%*%invA%*%t(temp1)) %*% (temp1%*%invA%*%t(temp2))

  estim <- rbind(theta1, opt$minimum)
  vcov <- var_cov(opt$minimum, x1, x2, IV, v)
  se <-  sqrt(diag(vcov))
  t <- estim/se
  ttests_pvalue <- 2*pnorm(t)
  estim <- cbind(estim, se, t, ttests_pvalue)
  colnames(estim) <- c("estim", "se", "t", "p-value")
  
  estim <- list(opt = opt$minimum, obj = opt$objective, estim = estim)
  return(estim)
}