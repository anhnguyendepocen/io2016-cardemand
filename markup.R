# MARKUPS
# 
# #for testing
# theta2 <- 0.005
# # yrid
# # yrindex
# # x2 <- size
# # mval <- exp(delta.vec)
# p <- df %>% select(p_adj) %>% as.matrix
# s <- df %>% select(s) %>% as.matrix
# firmid <- df %>% select(firmids) %>% as.matrix


#tt <- markups(x1, x2, IV, theta2, firmid, s)

#p - tt

markups <- function(theta2, x1, x2, IV, v, firmid, s){
  invA <- solve(t(IV) %*% IV)
  
  delta <- meanval(theta2, x1, x2, IV, v)
  mval <- exp(delta)
  x2 <- x2
  expmu <- exp(mufunc(theta2, x1, x2, IV, v))
  shares <- ind_sh(mval, expmu)
  rm(expmu)
  
   temp1 <- t(x1) %*% IV
   temp2 <- t(delta) %*% IV
   W <- temp1%*%invA%*%t(temp1)
   B <- temp1%*%invA%*%t(temp2)
   theta1 <- solve(W,B)
   rm(temp1, temp2, W, B)

  alpha <- -theta1[length(theta1)]
  
  H <- matrix(0, nrow = length(yrid), ncol = length(yrid))
  
  # computing (partial s)/(partial delta)
  n <- 1
  for(t in 1:length(yrindex)){
    temp1 <- shares[n:yrindex[t],]
    H1 <- temp1 %*% t(temp1) #h1_kl = sum_{i in 1:ns} s_{kt,i} s_{lt,i} = - ns* partial s_{kt}/partial delta_{mt}
    H[n:yrindex[t],n:yrindex[t]] <- (diag(rowSums(temp1)) - H1)/ns #h_kl = partial s_{kt}/partial delta_{mt}, H is symmetric
    n <- yrindex[t] + 1
  }
  rm(n, H1, temp1,t)
  
  # computing matrix Delta as defined by BLP 1994, p.854
  
  row.firmid <- firmid %*% rep(1, length(firmid))
  col.firmid <- t(row.firmid)
  
  Delta <- - (-alpha)*H*(row.firmid == col.firmid)

  markup <- solve(Delta) %*% s
  markup
}
