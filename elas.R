#elasticities

#for testing
# theta2 <- 0.02
# yrid
# yrindex
# x2 <- size
# mval <- exp(delta.vec)
# p <- df %>% select(p_adj) %>% as.matrix
# s <- df %>% select(s) %>% as.matrix

#tt <- elas(x1, size, IV, theta2, p, s) 

semi_elas <- function(theta2, x1, x2, IV, v, p, s, attr_coef){
  invA <- solve(t(IV) %*% IV)
  delta <- meanval(theta2, x1, x2, IV, v)
  mval <- exp(delta)
  
  expmu <- exp(mufunc(theta2, x1, x2, IV, v))
  shares <- ind_sh(mval, expmu)
  rm(expmu)
  
  
  temp1 <- t(x1) %*% IV
  temp2 <- t(delta) %*% IV
  W <- temp1%*%invA%*%t(temp1)
  B <- temp1%*%invA%*%t(temp2)
  theta1 <- solve(W,B)
  
  f <- matrix(0, nrow = length(yrid), ncol = length(yrid))
  
  # computing (partial s)/(partial delta)
  n <- 1
  for(t in 1:length(yrindex)){
    temp1 <- shares[n:yrindex[t],]
    H1 <- temp1 %*% t(temp1) #h1_kl = sum_{i in 1:ns} s_{kt,i} s_{lt,i} = - ns* partial s_{kt}/partial delta_{mt}
    H <- (diag(rowSums(temp1)) - H1)/ns #h_kl = partial s_{kt}/partial delta_{mt}, H is symmetric
    #f[n:yrindex[t]] <- - solve(H) %*% f1[n:yrindex[t]]
    #p_temp <- p[n:yrindex[t]]
    p1000 <- rep(1000, JT)
    p_temp <- p1000[n:yrindex[t]]
    s_temp <- s[n:yrindex[t]]
    f[n:yrindex[t], n:yrindex[t]] <- attr_coef*H*((1/s_temp) %*% t(p_temp))
    n <- yrindex[t] + 1
  }
  rm(n, H1, temp1,t, p_temp, s_temp, H)
  
  f
  }
