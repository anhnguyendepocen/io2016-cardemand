#elasticities

#for testing
# sigma <- 0.02
# yrid
# yrindex
# x2 <- size
# mval <- exp(delta.vec)
# p <- df %>% select(p_adj) %>% as.matrix
# s <- df %>% select(s) %>% as.matrix

#tt <- elas(x1, x2, IV, sigma, p, s)

elas <- function(x1, x2, z, sigma, p, s){
  delta <- meanval(sigma)
  mval <- exp(delta)
  
  expmu <- exp(mufunc(x2, sigma))
  shares <- ind_sh(mval, expmu)
  rm(expmu)
  
  
  x1.i<-cbind(rep(1,501),x1)
  temp1 <- t(x1.i) %*% z
  temp2 <- t(delta) %*% z
  W <- temp1%*%invA%*%t(temp1)
  B <- temp1%*%invA%*%t(temp2)
  theta1 <- solve(W,B)
  
  alpha <- -theta1[length(theta1)]
  
  f <- matrix(0, nrow = length(yrid), ncol = length(yrid))
  
  # computing (partial s)/(partial delta)
  n <- 1
  for(t in 1:length(yrindex)){
    temp1 <- shares[n:yrindex[t],]
    H1 <- temp1 %*% t(temp1) #h1_kl = sum_{i in 1:ns} s_{kt,i} s_{lt,i} = - ns* partial s_{kt}/partial delta_{mt}
    H <- (diag(rowSums(temp1)) - H1)/ns #h_kl = partial s_{kt}/partial delta_{mt}, H is symmetric
    #f[n:yrindex[t]] <- - solve(H) %*% f1[n:yrindex[t]]
    p_temp <- p[n:yrindex[t]]
    s_temp <- s[n:yrindex[t]]
    f[n:yrindex[t], n:yrindex[t]] <- - alpha*H* (p_temp %*% t(s_temp))
    n <- yrindex[t] + 1
  }
  rm(n, H1, temp1,t, p_temp, s_temp, H)
  
  f
  }
