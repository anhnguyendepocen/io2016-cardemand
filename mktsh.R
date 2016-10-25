# computes the simulated market shares per market (year) and product
#  mktsh = 1/ns sum_{i} s_{jt,i} 
  # s_{jt,i} is individual i's (drawn) probability to choose product j in market t

mktsh <- function(expmval,expmu) { # in the previous function, mval argument was called expmval
  res <- rowSums(ind_sh(expmval,expmu))/ns
  return(res)
}
