
FDR <- function(NumIterations=10000,m,ptruth,l){
  
  m0 = ptruth * m ; m1 = m-m0
  
  FDR <- c();power <- c()
  for (t in 1:NumIterations){
      
    truth = sort(sample(m)[1:m0])
    false = setdiff(c(1:m),truth)
    
    pp = c()
    for (i in truth) {pp[i] = min(pnorm(rnorm(1)),1-pnorm(rnorm(1)))*2}
    
    for (i in false[1:(m1/4)])  {pp[i] = (1-pnorm(rnorm(1) + l/4))*2}
    for (i in false[(m1/4+1):(m1/2)]) {pp[i] = (1-pnorm(rnorm(1) + l/2))*2}
    for (i in false[(m1/2+1):(m1*3/4)]) {pp[i] = (1-pnorm(rnorm(1) + 3*l/4))*2}
    for (i in false[(m1*3/4+1):(m1)]) {pp[i] = (1-pnorm(rnorm(1) + l))*2}
    
    pp_sort = sort(pp)/c(1:m)
    q = 0.05
    y = which(pp_sort <= q/m)
    if (length(y)>0) {
      k = max(y)    
      reject = order(pp,decreasing = F)[1:k]
      FDR[t] = length(intersect(reject,truth))/length(reject)
      power[t] = length(intersect(reject,false))/length(false)
    }
  }
  return(list(FDR = FDR,power=power))
}
    
    
    
    
    
    
    