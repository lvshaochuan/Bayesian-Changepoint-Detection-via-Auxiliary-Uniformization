  CPnum <- changepoints <- Changepoints <- params <- jumprates <- NULL
################################################################################
cp <- res$cp
CP <- res$CP
param <- res$param
Q <- res$Q

rate <- 2
threshold <- 15
space <- 5
################################################################################  
  for (j in 1:iter){
  cat("Iteration", j)  
  if (res$Number == 1){
  
  flag <- FALSE
  while (flag==FALSE){
  A <- Uniform(1, c(starting, ending), 1/(ending-starting)*rate)
  Uniformtimes <- A$Uniformtimes
  if (any(diff(c(starting,Uniformtimes,ending)) <= diff(eventtimes[1:2]))) next
  if (any(diff(c(starting,Uniformtimes,ending)) > space)){
  cat("\n Wide Uniform \n")
  next
  }
#  if(length(Uniformtimes)<=threshold) next
  JumpProb <- A$JumpProb
#  print(length(Uniformtimes))
  if (length(Uniformtimes) <= 300) flag <- TRUE
  }
  }
  else {  
  m <- length(CP) - 1
  rates <- -diag(Q)
  rates <- rates[-length(rates)]
  rates <- c(rates, 1/(CP[m+1]-CP[m]))
  flag <- FALSE
  while (flag==FALSE){
  A <- Uniform(1:m, CP, rates)
  Uniformtimes <- A$Uniformtimes
  if (any(diff(c(starting,Uniformtimes,ending)) <= diff(eventtimes[1:2]))) next
  if (any(diff(c(starting,Uniformtimes,ending)) > space)){
  cat("Wide Uniform \n")
  next
  }
  if(length(Uniformtimes)<=2) next
  JumpProb <- A$JumpProb
#  print(length(Uniformtimes))
  if (length(Uniformtimes) <= 300) flag <- TRUE
  }
  }
  res <- FFBS(Uniformtimes, JumpProb, eventtimes, obs)
  num <- res$Number
  cp <- res$cp
  CP <- res$CP
#  param <- res$param
  Q <- res$Q 
  
  if ((res$CP[1]==0)&&(res$CP[2]==ending)) {
  num <- 1
  cp <- NULL
  CP <- c(starting, ending)
# param <- res$param
  Q <- NULL  
  }

  if (j > burnin){
  CPnum <- c(CPnum, num)
  changepoints <- c(changepoints, list(cp))
  Changepoints <- c(Changepoints, list(CP))
  jumprates <- c(jumprates, list(Q))
# params <- c(params, list(param))
        }
  }  
  