Uniform <- function(states, jumptimes, rates){

##### simulate Poisson times with a constant Poisson rate ########
       initial <- jumptimes[1]
       resolution <- 6
       Poissonrate <- resolution*max(rates)
       Poissontimes <- NULL
       k <- length(states)
       while (initial <= jumptimes[k+1]){
       Poissontime <- rexp(1, Poissonrate)
       initial <- initial +Poissontime
       if(initial < jumptimes[k+1]) Poissontimes <- c(Poissontimes, initial)
              }
#######  Tninning ##############
  i <- 1
  j <- 2
  tag <- rep(NA, length(Poissontimes))
  while ((Poissontimes[i] < jumptimes[k+1]) && (i <= length(Poissontimes))){
   if (Poissontimes[i] <= jumptimes[j]){
       u <- runif(1)
       if (u <= resolution*rates[j-1]/Poissonrate)  tag[i] <- TRUE
       else tag[i] <- FALSE
       i <- i + 1
       }
   else j <- j + 1
   }
       Poissontimes <- Poissontimes[tag]
       Poissontimes <- c(Poissontimes, jumptimes[-c(1,k+1)])
       Poissontimes <- Poissontimes[order(Poissontimes)]
       k <- length(Poissontimes)
       JumpProb <- matrix(rep(NA, 2*k), ncol=2)
       i <- 1
       j <- 2
       while (i <= k){
       if(Poissontimes[i] <= jumptimes[j]){
       JumpProb[i,1] <- 1-rates[j-1]/(resolution*rates[j-1]+rates[j-1])
       JumpProb[i,2] <- rates[j-1]/(resolution*rates[j-1]+rates[j-1])
       i <- i+1
       }
       else j <- j+1
       }
   return(list(Uniformtimes=Poissontimes, JumpProb=JumpProb))
}