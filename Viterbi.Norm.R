Viterbi <- function(tau, obs, Q, delta, mu, sigma){
n <- length(tau)
m <- nrow(Q)
optimpath <- matrix(NA, nrow=n, ncol=m)
logforward <- matrix(NA, nrow=n, ncol=m)
z <- rep(NA, n)
for (i in 1 : m){
     logforward[1 , i] <- log(delta[i])
     optimpath[1 , i] <- 0
     }
for (i in 2 : (n)){
  u <- expm(Q*tau[i-1])
  for(j in 1:m){
   optimpath[i,j] <- k <- which.max(logforward[i-1,] + log(u[,j]))
   logforward[i,j] <- logforward[i-1,k] + log(u[k,j]) + log(dnorm(obs[i],mu[j],sigma[j]))
               }
                    }
z[n] <- which.max(logforward[n,])
for (i in (n):2){
   z[i-1] <- optimpath[i,z[i]]
                   }
return(z)
}