# Simulate the posterior distribution

# part 1
xlim.plot <- c(0,0.3); ylim.plot <- c(0,40)
col.prior <- "blue"; col.posterior <- "red"

# prior ~ beta(6.6,60)
n.sim <- 100000
n.sample <- 568
n_prior_base <- c(6.6, 60)

# simulate the prior distribution
sim.theta <- rbeta(n.sim, n_prior_base[1], n_prior_base[2])
plot(density(sim.theta), col=col.prior, lwd=2, xlim=xlim.plot, ylim=ylim.plot)

# theoretically, the mode should be (6.6-1)/(6.6+60-2) = 0.087
#abline(v=(6.6-1)/(6.6+60-2),lty=2)
# the simulated distribution is consistent

# part 2
# simulate the joint distribution for theta, x
sim.Y <- rbinom(n.sim, n.sample, sim.theta)

# part 3
plot(sim.theta, sim.Y, cex=0.5)
abline(h=60, col="red") # condition on the observed 60/568 cases

# part 4
# simulate prior and posterior distribution

x <- 60
n_posterior <- rep(NA,2)
n_posterior[1] <- n_prior_base[1] + x
n_posterior[2] <- n_prior_base[2] + n.sample - x

plot(density(sim.theta[sim.Y==60]), xlim=xlim.plot, ylim=ylim.plot, col=col.posterior, main="simulated prior and posterior", las=1)
# theoretic posterior distribution
#curve(dbeta(x,n_posterior[1],n_posterior[2]), from=0, to=1, n=501, add=TRUE, type="l", xname="x", lty=2, col=col.posterior)
lines(density(sim.theta), col=col.prior, lwd=2)

# Baysiean point est = mean(sim.theta[sim.Y==60])
# 95% credible interval : quantile(sim.theta[sim.Y==60],  probs = c(5, 95)/100)

# Example for effect of different priors on the posterior
timepause <- 0.5

ntime <- 12
x <- 60

range_beta1 <- c(3,12)
range_beta2 <- c(30,70)

windows(height=9, width=12)
par(mar=c(3,2,2,0.5))
layout(matrix(1:ntime, ncol=4,nrow=3, byrow=TRUE))

for (itime in 1:ntime){
  
  n_prior_base <- c(runif(1,range_beta1[1],range_beta1[2]), runif(1,range_beta2[1],range_beta2[2]))
  
  # based on the observed 60/568 cases
  n_posterior <- rep(NA,2)
  n_posterior[1] <- n_prior_base[1] + x
  n_posterior[2] <- n_prior_base[2] + n.sample - x
  
  # sample new cases (assuming the prior is the true distribution)
  #tout <- sample(c(1,0), size=n.sample, replace=TRUE, n_prior_base) # sample with simple sampling
  #n_posterior <- rep(NA,2)
  #n_posterior[1] <- n_prior_base[1] + sum(tout)
  #n_posterior[2] <- n_prior_base[2] + n.sample - n_prior[1]
  
  curve(dbeta(x,n_posterior[1],n_posterior[2]), from=0, to=1, n=501, add=FALSE, type="l", xname="x", xlab="", ylab="density", bty="L", xlim=xlim.plot, ylim=ylim.plot, las=1, col=col.posterior)
  curve(dbeta(x,n_prior_base[1],n_prior_base[2]), from=0, to=1, n=501, add=TRUE, type="l", lwd=3, col=col.prior)
  
  title(sprintf("prior=beta(%.1f,%.1f)", n_prior_base[1],n_prior_base[2]), col.main=col.prior)
  
  Sys.sleep(timepause)
  
}