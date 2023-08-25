# CMED6040 Practical 1
# example R script

# Question 1a
# log-likelihood dropping the constant term
logL <- function(theta){
	60*log(theta) + 508*log(1-theta)
}

curve(logL, from=0, to=0.25)

logL.exact <- function(theta){
	dbinom(60, 568, theta, log=T)
}

curve(logL.exact, from=0, to=0.25, ylim=c(-50,0), las=1, 
xlab=expression(paste('Prevalence (',theta,')')), ylab='log(L)')

# Question 1b
optim(0.1, logL, method="CG", control=list(fnscale=-1))
optim(0.1, logL, method="Brent", lower=0.01, upper=0.25, control=list(fnscale=-1))
optim(0.1, logL, method="L-BFGS-B", lower=0.01, upper=0.99, control=list(fnscale=-1))
# alternatively, you may define a new function -logL for minimization

# Question 1c
logL.pois <- function(lambda){
	330*log(lambda) - 720100*lambda
}

curve(logL.pois, from=0, to=0.001)

optim(0.0001, logL.pois, method="Brent", lower=0.00001, upper=0.001, control=list(fnscale=-1))



# Question 2a
sample <- rnorm(64, 2, 1)
mean(sample)
var(sample)

# Part 2b
# Method I: Direct calculation of test statistic , then p-value.
  # Assume known population variance
  teststat <- (mean(sample)-0)/(1/sqrt(64))
  pvalue <- pnorm(-abs(teststat), mean=0, sd=1, lower.tail=T) + 
pnorm(abs(teststat), mean=0, sd=1, lower.tail=F)
  pvalue

  # Assume population variance unknown
  test <- t.test(sample,mu=0) # Ho: mu=0
  pvalue <- test$p.value
  pvalue

# Method II: Simulate the sampling distribution of "estimate of population mean",
# then check for percentile of observed mean
  reference <- rnorm(10000, 0, 1/sqrt(64))
  mean(sample) > max(reference) # mean(sample) is even larger than max(reference)

# Method III: Simulate samples of size 64 from N(0,1) [the hypothesized distribution of data] 
# and count how often mean is more extreme than the "observed mean"
  count <- 0
  for (i in 1:1000) {
  hypo <- rnorm(64,0,1)
  if (mean(hypo) >= mean(sample)) count <- count + 1
  }
  count/1000

# Question 3b



x1 <- 147
x0 <- 183

n1 <- 434900
n0 <- 285200


## full likelihood
full <- function(x){
beta0 <- x[1]
beta1 <- x[2]
lambda0 <- exp(beta0)
lambda1 <- exp(beta0+beta1)

lik <- dpois(x0,n0*lambda0,1)+dpois(x1,n1*lambda1,1)
lik
}

optim(c(0.1,0.1), full , method="CG", control=list(fnscale=-1))


## simplified likelihood
prop <- function(x){
  beta0 <- x[1]
  beta1 <- x[2]

  lik <- (beta0*(x0+x1)+beta1*x1)- ( exp(beta0)*(n0) + exp(beta0+beta1)*n1      )
 return(lik)
}

optim(c(0.1,0.1), prop , method="CG", control=list(fnscale=-1))




