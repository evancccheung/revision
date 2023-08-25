# Pratical 1


# 1(a)
logL <- function(theta){
  dbinom(60, 568, theta, log=T)
}

curve(logL, from=0, to=0.25, xlim=c(0, .25), ylim=c(-50, 0), col="blue", lwd=2,
      xlab=expression(paste("Pravalence (", theta, ")")), ylab="log(L)", las=1)

# 1(b)
?optim # maximize log-likelihood

optim(0.1, logL, method="BFGS",control=list(fnscale=-1)) # 0.106
optim(0.1, logL, method="L-BFGS-B", control=list(fnscale=-1), lower=0, upper=0.25) # 0.106
optim(0.1, logL, method="Brent",control=list(fnscale=-1), lower=0, upper=0.25) # 0.106

# 1(c)
logL2 <- function(lambda){
  dbinom(330, 720100, lambda, log=T)
}

curve(logL2, from=0, to=0.0015, xlim=c(0, .0015), ylim=c(-600, 0), col="blue", lwd=2,
      xlab=expression(paste("influenza incidence (", lambda, ")")), ylab="log(L)", las=1)


# 2(a)
set.seed(6150)
sample <- rnorm(64, 2, 1)
mean(sample); var(sample) # mean = 1.82, var = 0.925

# 2(b)
t.test(sample, mu=0)$p.value # p-value << 0


# 3(b)
# Simplified version
x0 = 183
x1 = 147
n0 = 285200
n1 = 434900

logL.simp <- function(x){
  beta0 <- x[1]
  beta1 <- x[2]
  
  lik <-((x0+x1)*beta0 + x1*beta1) - n0*exp(beta0) - n1*exp(beta0 + beta1)
}

optim(c(0.1, 0.1), logL.simp, method="BFGS",control=list(fnscale=-1)) # x0=-7.35, x1=-0.641

# Full version

logL.full <- function(x){
  beta0 <- x[1]
  beta1 <- x[2]
  lambda0 <- exp(beta0)
  lambda1 <- exp(beta0 + beta1)
  
  lik <- dpois(x0, n0*lambda0, log=T)+dpois(x1, n1*lambda1, log=T)
}

optim(c(0.1, 0.1), logL.full, method="BFGS",control=list(fnscale=-1)) # x0=-7.35, x1=-0.641




