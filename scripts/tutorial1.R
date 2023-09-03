# Tutorial 1

# (a)
mle <- 13/185

logL <- function(theta){
  13*log(theta) + 172*log(1 - theta)
}

logL.ratio <- function(theta) return(2*abs(logL(mle) - logL(theta)))

curve(logL.ratio, from=mle*0.5, to=mle*2, xlab=expression(theta)) # Visualize logL.ratio

lr.f <- function(theta){
  return(abs(2*abs(logL(mle) - logL(theta)) - qchisq(0.95, 1)))
}

curve(lr.f, from=0.03, to=0.15, xlab=expression(theta)) # Check range

theta.lr.lb1 <- optim(0.04, lr.f, method="Brent", lower=0.03, upper=0.05)$par
theta.lr.ub1 <- optim(0.11, lr.f, method="Brent", lower=0.1, upper=0.12)$par
c(theta.lr.lb1, theta.lr.ub1) # 95% CI=(0.04, 0.11)

lr.f2 <- function(theta){
  return((2*abs(logL(mle) - logL(theta)) - qchisq(0.95, 1))^2)
}

theta.lr.lb2 <- optim(0.04, lr.f2, method="Brent", lower=0.03, upper=0.05)$par
theta.lr.ub2 <- optim(0.11, lr.f2, method="Brent", lower=0.1, upper=0.12)$par
c(theta.lr.lb2, theta.lr.ub2) # 95% CI=(0.04, 0.11) # 95% CI=(0.04, 0.11)


# (b)
data.obese <- data.frame(id=1:185, obese=(c(rep(1,13), rep(0, 172))))

obese.out <- function(data, indices){
  newdata <- data[indices, ]
  return(mean(newdata$obese))
}

require(boot)
obese.boot.out <- boot(data.obese, obese.out, R=1000)
boot.ci(obese.boot.out) # 95% CI=(0.03, 0.11)


# (c)
n.obese <- c(13, 18, 21, 10, 11, 10, 17, 12)
n.sample <- c(185, 161, 272, 154, 85, 101, 221, 150)

logL.i <- function(theta, n.obese, n.sample) return(n.obese*log(theta) + (n.sample - n.obese)*log(1 - theta))
logL.all <- function(theta) return(sum(logL.i(theta, n.obese, n.sample)))

logL.all2 <- function(theta){
  ret <- 0
  for (i in 1:8){
    ret <- ret + n.obese[i]*log(theta) + (n.sample[i] - n.obese[i])*log(1 - theta)
  }
  return(ret)
}
curve(logL.all2, from=0.01, to=0.15, xlab=expression(theta), las=1)

all.out <- optim(0.07, logL.all, method="Brent", lower=0.0001, upper=0.9999, control=list(fnscale=-1))
all.out # 0.084 (8.4%)

all.out2 <- optim(0.07, logL.all2, method="Brent", lower=0.0001, upper=0.9999, control=list(fnscale=-1))
all.out2 # 0.084 (8.4%)


# (d)
large.sample <- 1*(n.sample > 200) # large sample indicator

logL.i.group <- function(theta, n.obese, n.sample){
  return(n.obese*log(theta[1]*theta[2]^large.sample) +
           (n.sample - n.obese)*log(1-theta[1]*theta[2]^large.sample))
}
logL.group <- function(theta) return(sum(logL.i.group(theta, n.obese, n.sample)))

group.out <- optim(c(0.07, 1), logL.group, method="L-BFGS-B", 
                   lower=c(0.0001, 0.0001), upper=c(0.49, 2), control=list(fnscale=-1))
group.out # 0.09, 0.87
# k=0.87


# (e)
single.out <- optim(0.05, logL, method="Brent", lower=0.01, upper=0.2, control=list(fnscale=-1), hessian=T)

se.theta <- sqrt(-1/single.out$hessian); se.theta # 0.019
single.out$par + c(-1, 1)*qnorm(0.975)*se.theta # 95% CI=(0.03, 0.11)


# (f)
group.out <- optim(c(0.07, 1), logL.group, method="L-BFGS-B",
                   lower=c(0.0001, 0.0001), upper=c(0.49, 2), control=list(fnscale=-1), hessian=T)

se.k <- sqrt(solve(-group.out$hessian)[2,2])
group.out$par[2] + c(-1,1)*qnorm(0.975)*se.k # 95% CI=(0.54, 1.20)

(group.out$par[2] - 1)/se.k # -0.78 < 1.96, null hypothesis not rejected


# (g)
group.out.0 <- optim(0.07, logL.all, method="L-BFGS-B", lower=0.0001, upper=0.9999, control=list(fnscale=-1))

logL0 <- logL.all(group.out.0$par[1]); logL0 # -384.19
logL1 <- logL.group(c(group.out$par[1], group.out$par[2])); logL1 # -383.93

LRT <- 2*abs(logL0 - logL1); pchisq(LRT, df=1, lower.tail=F)
# LRT=0.53, p=0.47 > 0.05









