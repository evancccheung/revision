# Question 1
# 1(a)
mle <- 13/185
logL <- function(theta) return(13*log(theta)+172*log(1-theta))

logL.ratio <- function(theta) return(2*abs(logL(mle)-logL(theta)))

# likelihood ratio statistics for different theta
curve(logL.ratio, from=mle*0.5, to=2*mle, xlab=expression(theta))
abline(h=qchisq(0.95,1), lty=2)
abline(v=mle)

# obtaining the 95% CI
lr.f <- function(theta){
  return(abs(2*abs(logL(mle)-logL(theta))-qchisq(0.95,1)))
}

lr.f2 <- function(theta){
  return( (2*abs(logL(mle)-logL(theta))-qchisq(0.95,1)) ^2)
}

curve(lr.f, from=0.03, to=0.15, xlab=expression(theta))
#curve(lr.f, from=0.03, to=0.05, xlab=expression(theta))

optim(0.04, lr.f, method="Brent", lower=0.03, upper=0.05)
theta.lr.lb1 <- optim(0.04, lr.f, method="Brent", lower=0.03, upper=0.05)$par
theta.lr.ub1 <- optim(0.12, lr.f, method="Brent", lower=0.1, upper=0.13)$par


c(theta.lr.lb1, theta.lr.ub1) # 95% CI is (0.04-0.11)

theta.lr.lb2 <- optim(0.04, lr.f2, method="Brent", lower=0.03, upper=0.05)$par
theta.lr.ub2 <- optim(0.12, lr.f2, method="Brent", lower=0.1, upper=0.13)$par

c(theta.lr.lb2, theta.lr.ub2) # 95% CI is (0.04-0.11)

# 1(b) bootstrap method
# create a dataset - it's just 0 and 1s (obese or not)

data.obese <- data.frame(id=1:185, obese=c(rep(1,13),rep(0,172)))

obese.out <- function(data, indices){
  newdata <- data[indices,]
  return(mean(newdata$obese))
}

require(boot)
obese.boot.out <- boot(data=data.obese, statistic=obese.out, R=1000)
boot.ci(obese.boot.out)

# 1(c) multiple samples
n.obese <- c(13, 18, 21, 10, 11, 10, 17, 12)
n.sample <- c(185, 161, 272, 154, 85, 101, 221, 150)

logL.i <- function(theta,n.obese,n.sample) return(n.obese*log(theta)+(n.sample-n.obese)*log(1-theta))
logL.all <- function(theta) return(sum(logL.i(theta, n.obese, n.sample)))


logL.all2 <- function(theta){
        ret <- 0
        for (i in 1:8){
                ret <- ret + n.obese[i]*log(theta)+(n.sample[i]-n.obese[i])*log(1-theta)
        }
        return(ret)
}
curve(logL.all2, from=0.01, to=0.15, xlab=expression(theta))

# list(fnscale=-1) change minimization to maximization
all.out <- optim(0.07, logL.all, method="Brent", lower=0.0001, upper=0.9999, control=list(fnscale=-1))
all.out

all.out2 <- optim(0.07, logL.all2, method="Brent", lower=0.0001, upper=0.9999, control=list(fnscale=-1))
all.out2


# 1(d) estimate group difference
large.sample <- 1*(n.sample > 200) # large sample indicator

# when not large sample, theta[2]^0 == 1
logL.i.group <- function(theta,n.obese,n.sample) return(n.obese*log(theta[1]*theta[2]^large.sample)+
(n.sample-n.obese)*log(1-theta[1]*theta[2]^large.sample))
logL.group <- function(theta) return(sum(logL.i.group(theta, n.obese, n.sample)))

group.out <- optim(c(0.07,1), logL.group, method="L-BFGS-B", lower=c(0.0001,0.0001), upper=c(0.49,2), 
control=list(fnscale=-1))
group.out 

# illustration: log-likelihood curvature
# (sample from 1 institution)
logL <- function(theta) return(13*log(theta)+172*log(1-theta))

# higher curvature -> lower uncertainty / narrower confidence interval
curve(logL, from=0.01, to=0.2)

# 1(e) Variance from information matrix
single.out <- optim(0.05, logL, method="Brent", lower=0.01, upper=0.2, control=list(fnscale=-1), hessian=T)

# 95% CI from likelihood theory
se.theta <- sqrt(-1/single.out$hessian)
single.out$par+c(-1,1)*qnorm(0.975)*se.theta

# 1(f) 

logL.group <- function(theta) return(sum(logL.i.group(theta, n.obese, n.sample)))

group.out <- optim(c(0.07,1), logL.group, method="L-BFGS-B", lower=c(0.0001,0.0001), upper=c(0.49,2), 
control=list(fnscale=-1), hessian=T)

est.k <- group.out$par[2]
se.k <- sqrt(solve(-group.out$hessian)[2,2])

# 95% CI
est.k+c(-1,1)*qnorm(0.975)*se.k


# Wald statistics < 1.96: do not reject null hypothesis
(est.k-1) / se.k

# correlation between estimates
cov2cor(-solve(group.out$hessian))

# 1(g)
logL.i.group.k1 <- function(theta,n.obese,n.sample) return(n.obese*log(theta[1])+
(n.sample-n.obese)*log(1-theta[1]))
logL.group.k1 <- function(theta) return(sum(logL.i.group.k1(theta, n.obese, n.sample)))

group.k1.out <- optim(c(0.07), logL.group.k1, method="L-BFGS-B", lower=0.0001, upper=0.99, 
control=list(fnscale=-1))
group.k1.out 

est.theta.group.k1 <- group.k1.out$par[1]
est.theta.group <- group.out$par[1]

logL0 <- logL.group.k1(c(est.theta.group.k1))
logL1 <- logL.group(c(est.theta.group, est.k))

LRT <- 2*abs(logL0-logL1)
pchisq(LRT, df=1, lower.tail=F)

####