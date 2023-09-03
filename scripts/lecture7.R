# Lecture 7 - Marginal structural model

# Simulated data
set.seed(111)
n <- 1000
simdat <- data.frame(l = rnorm(n, 10, 5))
a.link <- simdat$l - 10
pa <- exp(a.link)/(1 + exp(a.link))
simdat$a <- rbinom(n, 1, prob=pa)
simdat$y <- 10*simdat$a + 0.5*simdat$l - 10 + rnorm(n, 0, 5)
simdat[1:4, ]

# estimate weights
if(!require(ipw)) {install.packages("ipw"); require(ipw)}
temp <- ipwpoint(exposure = a, family = "binomial", link = "logit", numerator = ~ 1, denominator = ~ l, data = simdat)
summary(temp$ipw.weights)

ipwplot(weights=temp$ipw.weights, logscale=F, main="Stabilized weights", xlim=c(0,8))

# IPW models
summary(temp$num.mod)

# obtain IPTW estimates
simdat$sw <- temp$ipw.weights
require(survey)

msm <- svyglm(y~a, design=svydesign(~1, weights= ~ sw, data=simdat))
summary(msm)
# Estimated causal effect of A on Y is 10.9 (95% CI: 9.2, 12.5)

# MSM using unstabilized weight
temp.uns <- ipwpoint(exposure=a, family="binomial", link="logit", denominator=~l, data=simdat)
summary(temp.uns$ipw.weights)

ipwplot(weights=temp.uns$ipw.weights, logscale=F, main="Unstabilized weights", xlim=c(0,8))
# Stabilized weights reduce variation

simdat$unsw <- temp.uns$ipw.weights
msm.uns <- svyglm(y~a, design=svydesign(~1, weights=~unsw, data=simdat))
summary(msm.uns); confint(msm.uns)[2, ]
# Estimated causal effect of A on Y is 10.9 (95% CI: 9.23, 12.5)

# MSM with time-dependent weights
data(haartdat)
haartdat[haartdat$patient==18, ]
haartdat[haartdat$patient==63, ]

# Standard time-dependent Cox regression
cox0 <- coxph(Surv(tstart, fuptime, event)~haartind+sex+age+cd4.sqrt+cluster(patient), data=haartdat)
summary(cox0)
# For patients receiving HAART: HR=0.54 (95% CI: 0.24, 1.22)

# IPW
temp <- ipwtm(exposure=haartind, family="survival", numerator=~sex+age, denominator=~cd4.sqrt+sex+age, 
              id=patient, tstart=tstart, timevar=fuptime, type="first", data=haartdat)
temp.uns <- ipwtm(exposure=haartind, family="survival", denominator=~cd4.sqrt+sex+age, 
                  id=patient, tstart=tstart, timevar=fuptime, type="first", data=haartdat)

ipwplot(weights=temp$ipw.weights, timevar=haartdat$fuptime, binwidth=100, ylim=c(-1.5, 1.5), main="Stabilized weights",
        xaxt="n", yaxt="n", xlab="follow-up time", ylab="log(weight)")
axis(side=1, at=0:7*5, labels=0:7*500)
axis(side=2, at=-3:3*0.5, labels=-3:3*0.5, las=1)

summary(temp$ipw.weights)
summary(temp.uns$ipw.weights)
# unstabilized weights have much larger variation

ipwplot(weights=temp.uns$ipw.weights, timevar=haartdat$fuptime, binwidth=100, ylim=c(-1, 8), main="Unstabilized weights",
        xaxt="n", yaxt="n", xlab="follow-up time", ylab="log(weight)")
axis(side=1, at=0:7*5, labels=0:7*500)
axis(side=2, at=c(-1, 0, 1, 2, 4, 8), labels=c(-1, 0, 1, 2, 4, 8), las=1)

# account for informatie censoring
temp2 <- ipwtm(exposure=dropout, family="survival", numerator=~sex+age, denominator=~cd4.sqrt+sex+age,
               id=patient, tstart=tstart, timevar=fuptime, type="first", data=haartdat)
ipwplot(weights=temp2$ipw.weights, timevar=haartdat$fuptime, binwidth=100, ylim=c(-1.5, 1.5),
        main="Stabilized weights", xaxt="n", yaxt="n", xlab="follow-up time", ylab="log(weight)")
axis(side=1, at=0:7*5, labels=0:7*500)
axis(side=2, at=-3:3*0.5, labels=-3:3*0.5, las=1)

# Fitting MSM using unstabilized weights
require(survival)
summary(coxph(Surv(tstart, fuptime, event)~haartind+cluster(patient), data=haartdat, 
             weights=temp$ipw.weights*temp2$ipw.weights))
# Patient receiving HAART: HR=0.39 (95% CI: 0.16-0.95)

# Fitting MSM using unstabilized weights
temp2.uns <- ipwtm(exposure=dropout, family="survival", denominator=~cd4.sqrt+sex+age, id=patient, 
                   tstart=tstart, timevar=fuptime, type="first", data=haartdat)
summary(coxph(Surv(tstart, fuptime, event)~haartind+cluster(patient), data=haartdat, 
              weights=temp.uns$ipw.weights*temp2.uns$ipw.weights))
# Patient receiving HAART: HR=1.34 (95% CI: 0.43, 0.44) -> much wider interval







