# Lecture 4

# Kaplan-Meier estimates for 2 groups
if(!require(survival)) {install.packages("survival"); require(survival)}

hpa <- read.csv("data/hpa.csv")

hpa.km2 <- survfit(Surv(time, event, type="right")~staining, data=hpa)
plot(hpa.km2, col=c(4,2), conf.int=F, mark.time=T)

survdiff(Surv(time, event, type="right")~staining, data=hpa) # compare estimates
# p=0.06, no significant difference

# Cox model (slide 19)
hpa.cox <- coxph(Surv(time, event)~staining, data=hpa)
summary(hpa.cox)
# HR=2.48, 95% CI=(0.93, 6.63), p=0.07
# Insignificat association between postive HPA staining and increased risk of death

plot(hpa.km2, fun="cloglog", lty=1:2, mark.time=T) 

hpa.cox.zph <- cox.zph(hpa.cox)
hpa.cox.zph # no significant interaction with time
plot(hpa.cox.zph)

# Accelerated failure time (AFT) model
hpa.aft1 <- survreg(Surv(time, event)~staining, data=hpa, dist="lognormal")
hpa.aft1

exp(coef(hpa.aft1)[2])
exp(confint(hpa.aft1))
# AF=0.32, 95% CI=(0.11, 0.88), p=0.03
# positive HPA staining is associated with faster death

plot(hpa.km2, lty=1:2, mark.time=T)
curve(1-plnorm(x, meanlog=hpa.aft1$coef[1], sdlog=hpa.aft1$scale), add=T, lty=1)
curve(1-plnorm(x, meanlog=hpa.aft1$coef[1]+hpa.aft1$coef[2], sdlog=hpa.aft1$scale),
      add=T, lty=2)

# Missing data
# Complete case analysis
mvc <- read.csv("data/mvc.csv")
mvc.miss <- mvc
mvc.miss$age[1:10] <- NA
lm.miss <- lm(MVC~age+height, data=mvc.miss)
summary(lm.miss)

# Mean imputation
mvc.miss$age[1:10] <- NA
mvc.miss$age[1:10] <- mean(mvc.miss$age, na.rm=T)
lm.miss <- lm(MVC~age+height, data=mvc.miss); summary(lm.miss)

# Regression
mvc.miss$age[1:10] <- NA
lm.impute <- lm(age~height, data=mvc.miss)
mvc.miss$age[1:10] <- predict(lm.impute, mvc.miss)[1:10]
lm.miss <- lm(MVC~age+height, data=mvc.miss); summary(lm.miss)

# Multiple imputation
if(!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)}

mvc.miss <- mvc
mvc.miss$age[1:5] <- mvc.miss$height[6:10] <- NA
mvc.impute <- transcan(~MVC + age + height, n.impute=50, shrink=T, 
                       data=mvc.miss, imputed=T)
mvc.impute.areg <- aregImpute(~MVC + age + height, n.impute=50, data=mvc.miss)
mvc.impute.areg$imputed$age









