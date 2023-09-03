# Lecture 5 - Hierarchical linear model

# Intraclass correlation (ICC)
if(!require(ICC)) {install.packages("ICC"); require(ICC)}
n.group <- 10; y <- 1:1000

group <- rep(1:n.group, each=length(y)/n.group)
rand.y <- sample(y, length(y), replace=F)

ICCest(as.factor(group), y) # sorted values
ICCest(as.factor(group), rand.y) # randomly assigned groupings

# Example: Maternity length of stay (slide 16)
mlos <- read.csv("data/mlos.csv")

plot(mlos$age, mlos$los, ylim=c(0, 6)) # no significant association?

lm.los <- lm(los~age, data=mlos)
summary(lm.los)
# age is positively associated with maternity LOS
# without consider potential within-hospital effect

plot(mlos$age, lm.los$residuals) # no obvious pattern on age, centre around 0

wiht(mlos, boxplot(lm.los$residuals~hosp, ylim=c(-2,2), cex=0.5, las=1))
# clustering of residuals by hospials, violated independence assumption

with(mlos, boxplot(los~hosp, ylim=c(0,6), cex=0.5, las=1))
# large variation in maternity LOS across hospitals

if(!require(lattice)) {install.packages("lattice"); require(lattice)}
xyplot(los~age|hosp, data=mlos, type=c('p', 'r')) 
# significant difference between A and D

# Linear mixed model (LMM)
if(!require(lme4)) {install.packages("lme4"); require(lme4)}

hlm.los1 <- lmer(los ~ 1 + (1|hosp), data=mlos)
summary(hlm.los1)
# Estimated overall mean of maternity LOS is 3.2 days
# ICC = 0.2576/(0.2576+0.1659) = 0.61, large variations

with(mlos, boxplot(summary(hlm.los1)$residuals ~ hosp, ylim=c(-3, 3), cex=0.5, las=1))
# no obvious clustering of residuals

# Random intercept model with age
hlm.los2 <- lmer(los ~ age + (1|hosp), data=mlos)
summary(hlm.los2)
# Estimated age effect on maternity LOS is small (<0.01)
# No change in variance: low explanatory power by age

# t-test for fixed effect
if(!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}

hlm.los2.ml <- lmer(los ~ age + (1|hosp), data=mlos, REML=F)
hlm.los20.ml <- lmer(los ~ 1 + (1|hosp), data=mlos, REML=F)
anova(hlm.los20.ml, hlm.los2.ml) # p = 0.17, age is not significant

# LRT for random intercept effect
lm.los20 <- lm(los ~ age, data=mlos)
lrt <- as.numeric(2*abs(logLik(lm.los20) - logLik(hlm.los2.ml)))
pchisq(lrt, df=1, lower.tail=F) # p=0
# reject null hypothesis, select random intercept model

# random intercept and slope model
hlm.los3 <- lmer(los ~ age + (age|hosp), data=mlos, REML=F)
summary(hlm.los3)
# age effect is not significant (p=0.25)

# effect of hospital size
hlm.los4 <- lmer(los ~ age + size + (age|hosp), data=mlos, REML=F)
summary(hlm.los4)
# medium sized hospital associated with shorter materity LOS (p=0.03)

anova(hlm.los4, hlm.los3) # effect of hospital size is not significant (p=0.13)

# Posterior
coef(hlm.los2)

# Model comparison
hlm.los1.ml <- lmer(los ~ 1 + (1|hosp), data=mlos, REML=F)
AIC(hlm.los1.ml, hlm.los2.ml, hlm.los3, hlm.los4)

# Model diagnostics
plot(hlm.los2, resid(.)~fitted(.)) #residuals by  fitted values
plot(hlm.los2, resid(., scaled=T)~age|hosp, abline=0) # no obvious nonlinearity
lattice::qqmath(hlm.los2) ## Q-Q plot, normality assumption looks valid









