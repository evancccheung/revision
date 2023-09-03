#setwd("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared ASM2/2020-21/tutorial/t3")
#setwd("/Users/vanialam/SPH Dropbox/Yun Lin/Shared ASM2/2022-23/tutorial/t3")
# (a) read in data and draw spaghetti plot
dat <- read.csv('YOUR PATH/ambc.csv')
require(lattice)
with(dat,interaction.plot(time,id,pm, legend=F, lty=1, col=gray(0.7), 
                          xlab='follow up', ylab='Mean PM2.5', las=1))
with(dat,interaction.plot(time,id,ambc, legend=F, lty=1, col=gray(0.7),
                          xlab='follow up', ylab='AMBC', las=1))


require(ICC)

with(dat, ICCest(as.factor(id), ambc))

# (b)
xyplot(ambc~pm|id, data=dat, type=c('p','r'))

xyplot(ambc~pm|id, data=dat[dat$id < 10,], type=c('p','r'))


# (c) crude linear regression analysis
l0 <- lm(ambc~male+age+pm, data=dat)
summary(l0)
cbind(coef(l0),confint(l0))


# (d) fitting GEE
library(geepack)
g.indp <- geeglm(ambc~male+age+pm, family=gaussian, id=id, corstr="independence", data=dat)
g.exch <- geeglm(ambc~male+age+pm, family=gaussian, id=id, corstr="exchangeable", data=dat)
g.ar1 <- geeglm(ambc~male+age+pm, family=gaussian, id=id, corstr="ar1", data=dat)
g.unstr <- geeglm(ambc~male+age+pm, family=gaussian, id=id, corstr="unstructured", data=dat)


# (e) comparing models
library(MESS)
QIC(g.indp); QIC(g.exch); QIC(g.ar1); QIC(g.unstr)

# (f)
summary(g.ar1)

library(doBy)
## beta2: male
esticon(g.ar1, c(0,1,0,0))
c(esticon(g.ar1,  c(0,1,0,0))$lwr,esticon(g.ar1,  c(0,1,0,0))$upr) 

## beta3: age
esticon(g.ar1, c(0,0,1,0))
c(esticon(g.ar1, c(0,0,1,0))$lwr,esticon(g.ar1, c(0,0,1,0))$upr)
## beta4: pm
esticon(g.ar1, c(0,0,0,1))
c(esticon(g.ar1, c(0,0,0,1))$lwr,esticon(g.ar1, c(0,0,0,1))$upr)



# (g) fitting linear mixed model (random intercept)
require(lme4)

lme1 <- lmer(ambc~male+age+pm+(1|id), data=dat)
summary(lme1)
confint(lme1, method="Wald")
cbind(lme1@beta[-1],confint(lme1, method="Wald")[-c(1:3),])


# (h) 
0.0452/(0.0452+0.1454)

# (i) fitting linear mixed model (random intercept and slope)
lme2 <- lmer(ambc~male+age+pm+(1+pm|id), data=dat)
summary(lme2)
confint(lme2, method="Wald")
cbind(lme2@beta[-1],confint(lme2, method="Wald")[-c(1:5),])

# (k) convert data to wide form 
data.wide <- reshape(dat, v.names=c("pm","ambc"), 
                     idvar="id", timevar="time", direction="wide")
head(data.wide)

l.multi <- lm(ambc.5~male+age+pm.1+pm.2+pm.3+pm.4+pm.5, data=data.wide)
summary(l.multi)
confint(l.multi)
cbind(coef(l.multi),confint(l.multi))

require(car)
vif(l.multi)

# (l) LASSO regression
x <- model.matrix(ambc.5~-1+male+age+pm.1+pm.2+pm.3+pm.4+pm.5, data=data.wide)
y <- data.wide$ambc.5

require(glmnet)
lasso.cv <- cv.glmnet(x,y,alpha=1)
plot(lasso.cv)
lambda.cv <- lasso.cv$lambda.min

lasso.opt <- glmnet(x, y, alpha=1, lambda=lambda.cv)
coef(lasso.opt)

lambda.1se <- lasso.cv$lambda.1se
lasso.1se <- glmnet(x, y, alpha=1, lambda=lambda.1se)
coef(lasso.1se)
