# Tutorial 3

# (a)
ambc <- read.csv("data/ambc.csv")

with(ambc, interaction.plot(time, id, jitter(pm), legend=F, ylab="mean indoor PM2.5 conc.", las=1, lty=1,
                            col=sample(1:20, max(id), replace=T)))
with(ambc, interaction.plot(time, id, jitter(ambc), legend=F, ylab="mean ambc", las=1, lty=1,
                            col=sample(1:20, max(id), replace=T)))
# AMBC are correlated within subjects

require(ICC)
with(ambc, ICCest(factor(id), ambc)) # ICC=0.45, moderate correlation

# (b)
with(ambc, plot(ambc~pm, cex=0.5, pch=16))

# (c)
lm <- lm(ambc ~ male + age + pm, data=ambc)
summary(lm)
round(cbind(coef(lm), confint(lm)), 3)
# male: B=0.091 (95% CI: 0.032, 0.150)
# age: B=0.020 (95% CI: 0.011, 0.029)
# pm: B=0.029 (95% CI: 0.026, 0.032)

# (d)
require(geepack)
gee.indp <- geeglm(ambc ~ male + age + pm, id=id, waves=time, data= ambc, corstr="independence")
gee.exch <- geeglm(ambc ~ male + age + pm, id=id, waves=time, data=ambc, corstr="exchangeable")
gee.ar1 <- geeglm(ambc ~ male + age+ pm, id=id, waves=time, data=ambc, corstr="ar1")
gee.unstr <- geeglm(ambc ~ male + age + pm, id=id, waves=time, data=ambc, corstr="unstructured")

# (e)
require(MESS)
QIC(gee.indp); QIC(gee.exch); QIC(gee.ar1); QIC(gee.unstr)
# gee.ar1 is selected

# (f)
require(doBy)
c(esticon(gee.ar1, c(0,1,0,0))$estimate,
      esticon(gee.ar1, c(0,1,0,0))$lwr,
      esticon(gee.ar1, c(0,1,0,0))$upr) # male: OR=0.096 (95% CI: 0.019, 0.172)
c(esticon(gee.ar1, c(0,0,1,0))$estimate,
      esticon(gee.ar1, c(0,0,1,0))$lwr,
      esticon(gee.ar1, c(0,0,1,0))$upr) # age: OR=0.020 (95% CI: 0.008, 0.033)
c(esticon(gee.ar1, c(0,0,0,1))$estimate,
      esticon(gee.ar1, c(0,0,0,1))$lwr,
      esticon(gee.ar1, c(0,0,0,1))$upr) # pm: OR=0.028 (95% CI: 0.024, 0.032)

# (g)
require(lme4)
hlm1 <- lmer(ambc ~ male + age + pm + (1|id), data=ambc)
summary(hlm1)
confint(hlm1)
# male: B=0.094 (95% CI: 0.012, 0.176)
# age: B=0.020 (95% cI: 0.007, 0.032)
# pm: B=0.028 (95% CI: 0.024, 0.032)

# (h)
0.04523/(0.04523+0.14544) # ICC=0.237

# (i)
hlm2 <- lmer(ambc ~ male + age + pm + (1+pm|id), data=ambc)
summary(hlm2)
confint(hlm2)
# male: B=0.092 (95% CI: 0.010, 0.175)
# age: B=0.021 (95% cI: 0.008, 0.034)
# pm: B=0.027 (95% CI: 0.021, 0.032)

# (k)
ambc.wide <- reshape(ambc, v.names=c("pm", "ambc"), timevar="time", idvar="id", direction="wide")

lm.wide <- lm(ambc.5 ~ male + age + pm.1 + pm.2 + pm.3 + pm.4 + pm.5, data=ambc.wide)
round(cbind(coef(lm.wide), confint(lm.wide)), 3)
# male: B=0.131 (95% CI: -0.001, 0.263)
# age: B=0.034 (95% CI: 0.014, 0.054)
# pm.1: B=0.004 (95% CI: -0.026, 0.035)
# pm.2: B=0.019 (95% CI: -0.034, 0.053)
# pm.3: B=0.025 (95% CI: -0.018, 0.069)
# pm.4: B=-0.023 (95% CI: -0.069, 0.023)
# pm.5: B=0.034 (95% CI: 0.003, 0.064)

# (l)
LASSO_x <- model.matrix(ambc.5 ~ male + age + pm.1 + pm.2 + pm.3 + pm.4 + pm.5 - 1, data=ambc.wide)
LASSO_y <- ambc.wide$ambc.5

require(glmnet)
LASSO_cv <- cv.glmnet(LASSO_x, LASSO_y, alpha=1)
plot(LASSO_cv)

LASSO_opt <- glmnet(LASSO_x, LASSO_y, alpha=1, lambda=LASSO_cv$lambda.min)
coef(LASSO_opt)

LASSO_1se <- glmnet(LASSO_x, LASSO_y, alpha=1, lambda=LASSO_cv$lambda.1se)
coef(LASSO_1se)
