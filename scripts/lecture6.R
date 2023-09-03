# Lecture 6 - Longitudinal analysis

# 'Spaghetti plot'
if(!require(geepack)) {install.packages("geepack"); require(geepack)}
data(ohio)

with(ohio, interaction.plot(age, id, jitter(resp), ylab='wheeze', legend=F, lty=1, col=gray(0.7)))
with(ohio, interaction.plot(age, id, jitter(resp), ylab='wheeze', legend=F, lty=1, col=sample(1:20, max(id), replace=T)))

# (naive) GLM model
glm.ohio <- glm(resp~age+smoke, family=binomial, data=ohio)
summary(glm.ohio)
# both age (p=0.04) and smoke (p=0.03) are significant

cor(glm.ohio$residuals[ohio$age==-2], glm.ohio$residuals[ohio$age==-1]) # 0.35
cor(glm.ohio$residuals[ohio$age==-2], glm.ohio$residuals[ohio$age==0]) # 0.30
cor(glm.ohio$residuals[ohio$age==-2], glm.ohio$residuals[ohio$age==1]) # 0.32
cor(glm.ohio$residuals[ohio$age==-1], glm.ohio$residuals[ohio$age==0]) # 0.44
cor(glm.ohio$residuals[ohio$age==-1], glm.ohio$residuals[ohio$age==1]) # 0.33
cor(glm.ohio$residuals[ohio$age==0], glm.ohio$residuals[ohio$age==1]) # 0.38

# GEE model
gee.indp <- geeglm(resp~age+smoke, family=binomial, data=ohio, id=id, corstr="independence")
summary(gee.indp)
# same estimate as from GLM, different standard errors

if(!require(doBy)) {install.packages("doBy"); require(doBy)} # Wald statistics
esticon(gee.indp, c(0,0,1)) # specify: smoke
exp(c(esticon(gee.indp, c(0,0,1))$estimate,
      esticon(gee.indp, c(0,0,1))$lwr,
      esticon(gee.indp, c(0,0,1))$upr)) # Smoke: OR=1.31 (95% CI: 0.93, 1.86)
exp(c(esticon(gee.indp, c(0,1,0))$estimate,
      esticon(gee.indp, c(0,1,0))$lwr,
      esticon(gee.indp, c(0,1,0))$upr)) # Age: OR=0.89 (95% CI: 0.82, 0.97)

# Model comparison
gee.indp0 <- geeglm(resp~age, id=id, data=ohio, family=binomial, corstr="independence")
anova(gee.indp, gee.indp0) # p=0.13, maternalsmoking is not significant

# GEE with other correlation structure
gee.exch <- geeglm(resp~age+smoke, family=binomial, data=ohio, id=id, corstr="exchangeable")
summary(gee.exch)
gee.ar1 <- geeglm(resp~age+smoke, family=binomial, data=ohio, id=id, corstr="ar1")
summary(gee.ar1)
gee.unstr <- geeglm(resp~age+smoke, family=binomial, data=ohio, id=id, corstr="unstructured")
summary(gee.unstr)

# Choosing correlation structure
if(!require(MESS)) {install.packages("MESS"); require(MESS)}
QIC(gee.indp); QIC(gee.exch); QIC(gee.ar1); QIC(gee.unstr)
# all four models are similar

# Interaction between maternal smoking and age
gee.int.ar1 <- geeglm(resp~age*smoke, family=binomial, data=ohio, id=id, corstr="ar1")
summary(gee.int.ar1)
# interaction term not significant: no significant difference in effect across groups

# Missing data
set.seed(111)
n <- nrow(ohio)
n.missing <- 100
missing.x <- sample(1:n, n.missing, replace=F)
missing.y <- sample(c(3,4), n.missing, replace=T)
ohio.miss <- ohio

ohio.miss$waves <- ohio.miss$age + 3
ohio.miss[cbind(missing.x, missing.y)] <- NA

gee.ar1.miss <- geeglm(resp~age+smoke, family=binomial, waves=waves, data=na.omit(ohio.miss), id=id, corstr="ar1")
summary(gee.ar1.miss)


# Longitudinal data analysis using GLMM
require(lme4)
ohio.glmm <- glmer(resp~age+smoke+(1|id), family=binomial, data=ohio)
summary(ohio.glmm)
# result similar to GEE
# GEE vs GLMM: GEE on population average, GLMM on subject-specific interpretation









