rm(list=ls())
setwd("~/SPH Dropbox/Yun Lin/Shared ASM2/2022-23/tutorial/t2/")

# read in dataset
flu <- read.csv("fluvaccine.csv")

# (a) complete case analysis
lr0 <- glm(flu~vac+shealth+vac*smoking, data=flu, family=binomial)

cbind(exp(coef(lr0)),exp(confint(lr0)))

# (b) dataset with missing data (MCAR)
flu.m1 <- read.csv('fluvaccine_m1.csv')

summary(flu.m1) # missing data in shealth, smoking, vac and flu
# Visualize missing pattern
require(VIM)
aggr_plot <- aggr(flu.m1, sortVars=T, numbers=T)
#colnames(flu.m1)
marginplot(flu.m1[,c(4,5)]) # shealth and smoking 
quantile(flu.m1$shealth[is.na(flu.m1$smoking)],na.rm=T)
quantile(flu.m1$shealth[!is.na(flu.m1$smoking)],na.rm=T)
marginplot(flu.m1[,c(4,7)]) # shealth and vac

flu.m1$complete <- complete.cases(flu.m1)
head(flu.m1)
prop.table(table(flu.m1$complete)) # 66% of the records are complete cases
boxplot(shealth~complete, data=flu.m1)
with(flu.m1, prop.table(table(vac, complete),2))
#fisher.test(with(flu.m1, table(vac, complete)))
with(flu.m1, prop.table(table(smoking, complete),2))
#fisher.test(with(flu.m1, table(smoking, complete)))

# (c) Complete case analysis
lr0.m1 <- glm(flu~vac+shealth+vac*smoking, data=flu.m1, family=binomial)
summary(lr0.m1)

cbind(exp(coef(lr0.m1)),exp(confint(lr0.m1)))

# (d) Construct imputed dataset using variables in the final analysis model
require(Hmisc)
set.seed(111)
flu.m1.impute <- transcan(~flu+vac+shealth+smoking, n.impute=50,
                          shrink=T, data=flu.m1, imputed=T)

flu.m1.impute$imputed$shealth[,1]
flu.m1.impute$imputed$vac[,1]

# switch to aregImpute
set.seed(11)
flu.m1.impute <- aregImpute(~flu+vac+shealth+smoking, n.impute=50, data=flu.m1)

flu.m1.impute$imputed$shealth[,1]
flu.m1.impute$imputed$vac[,1]

# Visualize imputed data
# shealth (numeric)
boxplot(flu.m1$shealth, border='blue', at=1, xlim=c(0,3))
boxplot(flu.m1.impute$imputed$shealth[,1], border='red', at=2, add=T)

boxplot(flu.m1$shealth, border='blue', at=1, xlim=c(0,53))
for (i in 1:50) boxplot(flu.m1.impute$imputed$shealth[,i], border='red', at=i+1, add=T)

# vac (categorical)
prop.table(table(flu.m1$vac))
prop.table(table(flu.m1.impute$imputed$vac[,1]))

for(i in 1:50) print(prop.table(table(flu.m1.impute$imputed$vac[,i])))


# (e) Fit regression model on imputed datasets
lr0.m1.impute <- fit.mult.impute(flu ~vac+shealth+vac*smoking, glm,
                                 flu.m1.impute, data=flu.m1, family=binomial)
summary(lr0.m1.impute)

cbind(exp(coef(lr0.m1.impute)),exp(confint(lr0.m1.impute)))


# (f) dataset with missing data (not MCAR)
flu.m2 <- read.csv('fluvaccine_m2.csv')

aggr_plot <- aggr(flu.m2, sortVars=T, numbers=T)
#head(flu.m2)
marginplot(flu.m2[,c(4,5)])
marginplot(flu.m2[,c(4,6)])
marginplot(flu.m2[,c(4,7)])

# (g) complete case analysis
lr0.m2 <- glm(flu~vac+shealth+vac*smoking, data=flu.m2, family=binomial)
summary(lr0.m2)

exp(coef(lr0.m2))
exp(confint(lr0.m2))


# (h) multiple imputation using variables in the final model
set.seed(1)
flu.m2.impute <- aregImpute(~flu+vac+shealth+smoking, n.impute=50, data=flu.m2)

flu.m2.impute$imputed$vac

# Visualize imputed data
# shealth
boxplot(flu.m2$shealth, border='blue', at=1, xlim=c(0,3))
boxplot(flu.m2.impute$imputed$shealth[,1], border='red', at=2, add=T)

boxplot(flu.m2$shealth, border='blue', at=1, xlim=c(0,53))
for (i in 1:50) boxplot(flu.m2.impute$imputed$shealth[,i], border='red', at=i+1, add=T)

# vac
prop.table(table(flu$vac))
prop.table(table(flu.m2$vac))
prop.table(table(flu.m2.impute$imputed$vac[,1]))
for(i in 1:50) print(prop.table(table(flu.m2.impute$imputed$vac[,i])))

# comparing the distribution of vaccination for different shealth score, original vs imputed dataset
prop.table(table(flu.m2$shealth, flu.m2$vac),1)
vac.miss.m2 <- which(is.na(flu.m2$vac))
shealth.miss.m2 <- which(is.na(flu.m2$shealth))
imputed.shealth.m2.i1 <- flu.m2$shealth
imputed.shealth.m2.i1[shealth.miss.m2] <- flu.m2.impute$imputed$shealth[,1] ## all shealth score, original and imputed
prop.table(table(imputed.shealth.m2.i1[vac.miss.m2], flu.m2.impute$imputed$vac[,1]),1)

lr0.m2.impute <- fit.mult.impute(flu~vac+shealth+vac*smoking, glm,
                                 flu.m2.impute, data=flu.m2, family=binomial)
summary(lr0.m2.impute)

exp(coef(lr0.m2.impute))
exp(confint(lr0.m2.impute))

# (i) using all variables for imputation
set.seed(704)
flu.m2.impute2 <- aregImpute(~flu+vac+shealth+smoking+age+male+bmi+abT, 
                             n.impute=50, data=flu.m2)

flu.m2.impute2$imputed$vac

lr0.m2.impute2 <- fit.mult.impute(flu ~vac+shealth+vac*smoking, glm,
                                  flu.m2.impute2, data=flu.m2, family=binomial)
summary(lr0.m2.impute2)

exp(coef(lr0.m2.impute2))
exp(confint(lr0.m2.impute2))

# (j) 
flu.m3 <- read.csv('fluvaccine_m3.csv')
aggr_plot <- aggr(flu.m3, sortVars=T, numbers=T)
marginplot(flu.m3[,c(4,7)])

# (k)
# complete case analysis
lr0.m3 <- glm(flu~vac+shealth+vac*smoking, data=flu.m3, family=binomial)
summary(lr0.m3)

exp(coef(lr0.m3))
exp(confint(lr0.m3))

# multiple imputation using all variables
set.seed(2)
flu.m3.impute <- aregImpute(~flu+vac+shealth+smoking+age+male+bmi+abT, n.impute=50, data=flu.m3)

lr0.m3.impute <- fit.mult.impute(flu ~vac+shealth+vac*smoking, glm,
                                 flu.m3.impute, data=flu.m3, family=binomial)
summary(lr0.m3.impute)

exp(coef(lr0.m3.impute))
exp(confint(lr0.m3.impute))

