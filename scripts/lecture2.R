# Lecture 2

# Area under ROC curve (AUROC) (slide 10)
if(!require(ROCR)) {install.packages("ROCR"); require(ROCR)} # calculate auroc

#setwd("C:/Users/user/Documents/1 - Evan/0 - Courses/MMPH6150 Advanced Statistical Methods II/Project/")
sars <- read.csv("data/SARS08.csv")
sars.glm <- glm(case ~ age+male+fever+sorethroat+cough, data=sars, family=binomial)
sars$pred <- predict(sars.glm, type="response")
pred.obj <- prediction(sars$pred, sars$case)
pref.obj <- performance(pred.obj, measure="auc")
auroc <- pref.obj@y.values[[1]]
auroc # 0.750


# Bootstrapping example (slide 15)
mvc <- read.csv("data/mvc.csv")
# fit a parametric model
lm.mvc <- lm(height ~ age, mvc); summary(lm.mvc)
confint(lm.mvc)[2, ]
# B=-0.195, SE=0.087, 95% CI=(-0.371, -0.019)

# calculate 95% CI by bootstrapping
bootlm <- function(m, original.data){
  beta <- rep(NA, m) # create a null df with length=m
  for (i in 1:m){
    newdata <- original.data[sample(1:41, 41, replace=T), ] # create randomized df from original df
    b.lm <- lm(height ~ age, data=newdata) # perform lm
    beta[i] <- coef(b.lm)[2] # extract coefficient
  }
  return(beta) # outcome: bootstrapped coefficients
}

b.beta <- bootlm(1000, mvc) # 1000 iterations
mean(b.beta); sd(b.beta); quantile(b.beta, c(0.025, 0.975)) 
# bootstrapped B=-0.194, SE=0.087, 95% CI=(-0.356, -0.030)

plot(density(b.beta, bw=0.05), main="Density plot from 1000 bootstrap estimates", 
     xlab="Bootstrap estimate", cex.lab=1.2, las=1) # density plot

# Alternative: require "ggplot2"
ggplot() + 
  aes(x=b.beta) + geom_density(fill="pink", alpha=0.5) + 
  labs(x="Bootstrap estimate", y="Density", title="Density plot from 1000 bootstrap estimates") + 
  theme_classic()


# Residual bootstrap (slide 20)
mvc$residual <- resid(lm.mvc)
mvc$height.pred <- predict(lm.mvc)
resbootlm <- function(m, original.data){
  beta <- rep(NA, m)
  for (i in 1:m){
    b.resid <- original.data$residual[sample(1:41, 41, replace=T)] # resampling residuals
    newdata <- original.data
    newdata$height.b <- newdata$height.pred + b.resid # recreate response
    b.lm <- lm(height.b ~ age, data=newdata) # fit model with recreated response
    beta[i] <- coef(b.lm)[2]
  }
  return(beta)
}

resb.beta <- resbootlm(1000, mvc)
mean(resb.beta); sd(resb.beta); quantile(resb.beta, c(0.025, 0.975)) 
# bootstrapped B=-0.196, SE=0.084, 95% CI=(-0.357, -0.037)

plot(density(resb.beta, bw=0.05), main="Density plot from 1000 bootstrap estimates", 
     xlab="Bootstrap estimate", cex.lab=1.2, las=1) # density plot

# Alternative: require "ggplot2"
ggplot() + 
  aes(x=resb.beta) + geom_density(fill="turquoise", alpha=0.5) + 
  labs(x="Bootstrap estimate", y="Density", title="Density plot from 1000 bootstrap estimates") + 
  theme_classic()


# Parametric bootstrap (slide 23)
parabootlm <- function(m, original.data){
  beta <- rep(NA, m)
  for (i in 1:m){
    newdata <- original.data
    newdata$height.b <- simulate(lm.mvc)$sim_1 # resampling, with replacement, obs from model
    b.lm <- lm(height.b ~ age, data=newdata)
    beta[i] <- coef(b.lm)[2]
  }
  return(beta)
}

parab.beta <- parabootlm(1000, mvc)
mean(parab.beta); sd(parab.beta); quantile(parab.beta, c(0.025, 0.975))
# bootstrapped B=-0.194, SE=0.082, 95% CI=(-0.353, -0.038)

plot(density(parab.beta, bw=0.05), main="Density plot from 1000 bootstrap estimates", 
     xlab="Bootstrap estimate", cex.lab=1.2, las=1) # density plot

# Alternative: require "ggplot2"
ggplot() + 
  aes(x=parab.beta) + geom_density(fill="violet", alpha=0.5) + 
  labs(x="Bootstrap estimate", y="Density", title="Density plot from 1000 bootstrap estimates") + 
  theme_classic()


# Confidence interval for AUROC (slide 27)
b.auroc <- rep(NA, 1000)
for (i in 1:1000){
  sars.new <- sars[sample(1:800, 800, replace=T), ]
  b.pred.obj <- prediction(sars.new$pred, sars.new$case)
  b.perf.obj <- performance(b.pred.obj, measure="auc")
  b.auroc[i] <- b.perf.obj@y.values[[1]]
}
mean(b.auroc); sd(b.auroc); quantile(b.auroc, c(0.025, 0.975))
# bootstrapped AUROC=0.750, SE=0.019, 95% CI=(0.713, 0.786)

plot(density(b.auroc), las=1, xlab="AUROC", main="Bootstrap estimates of AUROC")
#Alternative: require "ggplot2"
ggplot() + 
  aes(x=b.auroc) + geom_density(fill="pink", alpha=0.5) + 
  labs(x="AUROC", y="Density", title="Bootstrap estiamtes of AUROC") +
  theme_classic()


# Bootstrap package in R (slide 29)
if(!require(boot)) {install.packages("boot"); require(boot)} # generate bootstrap samples and construct boostrap CI

# Boot function: MVC example (slide 30)
mvclm.out <- function(data, indices){
  newdata <- data[indices, ]
  b.lm <- lm(height ~ age, data=newdata)
  return(coef(b.lm)["age"])
}

mvc.b.out <- boot(data=mvc, statistic=mvclm.out, R=1000)
boot.ci(mvc.b.out) # usually take normal
# bootstrapped 95% CI=(-0.358, -0.036)

# Boot function: AUROC example (slide 31)
auroc.b.out <- function(data, indices){
  sars.new <- sars[indices, ]
  sars.glm <- glm(case ~ age+male+fever+sorethroat+cough, data=sars.new, family=binomial)
  sars.new$pred <- predict(sars.glm, type="response")
  b.pred.obj <- prediction(sars.new$pred, sars.new$case)
  b.perf.obj <- performance(b.pred.obj, measure="auc")
  return(b.perf.obj@y.values[[1]])
}

sars.b.out <- boot(data=sars, statistic=auroc.b.out, R=1000)
boot.ci(sars.b.out)
# bootstrapped 95% CI=(0.712, 0.787)









