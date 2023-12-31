# Lecture 8 - LASSO Regression

# polynomial regression example (slide 4)
set.seed(6789)
x <- c(0:8)/8*2*pi
y <- 3*sin(x) + rnorm(n=9)
data <- data.frame(x, y)

lm1 <- lm(y ~ x, data=data)
lm3 <- lm(y ~ x + I(x^2) + I(x^3), data=data)
lm6 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data=data)
lm8 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), data=data)

require(ggplot2)
ggplot(data) + aes(x, y) + geom_point(color='red') + 
  stat_smooth(formula=y ~ x, method=lm, se=F, color=3) + 
  geom_smooth(method="loess", lty=3, color=4, se=F) +
  ylab("y1") -> m1
ggplot(data) + aes(x, y) + geom_point(color='red') + 
  stat_smooth(formula=y ~ x + I(x^2) + I(x^3), method=lm, se=F, color=3) + 
  geom_smooth(method="loess", lty=3, color=4, se=F) +
  ylab("y3") -> m3
ggplot(data) + aes(x, y) + geom_point(color='red') + 
  stat_smooth(formula=y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), method=lm, se=F, color=3) + 
  geom_smooth(method="loess", lty=3, color=4, se=F) +
  ylab("y6") -> m6
ggplot(data) + aes(x, y) + geom_point(color='red') + 
  stat_smooth(formula=y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), method=lm, se=F, color=3) + 
  geom_smooth(method="loess", lty=3, color=4, se=F) +
  ylab("y8") -> m8
require(gridExtra)
grid.arrange(m1, m3, m6, m8, nrow=2, ncol=2)

# training data set & testing data: skip

# Prstate cancer
prostate <- read.csv("data/prostate.csv")

# Divide training and testing sets
set.seed(23456)
testingindex <- sample(1:nrow(prostate), 20)
training <- prostate[-testingindex, ]
testing <- prostate[testingindex, ]

# Automated selection procedure
# Backward selection
full <- lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=training)
backward1 <- step(full, direction="backward")

alt <- lm(lpsa ~ . - age - svi, data=training) # ".": all other varaibles, "-": exclude 
backward2 <- step(full, direction="backward", test="F")
backward3 <- step(full, direction="backward", test="Chisq")

# Forward selection
null <- lm(lpsa ~ 1, data=training)
forward <- step(null, scope=list(lower=null, upper=full), direction="forward")

# Stepwise selection
stepwise1 <- step(full, direction="both")
stepwise2 <- step(null, scope=list(lower=null, upper=full), direction="both")



# Ridge regression
if(!require(glmnet)) {install.packages("glmnet"); require(glmnet)}

x <- model.matrix(lpsa ~ . - 1, data=training)
y <- training$lpsa
ridge1 <- glmnet(x, y, alpha=0, lambda=c(0.5, 1))
ridge1

coef(ridge1)

# lambda not specified
ridge2 <- glmnet(x, y, alpha=0)
plot(ridge2, xvar="lambda", label=T, las=1)
plot(ridge2, xvar="dev", label=T, las=1)

# cross-validation to determine lambda
set.seed(56789)
ridge2.cv <- cv.glmnet(x, y, alpha=0)
plot(ridge2.cv) # mean MSE +- SE

min(ridge2.cv$cvm) # 0.61
log(ridge2.cv$lambda.min) # -2.42
log(ridge2.cv$lambda.1se) # -0.37, selected lambda

# cofficients by 1-SE rule
coef(ridge2, s=ridge2.cv$lambda.1se)

# predicted values on tested set
x.test <- model.matrix(lpsa ~ . - 1, data=testing)
ridge2.pred <- predict(ridge2, newx=x.test, s=ridge2.cv$lambda.1se)



# LASSO regression
LASSO1 <- glmnet(x, y, alpha=1, lambda=c(0.25, 0.5, 0.75, 1))
LASSO1
coef(LASSO1)

LASSO2 <- glmnet(x, y, alpha=1)
plot(LASSO2, xvar="lambda", label=T, las=1)
plot(LASSO2, xvar="dev", label=T, las=1)

# cross-validation to determine lambda
set.seed(54321)
LASSO2.cv <- cv.glmnet(x, y, alpha=1)
plot(LASSO2.cv)

# minimum MSE
min(LASSO2.cv$cvm) # 0.62
log(LASSO2.cv$lambda.min) # -3.37

# 1-SE rule
log(LASSO2.cv$lambda.1se) # -1.42

# non-zero cofficients
coef(LASSO2, s=LASSO2.cv$lambda.1se)

# export predicted values for further analysis
LASSO2.pred <- predict(LASSO2, newx=x.test, s=LASSO2.cv$lambda.1se)

# Multicollinearity
prostate.col <- prostate # create new predictor
prostate.col$new1 <- prostate.col$lcavol + prostate.col$lweight # sum two variables

coefficients(lm(lpsa~lcavol+lweight+new1, data=prostate.col)) # OLS
coef(glmnet(x=model.matrix(lpsa ~ lcavol + lweight + new1 - 1, data=prostate.col), 
            y= prostate.col$lpsa, alpha=0, lambda=1)) # Ridge

set.seed(234567)
prostate.col$new2 <- prostate.col$new1 + rnorm(nrow(prostate.col), 0, 0.1) # create another new variable

coefficients(lm(lpsa~lcavol+lweight+new2, data=prostate.col))
coef(glmnet(x=model.matrix(lpsa~lcavol+lweight+new2-1, data=prostate.col), 
            y=prostate.col$lpsa, alpha=0, lambda=1))
# OLS is sensitive to multicollinearity, ridge is more robust

# Number of predictors
set.seed(21098)
smallindex <- sample(1:nrow(prostate), 6)
small <- prostate[smallindex, ]

coefficients(lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=small)) # OLS

x.small <- model.matrix(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45 - 1,
                        data=small)
y.small <- small$lpsa
small.ridge <- glmnet(x.small, y.small, alpha=0, lambda=c(0.1, 0.01)); coef(small.ridge)
small.LASSO <- glmnet(x.small, y.small, alpha=1, lambda=c(0.1, 0.01)); coef(small.LASSO)

# Elastic net (0 < alpha < 1)
elastic000 <- glmnet(x, y, alpha=0)
elastic005 <- glmnet(x, y, alpha=0.05)
elastic010 <- glmnet(x, y, alpha=0.1)
elastic020 <- glmnet(x, y, alpha=0.2)
elastic050 <- glmnet(x, y, alpha=0.5)
elastic080 <- glmnet(x, y, alpha=0.8)
elastic090 <- glmnet(x, y, alpha=0.9)
elastic095 <- glmnet(x, y, alpha=0.95)
elastic100 <- glmnet(x, y, alpha=1)

par(mfrow=c(3, 3))
plot(elastic000, xvar="lambda", label=T, las=1)
plot(elastic005, xvar="lambda", label=T, las=1)
plot(elastic010, xvar="lambda", label=T, las=1)
plot(elastic020, xvar="lambda", label=T, las=1)
plot(elastic050, xvar="lambda", label=T, las=1)
plot(elastic080, xvar="lambda", label=T, las=1)
plot(elastic090, xvar="lambda", label=T, las=1)
plot(elastic095, xvar="lambda", label=T, las=1)
plot(elastic000, xvar="lambda", label=T, las=1)
dev.off()

# cross-validation to determine lambda
set.seed(76543)
elastic.cv <- cv.glmnet(x, y, alpha=0.5)
plot(elastic.cv)

# minimum MSE
min(elastic.cv$cvm) # 0.61
log(elastic.cv$lambda.min) # -2.96

log(elastic.cv$lambda.1se) # 1-SE rule: lambda=-1.10
coef(elastic050, s=elastic.cv$lambda.cv$lambda.1se) # non-zero coefficients
elastic.pred <- predict(elastic050, newx=x.test, s=elastic.cv$lambda.1se)









