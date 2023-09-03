# Practical 8

lowbwt <- read.csv("data/lowbwt.csv")

require(glmnet)

# continuous outcome
x.con <- model.matrix(BWT ~ . - 1 - ID - LOW, data=lowbwt)
y.con <- lowbwt$BWT

# binary outcome
x.bin <- model.matrix(LOW ~ . - 1 - ID - BWT, data=lowbwt)
y.bin <- lowbwt$LOW

# continous outcome: ridge regression
ridge1 <- glmnet(x.con, y.con, alpha=0)
par(mfrow=c(1,2))
plot(ridge1, xvar="lambda", label=T, las=1)
plot(ridge1, xvar="dev", label=T, las=1)
par(mfrow=c(1,1))

set.seed(56789)
ridge1_cv <- cv.glmnet(x.con, y.con, alpha=0)
plot(ridge1_cv)
coef(ridge1, s=ridge1_cv$lambda.1se)

# continous outcome: LASSO regression
LASSO1 <- glmnet(x.con, y.con, alpha=1)
par(mfrow=c(1,2))
plot(LASSO1, xvar="lambda", label=T, las=1)
plot(LASSO1, xvar="dev", label=T, las=1)
par(mfrow=c(1,1))

set.seed(65432)
LASSO1_cv <- cv.glmnet(x.con, y.con, alpha=1)
plot(LASSO1_cv)
coef(LASSO1, s=LASSO1_cv$lambda.1se)

# binary outcome: ridge regression
ridge2 <- glmnet(x.bin, y.bin, alpha=0, family="binomial")
par(mfrow=c(1,2))
plot(ridge2, xvar="lambda", label=T, las=1)
plot(ridge2, xvar="dev", label=T, las=1)
par(mfrow=c(1,1))

set.seed(56789)
ridge2_cv <- cv.glmnet(x.bin, y.bin, alpha=0, family="binomial")
plot(ridge2_cv)
coef(ridge2, s=ridge2_cv$lambda.1se)

# binary outcome: LASSO regression
LASSO2 <- glmnet(x.bin, y.bin, alpha=1, family="binomial")
par(mfrow=c(1,2))
plot(LASSO2, xvar="lambda", label=T, las=1)
plot(LASSO2, xvar="dev", label=T, las=1)
par(mfrow=c(1,1))

set.seed(65432)
LASSO2_cv <- cv.glmnet(x.bin, y.bin, alpha=1, family="binomial")
plot(LASSO2_cv)
coef(LASSO2, s=LASSO2_cv$lambda.1se)
