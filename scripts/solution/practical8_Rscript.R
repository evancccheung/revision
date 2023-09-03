##################
#### Slide 38 ####
##################

#### Exercise - Low Birth Weight dataset ####
LowBWT <- read.csv("~/SPH Dropbox/Yun Lin/Shared ASM2/2021-22/lecture/s8/lowbwt.csv")
LowBWT$LOW <- factor(LowBWT$LOW)
LowBWT$RACE <- factor(LowBWT$RACE)
LowBWT$SMOKE <- factor(LowBWT$SMOKE)
LowBWT$HT <- factor(LowBWT$HT)
LowBWT$UI <- factor(LowBWT$UI)

library(glmnet)

# Fitting continuous outcome, variable "BWT"
x.con = model.matrix(BWT ~ . - 1 - ID - LOW, data = LowBWT)
y.con = LowBWT$BWT

# Fitting binary outcome, variable "LOW"
x.bin = model.matrix(LOW ~ . - 1 - ID - BWT, data = LowBWT)
y.bin = LowBWT$LOW


# Ridge Regression
Ridge1 <- glmnet(x.con, y.con, alpha = 0)
#Ridge1
plot(Ridge1, xvar="lambda", label=TRUE)
plot(Ridge1, xvar="dev", label=TRUE)

set.seed(56789)
Ridge1.cv = cv.glmnet(x.con, y.con, alpha = 0)
plot(Ridge1.cv)
coef(Ridge1, s = Ridge1.cv$lambda.1se)

# LASSO
LASSO1 <- glmnet(x.con, y.con, alpha = 1)
plot(LASSO1, xvar="lambda", label=TRUE)
plot(LASSO1, xvar="dev", label=TRUE)

set.seed(65432)
LASSO1.cv = cv.glmnet(x.con, y.con, alpha = 1)
plot(LASSO1.cv)
coef(LASSO1, s = LASSO1.cv$lambda.1se)


# Ridge Regression
Ridge2 <- glmnet(x.bin, y.bin, alpha = 0, family = "binomial")
plot(Ridge2, xvar="lambda", label=TRUE)
plot(Ridge2, xvar="dev", label=TRUE)

set.seed(56789)
Ridge2.cv = cv.glmnet(x.bin, y.bin, alpha = 0, family = "binomial")
plot(Ridge2.cv)
coef(Ridge2, s = Ridge2.cv$lambda.1se)

# LASSO Regression
LASSO2 <- glmnet(x.bin, y.bin, alpha = 1, family = "binomial")
plot(LASSO2, xvar="lambda", label=TRUE)
plot(LASSO2, xvar="dev", label=TRUE)

set.seed(65432)
LASSO2.cv = cv.glmnet(x.bin, y.bin, alpha = 1, family = "binomial")
plot(LASSO2.cv)
coef(LASSO2, s = LASSO2.cv$lambda.1se)

