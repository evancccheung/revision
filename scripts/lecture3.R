# Lecture 3 - Regression Discontinuity Design

# Alcohol-related suicide (slide 12)
alc <- read.csv("data/alcohol.csv")
alc$MLDA <- 1 * (alc$month>=0)
rd1 <- lm(alc.suicide ~ month*MLDA, data=alc)
summary(rd1)
cbind(coef(rd1), confint(rd1))
# MLDA: 0.14 (95% CI: 0.04, 0.24)

with(alc, plot(alc.suicide~month, xlim=c(-30,30), ylim=c(0,1), pch=16, las=1,
               xlab="Age in months from MLDA", ylab="alcohol-related suicides per 1000 hospital episodes"))
abline(v=0, col="grey70", lwd=2)
with(alc, segments(alc$month[1], fitted(rd1)[1], alc$month[36], fitted(rd1)[36], col="grey70", lwd=2))
with(alc, segments(alc$month[37], fitted(rd1)[37], alc$month[73], fitted(rd1)[73], col="grey70", lwd=2))

# Revised model (slide 16)
rd1b <- lm(alc.suicide ~ month + MLDA, data=alc)
summary(rd1b)
cbind(coef(rd1b), confint(rd1b))
# MLDA: 0.14 (95% CI: 0.04, 0.24)

with(alc, plot(alc.suicide~month, xlim=c(-30,30), ylim=c(0,1), pch=16, las=1,
               xlab="Age in months from MLDA", ylab="alcohol-related suicides per 1000 hospital episodes"))
abline(v=0, col="grey70", lwd=2)
with(alc, segments(alc$month[1], fitted(rd1b)[1], alc$month[36], fitted(rd1b)[36], col="grey70", lwd=2))
with(alc, segments(alc$month[37], fitted(rd1b)[37], alc$month[73], fitted(rd1b)[73], col="grey70", lwd=2))

# Test potential polynomail relation (slide 19)
rd1c <- lm(alc.suicide ~ MLDA + poly(month, degree=3), data=alc)
summary(rd1c) #estimate of MLDA changed slight from 0.14 to 0.12


# Local polynomial regression - BMI and age (slide 28)
if(!require(np)) {install.packages("np"); require(np)}
data <- read.csv("data/digoxin.csv")
plot(data$age, data$bmi, type="p", xlab="Age", ylab="BMI", las=1)

bmi.bw <- npregbw(bmi~age, bandwidth.compute=T, ckertype="gaussian", regtype="ll", data=data)
bmi.lp <- npreg(bws = bmi.bw)

with(data, plot(age[order(age)], predict(bmi.lp)[order(age)], type="l", las=1,
                xlab="Age", ylab="BMI", xlim=c(30, 90), ylim=c(15, 45)))
with(data, points(age, bmi, pch=19, col="red"))

# Different smoothness
bmi.bw2 <- npregbw(bmi~age, bws=c(5), bandwidth.compute=F, ckertype="gaussian", regtype="ll", data=data)
bmi.lp2 <- npreg(bws = bmi.bw2)
bmi.bw3 <- npregbw(bmi~age, bws=c(10), bandwidth.compute=F, ckertype="gaussian", regtype="ll", data=data)
bmi.lp3 <- npreg(bws = bmi.bw3)

with(data, lines(age[order(age)], predict(bmi.lp2)[order(age)], col="blue"))
with(data, lines(age[order(age)], predict(bmi.lp3)[order(age)], col="red"))
legend(75, 20, c("bw=5", "bw=10", "optimal bw"), col=c("blue", "red", "black"), lwd=1, box.lty=0, bg=alpha("blue", 0))


# RDD with local regresion
if(!require(rdrobust)) {install.packages("rdrobust"); require(rdrobust)}
rd.l <- rdrobust(alc$alc.suicide, alc$month, all=T)
summary(rd.l)
# Estimate: 0.08 (95% CI: -0.30, 0.46) -> robust(bandwidth selection, wide 95% CI)

rd.g <- rdrobust(alc$alc.suicide, alc$month, h=100, kernel="uniform", all=T)
summary(rd.g)
# Estimate: 0.14 (95% CI: 0.02, 0.27) -> conventional(pre-defined bandwidth, bandwidth selection)









