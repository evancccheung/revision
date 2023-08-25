# CMED6040 Practical 2
# example R script

# Read in data
bmi <- read.csv("./lecture/s2/BMI.csv")
bmi$bmi <- bmi$weight/bmi$height^2

# Question (a)
par(mfrow=c(3,1))
plot(density(bmi$height))
plot(density(bmi$weight))
plot(density(bmi$bmi))


# Question (b)
# Test normality
# (Google Search: Test normality, R) or ??normality
# Shapiro-Wilk Normality Test - shapiro.test 

shapiro.test(bmi$height)
shapiro.test(bmi$weight)
shapiro.test(bmi$bmi)

# p=0.81, 0.53, 0.04 respectively


# Question (c)
b.bmi <- rep(NA,1000)
for (i in 1:1000){
bmi.new <- bmi[sample(1:100, 100, replace=TRUE),]
b.bmi[i] <- mean(bmi.new$weight/bmi.new$height^2)
}

mean(b.bmi)

quantile(b.bmi, c(0.025, 0.975))


# Question (d)
library(boot)

bmi.f <- function(data, indices){
	bmi.new <- bmi[indices,]
	return(mean(bmi.new$bmi))
}

b.bmi.out <- boot(data=bmi, statistic=bmi.f, R=1000)

boot.ci(b.bmi.out)





