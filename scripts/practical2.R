# Pratical 2

bmi <- read.csv("data/BMI.csv")

# (a)
bmi$bmi <- bmi$weight/bmi$height^2

require(ggplot2)
heightplot <- ggplot(bmi) + 
  aes(height) + geom_density(fill="red", alpha=0.2) + 
  labs(x="Height (m)", y="Density")
weightplot <- ggplot(bmi) + 
  aes(weight) + geom_density(fill="blue", alpha=0.2) + 
  labs(x="Weight (kg)", y="Density")
bmiplot <- ggplot(bmi) + 
  aes(bmi) + geom_density(fill="purple", alpha=0.2) + 
  labs(x="BMI", y="Density")

require(gridExtra)
grid.arrange(heightplot, weightplot, bmiplot, nrow=3, ncol=1)

# (b)
shapiro.test(bmi$height) # 0.813
shapiro.test(bmi$weight) # 0.534
shapiro.test(bmi$bmi) # 0.0370
# BMI is not noramlly distributed

# (c)
b.bmi <- rep(NA, 1000)
for (i in 1:1000){
  newdata <- bmi[sample(1:100, 100, replace=T), ]
  newdata$bmi <- newdata$weight/newdata$height^2
  b.bmi[i] <- mean(newdata$bmi)
}
mean(b.bmi); quantile(b.bmi, c(0.025, 0.975))
# mean=28.4, 95% cI=(27.7, 29.1)

# (d)
require(boot)
meanbmi.b.out <- function(data, indices){
  newdata <- bmi[indices, ]
  return(mean(newdata$bmi))
}

bmi.b.out <- boot(bmi, statistic=meanbmi.b.out, R=1000)
boot.ci(bmi.b.out)
# 95% BCa CI=(27.7, 29.1)









