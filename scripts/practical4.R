# Practical 4

flu <- read.csv("data/pilot2007.csv")

# (a)
require(survival)
flu.km2.a <- survfit(Surv(time, event, type="right")~1, data=flu)
plot(flu.km2.a, col=4, mark.time=T, conf.int=F)

# (b)
flu$age.gp <- cut(flu$age, breaks=c(0, 6, 15, 81), labels=1:3, include.lowest=F, right=T)

flu.km2.b <- survfit(Surv(time, event, type="right")~age.gp, data=flu)
plot(flu.km2.b, col=c(2:4), mark.time=T, conf.int=F)

# (c)
flu.km2.c <- survfit(Surv(time, event, type="right")~flu.type, data=flu)
plot(flu.km2.c, col=c(2,4), mark.time=T, conf.int=F)
