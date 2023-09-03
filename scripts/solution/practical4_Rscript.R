library(survival)

flu <- read.csv("YOUR PATH/pilot2007.csv")

plot(survfit(Surv(time, event)~1, data=flu), conf.int=FALSE , mark.time=T)


plot(survfit(Surv(time, event)~cut(age, breaks=c(0,6,16,100)), data=flu), col=1:3, lwd=2, mark.time=T)
legend(0.5, 0.5, c("0-6", "7-15", "16+"), col = 1:3, lwd=2)

plot(survfit(Surv(time, event)~flu.type, conf.int=FALSE, data=flu), col=2:3, lwd=2)
legend(0.5, 0.5, c("Type A", "Type B"), col = 2:3, lwd=2) 
