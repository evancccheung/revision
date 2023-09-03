# Practice 6

rtc <- read.csv("data/rtc.csv")
require(geepack)

# (A)
require(ggplot2)
ggplot(rtc, aes(age)) +
  geom_density(fill="gray80") +
  theme_classic() # non-uniform distribution of age

prop.table(table(rtc$male)) # 56% male

with(rtc, interaction.plot(time, id, jitter(rtc), ylab='readiness to change score', 
                           legend=F, lty=1, col=sample(1:20, max(id), replace=T)))
with(rtc, interaction.plot(time, id, jitter(hdd), ylab='heavy drinking days',
                           legend=F, lty=1, col=sample(1:20, max(id), replace=T)))


# (B)
gee.indp <- geeglm(hdd~rtc+age+male, id=id, waves=time, family=poisson, data=rtc, corstr="independence")
gee.exch <- geeglm(hdd~rtc+age+male, id=id, waves=time, family=poisson, data=rtc, corstr="exchangeable")
gee.ar1 <- geeglm(hdd~rtc+age+male, id=id, waves=time, family=poisson, data=rtc, corstr="ar1")
gee.unstr <- geeglm(hdd~rtc+age+male, id=id, waves=time, family=poisson, data=rtc, corstr="unstructured")

require(MESS)
QIC(gee.indp); QIC(gee.exch); QIC(gee.ar1); QIC(gee.unstr)
# select gee.ar1

# (C)
require(doBy)
exp(c(esticon(gee.ar1, c(1,0,0,0))$estimate,
      esticon(gee.ar1, c(1,0,0,0))$lwr,
      esticon(gee.ar1, c(1,0,0,0))$upr)) # intercept: OR=2.13 (95% CI: 1.18, 3.85)
exp(c(esticon(gee.ar1, c(0,1,0,0))$estimate,
      esticon(gee.ar1, c(0,1,0,0))$lwr,
      esticon(gee.ar1, c(0,1,0,0))$upr)) # rtc: OR=0.90 (95% CI: 0.87, 0.94)
exp(c(esticon(gee.ar1, c(0,0,1,0))$estimate,
      esticon(gee.ar1, c(0,0,1,0))$lwr,
      esticon(gee.ar1, c(0,0,1,0))$upr)) # age: OR=1.02 (95% CI: 1.01, 1.03)
exp(c(esticon(gee.ar1, c(0,0,0,1))$estimate,
      esticon(gee.ar1, c(0,0,0,1))$lwr,
      esticon(gee.ar1, c(0,0,0,1))$upr)) # male: OR=1.19 (95% CI: 0.95, 1.50)




