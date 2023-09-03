#
#rm(list=ls())
setwd("~/SPH Dropbox/Yun Lin/Shared ASM2/2022-23/lecture/s6/")
#
require(geepack)
require(MESS)
require(doBy)

data1 <- read.csv("rtc.csv")

#
### A)
# sample size
length(unique(data1$id)) # N=100
data1.flat <- data1[!duplicated(data1$id),]
# age
summary(data1.flat$age)
hist(data1.flat$age,las=1,xlab="Age",ylab="Number of cases",main=NA)
# sex
table(data1.flat$male)
prop.table(table(data1.flat$male))
# RTC and HDD (spagetti plot)
set.seed(1)
with(data1,interaction.plot(time,id,jitter(rtc),ylab='Readiness to change', legend=F, 
                            lty=1, col=sample(1:20, max(id), replace=T),las=1))
with(data1,interaction.plot(time,id,jitter(hdd),ylab='Heavy drinking days', legend=F, 
                            lty=1, col=alpha("gray",.6),las=1))


### B)
gee.indp <- geeglm(hdd~age+male+rtc,data=data1,id=id,
                   family=poisson,corstr = "independence")
gee.exch <- geeglm(hdd~age+male+rtc,data=data1,id=id,
                   family=poisson,corstr = "exchangeable")
gee.ar1 <- geeglm(hdd~age+male+rtc,data=data1,id=id,
                  family=poisson,corstr = "ar1")
gee.unstr <- geeglm(hdd~age+male+rtc,data=data1,id=id,
                    family=poisson,corstr = "unstructured")
QIC(gee.indp); QIC(gee.exch); QIC(gee.ar1); QIC(gee.unstr)


### C)
summary(gee.indp)
#
exp(c(esticon(gee.indp, c(0,1,0,0))$estimate,esticon(gee.indp, c(0,1,0,0))$lwr,
      esticon(gee.indp, c(0,1,0,0))$upr)) # age*
exp(c(esticon(gee.indp, c(0,0,1,0))$estimate,esticon(gee.indp, c(0,0,1,0))$lwr,
      esticon(gee.indp, c(0,0,1,0))$upr)) # male
exp(c(esticon(gee.indp, c(0,0,0,1))$estimate,esticon(gee.indp, c(0,0,0,1))$lwr,
      esticon(gee.indp, c(0,0,0,1))$upr)) # rtc*


####