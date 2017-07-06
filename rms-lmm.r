
# Luke Zhou. June 2017.
# Consolidated RMS LMM analyser.

rm(list = ls())

library(lattice)
library(plyr)
library(reshape2)
library(lme4)
#library(lmerTest)
library(stargazer)

setwd("/home/luke/Dropbox/LIN1290/Graphing")

source('lib-fileIO.r')
def.globals()


# Baseline data only
base.only.rms = lm(y~Sex,data=rms.data[rms.data$Type=="s.sh.tasks",])
summary(base.only.rms)

residual.scat.plot(base.only.rms)
residual.hist(base.only.rms)
residual.qq.plot(base.only.rms)
dfbeta(base.only.rms)

stargazer(base.only.rms,digit.separator="")



# Baseline vs. shadowing
all.data.rms1 = lm(y~Type,data=rms.data[rms.data$Type %in% c("s.sh.base","s.sh.imit"),])
summary(all.data.rms1)
stargazer(all.data.rms1,digit.separator="")

all.data.rms2 = lm(y~Type+Sex,data=rms.data[rms.data$Type %in% c("s.sh.base","s.sh.imit"),])
summary(all.data.rms2)

all.data.rms3 = lm(y~Cond,data=rms.data[rms.data$Type=="s.sh.tasks" & rms.data$rot.tasks=="N",])
summary(all.data.rms3)
stargazer(all.data.rms3,digit.separator="")

residual.scat.plot(all.data.rms3)
residual.hist(all.data.rms3)
residual.qq.plot(all.data.rms3)
dfbeta(all.data.rms3)

all.data.rms4 = lm(y~Cond+Sex,data=rms.data[rms.data$Type=="s.sh.tasks" & rms.data$rot.tasks=="N",])
summary(all.data.rms4)

residual.scat.plot(all.data.rms4)
residual.hist(all.data.rms4)
residual.qq.plot(all.data.rms4)
dfbeta(all.data.rms4)
