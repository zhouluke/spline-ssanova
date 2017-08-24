
# Luke Zhou. June 2017.
# COG LMERs w.r.t. Cond ass't and IAT score

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

############################################################
# LINEAR MIXED FX MODELS
############################################################

do.my.model <- function(data,formula,REML=TRUE){
  
  model = lmer(data=data,formula,REML=FALSE)
  
  #res = plot(fitted(model),residuals(model))
  #hist.res = hist(residuals(model))
  #print(hist.res)
  return(model)
}

compare.my.models <- function(models) {
  combn(models,2, function(combo) { anova(combo[[1]],combo[[2]]) } , simplify=FALSE) 
}

compare <- function(data,models) {
  compare.my.models(lapply(models,function(m){do.my.model(data,m,REML=FALSE)} )) 
}

residual.scat.plot = function(model){
  plot(fitted(model),residuals(model))
}

residual.hist = function(model){
  hist(residuals(model))
}

residual.qq.plot = function(model){
  qqnorm(residuals(model))
}

#########################################################################


# Models over all speakers & tasks

# WITH COND INSTEAD OF IAT

simpler1a = lmer(data=cog.tm,COG~Task+Label+Cond+(1|Spk),REML=FALSE)
summary(simpler1a)

simpler1b = lmer(data=cog.tm,COG~Task+Label+Cond+Sex+(1|Spk),REML=FALSE)
summary(simpler1b)

anova(simpler1a,simpler1b)

simpler1c = lmer(data=cog.tm,COG~Task+Label+Cond+Label:Task+Label:Cond+(1|Spk),REML=FALSE)
summary(simpler1c)

simpler1d = lmer(data=cog.tm,COG~Task+Label+Cond+Sex+Label:Task+Label:Cond+(1|Spk),REML=FALSE)
summary(simpler1d)

anova(simpler1c,simpler1d)

anova(simpler1a,simpler1c)

cond.ass.model.cog = lmer(data=cog.tm,COG~Task+Label+Cond+Label:Task+Label:Cond+(1|Spk),REML=TRUE)
stargazer(cond.ass.model.cog,digit.separator="")


simpler2a = lmer(data=cog.tm,COG.from.BM~Task+Label+Cond+(1|Spk),REML=FALSE)
summary(simpler2a)

simpler2b = lmer(data=cog.tm,COG.from.BM~Task+Label+Cond+Sex+(1|Spk),REML=FALSE)
summary(simpler2b)

anova(simpler2a,simpler2b)


simpler2c = lmer(data=cog.tm,COG.from.BM~Task+Label+Cond+Label:Task+Label:Cond+(1|Spk),REML=FALSE)
summary(simpler2c)

simpler2d = lmer(data=cog.tm,COG.from.BM~Task+Label+Cond+Sex+Label:Task+Label:Cond+(1|Spk),REML=FALSE)
summary(simpler2d)

anova(simpler2c,simpler2d)

anova(simpler2a,simpler2c)


cond.ass.model.cog.from.bm = lmer(data=cog.tm,COG.from.BM~Task+Label+Cond+Label:Task+Label:Cond+(1|Spk),REML=TRUE)
stargazer(cond.ass.model.cog.from.bm,digit.separator="")


stargazer(cond.ass.model.cog,cond.ass.model.cog.from.bm,digit.separator="")


# Interactions were significant: have to run seperate LMERs on each consonant T_T

cog.tm.s = cog.tm[cog.tm$Label=="s",]
cog.tm.sh = cog.tm[cog.tm$Label==SH,]

s.test1a = lmer(data=cog.tm.s,COG.from.BM~Task+Cond+(1|Spk),REML=FALSE)
s.test1b = lmer(data=cog.tm.s,COG.from.BM~Task+Cond+Sex+(1|Spk),REML=FALSE)
anova(s.test1a,s.test1b)

sh.test1a = lmer(data=cog.tm.sh,COG.from.BM~Task+Cond+(1|Spk),REML=FALSE)
sh.test1b = lmer(data=cog.tm.sh,COG.from.BM~Task+Cond+Sex+(1|Spk),REML=FALSE)
anova(sh.test1a,sh.test1b)


cond.ass.model.cog.from.bm.s = lmer(data=cog.tm.s,COG.from.BM~Task+Cond+(1|Spk),REML=TRUE)
cond.ass.model.cog.from.bm.sh = lmer(data=cog.tm.sh,COG.from.BM~Task+Cond+(1|Spk),REML=TRUE)
stargazer(cond.ass.model.cog.from.bm.s,cond.ass.model.cog.from.bm.sh,digit.separator="")


#########################################################################

# WITH IAT INSTEAD OF COND

simpler1a = lmer(data=cog.tm,COG~Task+Label+IAT.score+(1|Spk),REML=FALSE)
summary(simpler1a)

simpler1b = lmer(data=cog.tm,COG~Task+Label+IAT.score+Sex+(1|Spk),REML=FALSE)
summary(simpler1b)

anova(simpler1a,simpler1b)

simpler1c = lmer(data=cog.tm,COG~Task+Label+IAT.score+Label:Task+Label:IAT.score+(1|Spk),REML=FALSE)
summary(simpler1c)

simpler1d = lmer(data=cog.tm,COG~Task+Label+IAT.score+Sex+Label:Task+Label:IAT.score+(1|Spk),REML=FALSE)
summary(simpler1d)

anova(simpler1c,simpler1d)

anova(simpler1a,simpler1c)

iat.model.cog = lmer(data=cog.tm,COG~Task+Label+IAT.score+Label:Task+Label:IAT.score+(1|Spk),REML=TRUE)
stargazer(iat.model.cog,digit.separator="")


simpler2a = lmer(data=cog.tm,COG.from.BM~Task+Label+IAT.score+(1|Spk),REML=FALSE)
summary(simpler2a)

simpler2b = lmer(data=cog.tm,COG.from.BM~Task+Label+IAT.score+Sex+(1|Spk),REML=FALSE)
summary(simpler2b)

anova(simpler2a,simpler2b)

simpler2c = lmer(data=cog.tm,COG.from.BM~Task+Label+IAT.score+Label:Task+Label:IAT.score+(1|Spk),REML=FALSE)
summary(simpler2a)

simpler2d = lmer(data=cog.tm,COG.from.BM~Task+Label+IAT.score+Sex+Label:Task+Label:IAT.score+(1|Spk),REML=FALSE)
summary(simpler2b)

anova(simpler2c,simpler2d)

anova(simpler2a,simpler2c)

iat.model.cog.from.bm = lmer(data=cog.tm,COG.from.BM~Task+Label+IAT.score+Task:Label+Label:IAT.score+(1|Spk),REML=TRUE)
stargazer(iat.model.cog.from.bm,digit.separator="")


stargazer(iat.model.cog,iat.model.cog.from.bm,digit.separator="")


# Interactions were significant: have to run seperate LMERs on each consonant T_T

cog.tm.s = cog.tm[cog.tm$Label=="s",]
cog.tm.sh = cog.tm[cog.tm$Label==SH,]

s.test1a = lmer(data=cog.tm.s,COG.from.BM~Task+IAT.score+(1|Spk),REML=FALSE)
s.test1b = lmer(data=cog.tm.s,COG.from.BM~Task+IAT.score+Sex+(1|Spk),REML=FALSE)
anova(s.test1a,s.test1b)

sh.test1a = lmer(data=cog.tm.sh,COG.from.BM~Task+IAT.score+(1|Spk),REML=FALSE)
sh.test1b = lmer(data=cog.tm.sh,COG.from.BM~Task+IAT.score+Sex+(1|Spk),REML=FALSE)
anova(sh.test1a,sh.test1b)


cond.ass.model.cog.from.bm.s = lmer(data=cog.tm.s,COG.from.BM~Task+IAT.score+(1|Spk),REML=TRUE)
cond.ass.model.cog.from.bm.sh = lmer(data=cog.tm.sh,COG.from.BM~Task+IAT.score+(1|Spk),REML=TRUE)
stargazer(cond.ass.model.cog.from.bm.s,cond.ass.model.cog.from.bm.sh,digit.separator="")



#######################################################################################################

# WITH COND AND IAT

# Without random slopes

models.all.cog.data.no.slopes = list(COG~Label+Task+Sex+Cond+(1|Spk),
                           COG~Label+Task+Sex+Cond+(1|Spk)+IAT.score,
                           COG~Label+Task+Sex+Cond+(1|Spk)+IAT.score+IAT.order)
compare(cog.tm,models.all.cog.data.no.slopes) # no sig. improvements


# With random slopes

models.all.cog.data = list(
  COG~Label+Task+Sex+Cond+(1+Task|Spk),
  COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk),
  COG~Label*Task+Sex+Cond+IAT.score+IAT.order+(1+Task|Spk))
compare(cog.tm,models.all.cog.data) # no sig. improvements

# w/o random slopes vs. with --> random slopes give major improvements
compare(cog.tm,list(models.all.cog.data.no.slopes[[1]],models.all.cog.data[[1]]))

# Without sex
models.all.cog.data.no.sex = list(COG~Label+Task+Cond+(1|Spk),
                                  COG~Label+Task+Cond+(1|Spk)+IAT.score,
                                  COG~Label+Task+Cond+(1|Spk)+IAT.score+IAT.order,
                                  COG~Label+Task+Cond+(1+Task|Spk),
                                  COG~Label+Task+Cond+(1+Task|Spk)+IAT.score,
                                  COG~Label+Task+Cond+(1+Task|Spk)+IAT.score+IAT.order)
compare(cog.tm,models.all.cog.data.no.sex) # --> random slopes give major improvements

compare(cog.tm,list(models.all.cog.data[[1]],models.all.cog.data.no.sex[[4]])) # dropping sex approaches beting better
compare(cog.tm,list(models.all.cog.data[[2]],models.all.cog.data.no.sex[[5]])) # better with sex -- sig.
compare(cog.tm,list(models.all.cog.data[[3]],models.all.cog.data.no.sex[[6]])) # not much better with sex

# winner up to here (roughly): models.all.cog.data[[2]]; i.e., COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk)

winner.fml = models.all.cog.data[[2]]
winner.model = lmer(winner.fml,data=cog.tm,REML=TRUE)
stargazer(winner.model,digit.separator="")

residual.scat.plot(winner.model)
residual.hist(winner.model)
residual.qq.plot(winner.model)

competitors = list(winner.fml,
                   COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk)+(1+Label|Spk),
                   COG~Label+Task+Sex+Cond+IAT.score+(1+Label+Task|Spk),
                   COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk)+(1+Label|Spk)+(1+Cond|Spk))
compare(cog.tm,competitors) # major improvement!




# Simpler

simpler3a = lmer(data=cog.tm,COG~Task+Label+Cond+IAT.score+(1|Spk),REML=FALSE)
summary(simpler3a)

simpler3b = lmer(data=cog.tm,COG~Task+Label+Cond+IAT.score+Sex+(1|Spk),REML=FALSE)
summary(simpler3b)

anova(simpler3a,simpler3b)
cog.all.fac.model = lmer(data=cog.tm,COG~Task+Label+Cond+IAT.score+Sex+(1|Spk),REML=TRUE)


simpler4a = lmer(data=cog.tm,COG.from.BM~Task+Label+Cond+IAT.score+(1|Spk),REML=FALSE)
summary(simpler4a)

simpler4b = lmer(data=cog.tm,COG.from.BM~Task+Label+Cond+IAT.score+Sex+(1|Spk),REML=FALSE)
summary(simpler4b)

anova(simpler4a,simpler4b)
stargazer(simpler4a)
cog.from.bm.all.fac.model = lmer(data=cog.tm,COG.from.BM~Task+Label+Cond+IAT.score+Sex+(1|Spk),REML=TRUE)

stargazer(cog.all.fac.model,cog.from.bm.all.fac.model,digit.separator="")




#########################################################################

# Model on baseline data only

baseline.cog = cog.tm[cog.tm$Task=="baseline",]

tapply(baseline.cog$COG, list(baseline.cog$Label,baseline.cog$Sex), mean) 

models.cog.baseline = list(COG~Label+(1|Spk),
                           COG~Label+Sex+(1|Spk),
                           COG~Label+(1+Label|Spk),
                           COG~Label:Sex+(1|Spk))
compare(baseline.cog,models.cog.baseline) # major improvement with random slopes; Sex doesn't help

baseline.winner = lmer(COG~Label+(1|Spk),data=baseline.cog,REML=TRUE)
stargazer(baseline.winner,digit.separator = "",single.row)

compare(baseline.cog,list(COG~Label+(1|Spk),COG~Label+Sex+(1|Spk)))

residual.scat.plot(baseline.winner)
residual.hist(baseline.winner)
residual.qq.plot(baseline.winner)

###

baseline.cog.from.bm = lmer(COG.from.BM~Label+(1|Spk),data=baseline.cog,REML=TRUE)
stargazer(baseline.cog.from.bm,digit.separator = "")

compare(baseline.cog,list(COG.from.BM~Label+(1|Spk),COG.from.BM~Label+Sex+(1|Spk)))

residual.scat.plot(baseline.cog.from.bm)
residual.hist(baseline.cog.from.bm)
residual.qq.plot(baseline.cog.from.bm)

###

stargazer(baseline.winner,baseline.cog.from.bm,digit.separator = "")



# Model on shadowing data only

shadowing.cog = cog.tm[cog.tm$Task=="shadowing",]

models.cog.shadowing = list(COG~Label+Sex+Cond+(1|Spk),
                           COG~Label+Sex+Cond+IAT.score+(1|Spk),
                           COG~Label*Sex+Cond+IAT.score+IAT.order+(1|Spk),
                           COG~Label*Sex+Cond+IAT.score+IAT.order+(1+Cond|Spk),
                           COG~Label*Sex+Cond+IAT.score+IAT.order+(1+Cond+Label|Spk))
compare(shadowing.cog,models.cog.shadowing)

#########################################################################

# Model on males only

male.cog = cog.tm[cog.tm$Sex=="M",]

models.cog.male = list(COG~Label+Cond+(1+Task|Spk),
                      COG~Label+Cond+IAT.score+(1+Task|Spk),
                      COG~Label+Cond+IAT.score+IAT.order+(1+Task|Spk),
                      COG~Label+Cond+IAT.score+IAT.order+(1+Task+Label|Spk))
compare(male.cog,models.cog.male)

# Model on females only

female.cog = cog.tm[cog.tm$Sex=="F",]

models.cog.female = list(COG~Label+Cond+(1+Task|Spk),
                       COG~Label+Cond+IAT.score+(1+Task|Spk),
                       COG~Label+Cond+IAT.score+IAT.order+(1+Task|Spk),
                       COG~Label+Cond+IAT.score+IAT.order+(1+Task+Label|Spk))
compare(female.cog,models.cog.female)

#########################################################################

# Conversion changes

concat.cog = melt(cog.chg.per.spk)

models.conversion.chg.s = lm(value~Cond,data=concat.cog[concat.cog$variable %in% c("chg.s"),])
summary(models.conversion.chg.s)

models.conversion.chg.sh = lm(value~Cond,data=concat.cog[concat.cog$variable %in% c("chg.s"),])
summary(models.conversion.chg.sh)

models.conversion.chg = lm(value~Cond,data=concat.cog[concat.cog$variable %in% c("chg.s","chg.sh"),])
summary(models.conversion.chg)

stargazer(models.conversion.chg.s,models.conversion.chg.sh)


cog.conv.chg = concat.cog[concat.cog$variable %in% c("chg.s","chg.sh"),]
cog.conv.chg$Phone = ifelse(cog.conv.chg$variable=="chg.s","s",SH)

models.conversion.chg = lm(value~Cond+Phone,data=cog.conv.chg)
summary(models.conversion.chg)
stargazer(models.conversion.chg)

anova(lm(value~Cond+Phone,data=cog.conv.chg),lm(value~Cond+Phone+Cond:Phone,data=cog.conv.chg))

models.conversion.chg.inter = lm(value~Cond+Phone+Cond:Phone,data=cog.conv.chg)
summary(models.conversion.chg.inter)
stargazer(models.conversion.chg.inter)

#########################################################################

# Contrast changes

contr.cog.per.task = concat.cog[grepl(concat.cog$variable,pattern="s.sh"),]
contr.cog.per.task$Task = ifelse(grepl(contr.cog.per.task$variable,pattern="base"),"baseline","shadowing")

models.contr.cog.per.task = lm(value~Task+Cond,data=contr.cog.per.task)
summary(models.contr.cog.per.task)
stargazer(models.contr.cog.per.task)

models.contr.cog.per.task.sex = lm(value~Task+Cond+Sex,data=contr.cog.per.task)
summary(models.contr.cog.per.task.sex)

compare(models.contr.cog.per.task,models.contr.cog.per.task)





