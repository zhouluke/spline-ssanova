
# Luke Zhou. June 2017.
# Consolidated RMS & COG analyser.

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

# Conversion change



