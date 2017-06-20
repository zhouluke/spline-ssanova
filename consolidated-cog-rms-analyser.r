
# Luke Zhou. June 2017.
# Consolidated RMS & COG analyser.

rm(list = ls())

library(lattice)
library(plyr)
library(reshape2)
library(lme4)

source('lib-fileIO.r')

setwd("/home/luke/Dropbox/LIN1290/Graphing")

############################################################
# DATA IMPORT 
############################################################

save.constants()

rms.data = read.rms.data()
soc.data = read.soc.data()
cog.data = with.bm.reorder(read.cog.data())

cog.bm = cog.data[cog.data$Spk=="BM1",]
cog.tm = no.bm.reorder(merge(cog.data[cog.data$Spk!="BM1",],soc.data))
write.table(cog.tm,file="soc-fric-data.txt",sep="\t",row.names=FALSE,quote=FALSE)

POS.SPK = as.vector(soc.data[soc.data$Cond=="pos" & soc.data$Spk %in% TP.SPEAKERS, "Spk"])
NEG.SPK = as.vector(soc.data[soc.data$Cond=="neg" & soc.data$Spk %in% TP.SPEAKERS, "Spk"])




############################################################
# LINEAR MIXED FX MODELS
############################################################

do.my.model <- function(formula,data){
  
  model = lmer(data=data,formula,REML=FALSE)
  
  #res = plot(fitted(model),residuals(model))
  #hist.res = hist(residuals(model))
  #print(hist.res)
  return(model)
}

compare.my.models <- function(models) {
  combn(models,2, function(combo) { anova(combo[[1]],combo[[2]]) } , simplify=FALSE) 
}

compare <- function(models) {
  combn(models,2, function(combo) { 
    anova(do.my.model(combo[[1]]),do.my.model(combo[[2]])) } 
    ,simplify=FALSE) 
}


#########################################################################

# Models over all speakers & tasks

# Without random slopes

models.all.cog.data.no.slopes = list(do.my.model(COG~Label+Task+Sex+Cond+(1|Spk),cog.tm),
                           do.my.model(COG~Label+Task+Sex+Cond+(1|Spk)+IAT.score,cog.tm),
                           do.my.model(COG~Label+Task+Sex+Cond+(1|Spk)+IAT.score+IAT.order,cog.tm))
compare.my.models(models.all.cog.data.no.slopes) # no sig. improvements

# With random slopes

models.all.cog.data = list(
  do.my.model(COG~Label+Task+Sex+Cond+(1+Task|Spk),cog.tm),
  do.my.model(COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk),cog.tm),
  do.my.model(COG~Label*Task+Sex+Cond+IAT.score+IAT.order+(1+Task|Spk),cog.tm))
summary(models.all.cog.data)
compare.my.models(models.all.cog.data) # no sig. improvements

# w/o random slopes vs. with --> random slopes give major improvements
compare.my.models(list(models.all.cog.data.no.slopes[[1]],models.all.cog.data[[1]]))

# Without sex
models.all.cog.data.no.sex = list(do.my.model(COG~Label+Task+Cond+(1|Spk),cog.tm),
                                     do.my.model(COG~Label+Task+Cond+(1|Spk)+IAT.score,cog.tm),
                                     do.my.model(COG~Label+Task+Cond+(1|Spk)+IAT.score+IAT.order,cog.tm),
                                  do.my.model(COG~Label+Task+Cond+(1+Task|Spk),cog.tm),
                                  do.my.model(COG~Label+Task+Cond+(1+Task|Spk)+IAT.score,cog.tm),
                                  do.my.model(COG~Label+Task+Cond+(1+Task|Spk)+IAT.score+IAT.order,cog.tm))
compare.my.models(models.all.cog.data.no.sex) # --> random slopes give major improvements

compare.my.models(list(models.all.cog.data[[1]],models.all.cog.data.no.sex[[4]])) # dropping sex approaches beting better
compare.my.models(list(models.all.cog.data[[2]],models.all.cog.data.no.sex[[5]])) # better with sex -- sig.
compare.my.models(list(models.all.cog.data[[3]],models.all.cog.data.no.sex[[6]])) # not much better with sex

# winner up to here (roughly): models.all.cog.data[[2]]; i.e., COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk)

winner.no.reml = models.all.cog.data[[2]]
winner = lmer(COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk),data=cog.tm,REML=TRUE)
stargazer(winner,digit.separator="")

#competitors = list(COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk)+(1+Label|Spk),
                   #COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk)+(1+Label|Spk)+(1+Cond|Spk))
#compare(competitors)
#compare.my.models(list(winner.no.reml,competitor)) # major improvement!

#########################################################################

# Model on baseline data only

baseline.cog = cog.tm[cog.tm$Task=="baseline",]

models.cog.baseline = list(do.my.model(COG~Label+Sex+(1|Spk),baseline.cog),
                           do.my.model(COG~Label+(1|Spk),baseline.cog),
                           do.my.model(COG~Label+Sex+(1+Label|Spk),baseline.cog),
                           do.my.model(COG~Label+(1+Label|Spk),baseline.cog))
compare.my.models(models.cog.baseline) # major improvement with random slopes

# Model on shadowing data only

shadowing.cog = cog.tm[cog.tm$Task=="shadowing",]

models.cog.shadowing = list(do.my.model(COG~Label+Sex+Cond+(1|Spk),shadowing.cog),
                           do.my.model(COG~Label+Sex+Cond+IAT.score+(1|Spk),shadowing.cog),
                           do.my.model(COG~Label*Sex+Cond+IAT.score+IAT.order+(1|Spk),shadowing.cog))


#########################################################################

# Model on male only

male.cog = cog.data[cog.data$Sex=="M",]

models.cog.male = list(do.my.model(COG~Label+Sex+Cond+(1+Task|Spk),male.cog),
                           do.my.model(COG~Label+Sex+Cond+IAT.score+(1+Task|Spk),male.cog),
                           do.my.model(COG~Label*Sex+Cond+IAT.score+IAT.order+(1+Task|Spk),male.cog))


# Model on shadowing data only

fem.cog = cog.data[cog.data$Sex=="F",]

models.cog.fem = list(do.my.model(COG~Label+Sex+Cond+(1+Task|Spk),fem.cog),
                            do.my.model(COG~Label+Sex+Cond+IAT.score+(1+Task|Spk),fem.cog),
                            do.my.model(COG~Label*Sex+Cond+IAT.score+IAT.order+(1+Task|Spk),fem.cog))

