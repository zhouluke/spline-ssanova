
# Luke Zhou. May/June 2017.
# COG analyser.

rm(list = ls())

library(lattice)
library(plyr)

out.width = 840
out.height = 560
out.res = 144

#############################################

setwd("/home/luke/Dropbox/LIN1290/Graphing")

# For renaming labels
OLD.LABELS = c("s","S","x")
NEW.LABELS = c("s", "ʃ", "ɕ")

FRICATIVES = c("s", "ʃ")
SPEAKERS = c("BM1","TP2","TP3","TP4","TP5","TP6","TP7","TP8")
TP.SPEAKERS = SPEAKERS("TP",SPEAKERS)

bm.stim.s = 7807.109245
bm.stim.sh = 2793.041364
bm.comp = function(...) {
  panel.abline(h=c(bm.stim.sh,bm.stim.s), col="black",lty=2,alpha=0.7)
  panel.bwplot(...)
}


cog.data.filename = "cog-data.txt"
orig.data = read.table(cog.data.filename, sep="\t", header=TRUE, strip.white=TRUE)

# Revaluing & separation of long description strings
orig.data$Label = mapvalues(orig.data$Label, from = OLD.LABELS, to = NEW.LABELS)
orig.data$Spk = substr(orig.data$FileName, 1, regexpr('-', orig.data$FileName)-1)
orig.data$SpkTask = substr(orig.data$FileName, 1, regexpr('-', orig.data$FileName)+4)
orig.data$SpkTask = mapvalues(orig.data$SpkTask, from = c("BM1-fals"), to = c("BM1-good"))
orig.data$Task = substr(orig.data$SpkTask, regexpr('-', orig.data$SpkTask)+1,nchar(orig.data$SpkTask))

# Let's use fricative data only
fric.data = orig.data[orig.data$Label %in% FRICATIVES,]
fric.data$Task = mapvalues(fric.data$Task, from = c("good","base","imit"), to = c("model","baseline","shadowing"))
fric.data$Task = factor(fric.data$Task, level=c("model","shadowing","baseline"))

# Filtering by speakers
filt.data = fric.data[fric.data$Spk %in% SPEAKERS,]
filt.data$Spk = factor(filt.data$Spk)
filt.data$Task = factor(filt.data$Task)
filt.data$Label = factor(filt.data$Label)

no.bm = filt.data[filt.data$Spk %in% TP.SPEAKERS,]

#############################################

# Mean CoG for each speaker + STDs

means.agg = tapply(filt.data$COG, list(filt.data$Task, filt.data$Label), mean)
sd.agg = tapply(filt.data$COG, list(filt.data$Task, filt.data$Label), sd)

means.per.spk.task = tapply(filt.data$COG, list(filt.data$SpkTask, filt.data$Label), mean)
sd.per.spk.task = tapply(filt.data$COG, list(filt.data$SpkTask, filt.data$Label), sd)

#############################################

# Graphs

filt.data$Task = factor(filt.data$Task, level=c("baseline","shadowing","model"))
png(filename="COG-task-label-boxplots.png",width=out.width,height=out.height,res=out.res)
bwplot(COG~Label | Task, data=filt.data, layout=c(3,1),panel=bm.comp)
dev.off()

#filt.data$Task = factor(filt.data$Task, level=rev(c("base","imit","model")))
png(filename="COG-task-label-hist.png",width=out.width,height=out.height*1.3,res=out.res)
histogram(~ COG | Label*Task, data=filt.data)
dev.off()

png(filename="COG-task-label-density.png",width=out.width,height=out.height*1.3,res=out.res)
densityplot(~ COG | Label*Task, data=filt.data)
dev.off()


SPK.ORDER = c("BM1","TP3","TP8","TP7","TP4","TP2","TP5","TP6")
POS.SPK = c("TP3","TP8","TP7","TP4")
NEG.SPK = c("TP2","TP5","TP6")

levels(filt.data$Spk) <- SPK.ORDER
png(filename="COG-pos-spk-task-boxes.png",width=out.width,height=out.height,res=out.res)
bwplot(COG~Task | Spk, data=filt.data[filt.data$Label=="s" & filt.data$Spk!="BM1",],panel=bm.comp)
dev.off()

png(filename="COG-neg-spk-task-boxes.png",width=out.width,height=out.height,res=out.res)
bwplot(COG~Task | Spk, data=filt.data[filt.data$Label=="ʃ" & filt.data$Spk!="BM1",],panel=bm.comp)
dev.off()


# ANOVAs

# anova(lm(COG~Label*Task,data=no.bm))
# tapply(filt.data$COG, list(filt.data$Label), mean)
# tapply(filt.data$COG, list(filt.data$Label), sd)
# tapply(filt.data$COG, list(filt.data$Task), mean)
# tapply(filt.data$COG, list(filt.data$Task), sd)

############################################

# Social correlations

soc.data.filename = "social-data.txt"
soc.data = read.table(soc.data.filename, sep="\t", header=TRUE, strip.white=TRUE)

# Inner join: fricative data table w/ social data table
names(soc.data)[1] <- "Spk"
soc.fric = merge(filt.data,soc.data)

write.table(soc.fric,file="soc-fric-data.txt",sep="\t",row.names=FALSE,quote=FALSE)

# source("http://www.danielezrajohnson.com/Rbrul.R")

############################################

# Between-task differences vs. social info

calc.task.mean = function(spk,task,label){
  #spk.s.mean = mean(filt.data[filt.data$Spk==spk & filt.data$Task==task & filt.data$Label==label,"COG"])
  #spk.sh.mean = mean(filt.data[filt.data$Spk==spk & filt.data$Task==task & filt.data$Label==label,"COG"])
  #spk.s.mean - spk.sh.mean
  mean(filt.data[filt.data$Spk==spk & filt.data$Task==task & filt.data$Label==label,"COG"])
}

chg.per.spk = data.frame(
  Spk = TP.SPEAKERS,
  s.base = as.vector(sapply(TP.SPEAKERS,function(spk) calc.task.mean(spk,"base","s") )),
  s.imit = as.vector(sapply(TP.SPEAKERS,function(spk) calc.task.mean(spk,"imit","s") )),
  sh.base = as.vector(sapply(TP.SPEAKERS,function(spk) calc.task.mean(spk,"base","ʃ") )),
  sh.imit = as.vector(sapply(TP.SPEAKERS,function(spk) calc.task.mean(spk,"imit","ʃ") ))
)

chg.per.spk$s.sh.base = chg.per.spk$s.base - chg.per.spk$sh.base
chg.per.spk$s.sh.imit = chg.per.spk$s.imit - chg.per.spk$sh.imit
chg.per.spk$Chg = chg.per.spk$s.sh.imit - chg.per.spk$s.sh.base

chg.per.spk = merge(chg.per.spk,soc.data)

png(filename="s-sh-chg-vs-iat.png",width=out.width,height=out.height,res=out.res)
ggplot(data=chg.per.spk,aes(x=IAT,y=Chg)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in RMS (mm)") + xlab("IAT score")
dev.off()

cor(x=chg.per.spk$IAT,y=chg.per.spk$Chg)


# Betweek-task differences vs. baseline separation of the consonants

png(filename="s-sh-chg-vs-base.png",width=out.width,height=out.height,res=out.res)
ggplot(data=chg.per.spk,aes(x=s.sh.base,y=Chg)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in RMS (mm)") + xlab("RMS in baseline task (mm)")
dev.off()

cor(x=chg.per.spk$s.sh.base,y=chg.per.spk$Chg)


# Betweek-task differences vs. baseline separation of the consonants

png(filename="s-sh-chg-vs-base-abs.png",width=out.width,height=out.height,res=out.res)
ggplot(data=chg.per.spk,aes(x=s.sh.base,y=abs(Chg))) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("|Change in RMS (mm)|") + xlab("RMS in baseline task (mm)")
dev.off()

cor(x=chg.per.spk$s.sh.base,y=abs(chg.per.spk$Chg))


# Imitation separation vs. baseline separation

png(filename="s-sh-imit-vs-base.png",width=out.width,height=out.height,res=out.res)
ggplot(data=chg.per.spk,aes(x=s.sh.base,y=s.sh.imit)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("RMS in shadowing task (mm)") + xlab("RMS in baseline task (mm)")
dev.off()

cor(x=chg.per.spk$s.sh.base,y=chg.per.spk$s.sh.imit)

############################################

# Distance FROM BM
bm.mean.s = mean(filt.data[filt.data$Spk=="BM1" & filt.data$Label=="s","COG"])
bm.mean.sh = mean(filt.data[filt.data$Spk=="BM1" & filt.data$Label=="ʃ","COG"])


bm.s = bm.stim.s
bm.sh = bm.stim.sh


# Distance of each speaker from BM

chg.per.spk$s.from.bm.base = chg.per.spk$s.base - bm.s
chg.per.spk$sh.from.bm.base = chg.per.spk$sh.base - bm.sh
chg.per.spk$s.from.bm.imit = chg.per.spk$s.imit - bm.s
chg.per.spk$sh.from.bm.imit = chg.per.spk$sh.imit - bm.sh

chg.per.spk$chg.s = chg.per.spk$s.imit - chg.per.spk$s.base
chg.per.spk$chg.sh = chg.per.spk$sh.imit - chg.per.spk$sh.base



png(filename="s-dev-chg-vs-iat.png",width=out.width,height=out.height,res=out.res)
ggplot(data=chg.per.spk,aes(x=IAT,y=chg.s)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("IAT score")
dev.off()
cor(x=chg.per.spk$IAT,y=chg.per.spk$chg.s)

png(filename="sh-dev-chg-vs-iat.png",width=out.width,height=out.height,res=out.res)
ggplot(data=chg.per.spk,aes(x=IAT,y=chg.sh)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("IAT score")
dev.off()
cor(x=chg.per.spk$IAT,y=chg.per.spk$chg.sh)



png(filename="s-dev-chg-vs-base-sepn.png",width=out.width,height=out.height,res=out.res)
plot1 = ggplot(data=chg.per.spk,aes(x=s.sh.base,y=chg.s)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("Baseline separation (Hz)") + facet_grid(. ~ sex)
dev.off()
cor(x=chg.per.spk$s.sh.base,y=chg.per.spk$chg.s)

png(filename="sh-dev-chg-vs-base-sepn.png",width=out.width,height=out.height,res=out.res)
plot2=ggplot(data=chg.per.spk,aes(x=s.sh.base,y=chg.sh)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("Baseline separation (Hz)")
dev.off()
cor(x=chg.per.spk$s.sh.base,y=chg.per.spk$chg.sh)


