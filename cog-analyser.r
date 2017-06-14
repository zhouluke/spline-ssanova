
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
TP.SPEAKERS = c("TP2","TP3","TP4","TP5","TP6","TP7","TP8")

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
fric.data$Task = mapvalues(fric.data$Task, from = c("good"), to = c("model"))
fric.data$Task = factor(fric.data$Task, level=c("model","imit","base"))

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

filt.data$Task = factor(filt.data$Task, level=rev(c("base","imit","model")))
png(filename="COG-task-label-hist.png",width=out.width,height=out.height*1.3,res=out.res)
histogram(~ COG | Label*Task, data=filt.data)
dev.off()

png(filename="COG-task-label-density.png",width=out.width,height=out.height*1.3,res=out.res)
densityplot(~ COG | Label*Task, data=filt.data)
dev.off()

filt.data$Task = factor(filt.data$Task, level=c("base","imit","model"))
png(filename="COG-task-label-boxplots.png",width=out.width,height=out.height,res=out.res)
bwplot(COG~Label | Task, data=filt.data, layout=c(3,1))
dev.off()


png(filename="COG-s-spk-task-boxes.png",width=out.width,height=out.height,res=out.res)
bwplot(COG~Task | Spk, data=filt.data[filt.data$Label=="s" & filt.data$Spk!="BM1",])
dev.off()

png(filename="COG-sh-spk-task-boxes.png",width=out.width,height=out.height,res=out.res)
bwplot(COG~Task | Spk, data=filt.data[filt.data$Label=="ʃ" & filt.data$Spk!="BM1",])
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



# Between-task differences vs. social info

calc.task.mean = function(spk){
  spk.imit.mean = mean(filt.data[filt.data$Spk==spk & filt.data$Task=="imit"])
  spk.base.mean = mean(filt.data[filt.data$Spk==spk & filt.data$Task=="base"])
  spk.imit.mean - spk.base.mean
}

chg.per.spk = data.frame(
  Spk = TP.SPEAKERS,
  Chg = as.vector(sapply(TP.SPEAKERS,calc.task.mean))
)

chg.per.spk = merge(chg.per.spk,soc.data)

ggplot(data=chg.per.spk,aes(x=IAT,y=Chg)) + geom_point(aes(shape=Condition)) + geom_smooth(method='lm')
cor(x=chg.per.spk$IAT,y=chg.per.spk$Chg)
