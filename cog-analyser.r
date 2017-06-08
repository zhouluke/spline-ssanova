
# Luke Zhou. May/June 2017.
# COG analyser.

rm(list = ls())

library(lattice)

#############################################

setwd("/home/luke/Dropbox/LIN1290/Graphing")

# For renaming labels
OLD.LABELS = c("s","S","x")
NEW.LABELS = c("s", "ʃ", "ɕ")

FRICATIVES = c("s", "ʃ")
SPEAKERS = c("BM1","TP2","TP3","TP4","TP5","TP6","TP7","TP8")

data.filename = "cog-data.txt"
orig.data = read.table(data.filename, sep="\t", header=TRUE, strip.white=TRUE)

orig.data$Label = mapvalues(orig.data$Label, from = OLD.LABELS, to = NEW.LABELS)

orig.data$Spk = substr(orig.data$FileName, 1, regexpr('-', orig.data$FileName)-1)
orig.data$SpkTask = substr(orig.data$FileName, 1, regexpr('-', orig.data$FileName)+4)
orig.data$SpkTask = mapvalues(orig.data$SpkTask, from = c("BM1-fals"), to = c("BM1-good"))

orig.data$Task = substr(orig.data$SpkTask, regexpr('-', orig.data$SpkTask)+1,nchar(orig.data$SpkTask))

fric.data = orig.data[orig.data$Label %in% FRICATIVES,]
fric.data$Task = mapvalues(fric.data$Task, from = c("good"), to = c("model"))
fric.data$Task = factor(fric.data$Task, level=c("model","imit","base"))

filt.data = fric.data[fric.data$Spk %in% SPEAKERS,]

filt.data$Task = factor(filt.data$Task)
filt.data$Label = factor(filt.data$Label)

#############################################

# Mean CoG for each speaker + STDs

tapply(filt.data$COG, list(filt.data$Task, filt.data$Label), mean)
tapply(filt.data$COG, list(filt.data$Task, filt.data$Label), sd)

#############################################

# Graphs

histogram(~ COG | Label*Task, data=filt.data)

densityplot(~ COG | Label*Task, data=filt.data)

filt.data$Task = factor(filt.data$Task, level=c("base","imit","model"))
bwplot(COG~Label | Task, data=filt.data)

anova(lm(COG~Label*Task,data=filt.data[filt.data$Task!="model",]))
