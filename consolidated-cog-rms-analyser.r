
# Luke Zhou. June 2017.
# Consolidated RMS & COG analyser.

rm(list = ls())

library(lattice)
library(plyr)
library(reshape2)

library(lme4)

out.width = 840
out.height = 560
out.height.small = 480
out.res = 144

setwd("/home/luke/Dropbox/LIN1290/Graphing")

############################################################

# For renaming labels
OLD.LABELS = c("s","S","x")
SH = "ʃ"
C = "ɕ"
NEW.LABELS = c("s", SH, C)

FRICATIVES = c("s", SH)
SPEAKERS = c("BM1","TP2","TP3","TP4","TP5","TP6","TP7","TP8","TP9")
TP.SPEAKERS = SPEAKERS[grepl("TP",SPEAKERS)]

############################################################
# DATA IMPORT HELPER FUNCTIONS
############################################################

cog.data.filename = "cog-data.txt"
rms.data.filename = "rms-data.txt"
soc.data.filename = "social-data.txt"

read.cog.data = function(){ 
  
  orig.data = read.table(cog.data.filename, sep="\t", header=TRUE, strip.white=TRUE)
  
  # Drops start & end times from Praat
  orig.data = orig.data[,!(names(orig.data) %in% c("Start","End"))]
  
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
  
  return(filt.data)
}

read.rms.data = function(){
  
  orig.data = read.table(rms.data.filename, sep="\t", header=TRUE)
  
  filt.by.spk = orig.data[orig.data$Spk %in% SPEAKERS,]
  filt.by.spk$s.sh.tasks = filt.by.spk$s.sh.imit - filt.by.spk$s.sh.base
  filt.by.spk$k.t.tasks = filt.by.spk$k.t.imit - filt.by.spk$k.t.base
  filt.by.spk$chg.quotient = abs(filt.by.spk$s.sh.tasks / filt.by.spk$k.t.tasks)
  
  filt.by.spk$Cond <- factor(filt.by.spk$Cond, levels = c("pos","neg"))
  
  NUM.SPK = length(SPEAKERS)
  
  concat.data = melt(filt.by.spk, 
                     id.vars=c("Spk","Cond","Sex","rot.tasks","IAT.id","IAT.score"),
                     variable.name="Type", value.name="y")
  
  return(concat.data)
}

read.soc.data = function(){
  
  soc.data = read.table(soc.data.filename, sep="\t", header=TRUE, strip.white=TRUE)
  
  # Inner join: fricative data table w/ social data table
  names(soc.data) <- c("Spk","IAT.score","Sex","Cond","IAT.order")
  
  soc.data$Cond = mapvalues(soc.data$Cond, from = c("+","-"), to = c("pos","neg"))
  
  return(soc.data)
}

cog.data = read.cog.data()
rms.data = read.rms.data()
soc.data = read.soc.data()

cog.bm = cog.data[cog.data$Spk=="BM1",]
cog.data = merge(cog.data,soc.data)
write.table(cog.data,file="soc-fric-data.txt",sep="\t",row.names=FALSE,quote=FALSE)


#########################################################################

bm.stim.s = 7807.109245
bm.stim.sh = 2793.041364
bm.comp = function(...) {
  panel.abline(h=c(bm.stim.sh,bm.stim.s), col="black",lty=2,alpha=0.7)
  panel.bwplot(...)
}

cog.data$Task = factor(cog.data$Task, level=c("baseline","shadowing","model"))
png(filename="COG-task-label-boxplots.png",width=out.width,height=out.height,res=out.res)
bwplot(COG~Label | Task, data=cog.data, ylab="CoG (Hz)",layout=c(3,1),panel=bm.comp)
dev.off()


#########################################################################


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


#########################################################################

# Models over all speakers & tasks

# Without random slopes

models.all.cog.data.no.slopes = list(do.my.model(COG~Label+Task+Sex+Cond+(1|Spk),cog.data),
                           do.my.model(COG~Label+Task+Sex+Cond+(1|Spk)+IAT.score,cog.data),
                           do.my.model(COG~Label+Task+Sex+Cond+(1|Spk)+IAT.score+IAT.order,cog.data))

compare.my.models(models.all.cog.data.no.slopes)

# With random slopes

models.all.cog.data = list(
  do.my.model(COG~Label+Task+Sex+Cond+(1+Task|Spk),cog.data),
  do.my.model(COG~Label+Task+Sex+Cond+IAT.score+(1+Task|Spk),cog.data),
  do.my.model(COG~Label*Task+Sex+Cond+IAT.score+IAT.order+(1+Task|Spk),cog.data))

summary(models.all.cog.data)
compare.my.models(models.all.cog.data)

# w/o random slopes vs. with
compare.my.models(list(models.all.cog.data.no.slopes[[1]],models.all.cog.data[[1]]))


# Without sex
models.all.cog.data.no.sex = list(do.my.model(COG~Label+Task+Cond+(1|Spk),cog.data),
                                     do.my.model(COG~Label+Task+Cond+(1|Spk)+IAT.score,cog.data),
                                     do.my.model(COG~Label+Task+Cond+(1|Spk)+IAT.score+IAT.order,cog.data))
compare.my.models(models.all.cog.data.no.sex)



#########################################################################

# Model on baseline data only

baseline.cog = cog.data[cog.data$Task=="baseline",]

models.cog.baseline = list(do.my.model(COG~Label+Sex+Cond+(1+Task|Spk),baseline.cog),
                           do.my.model(COG~Label+Sex+Cond+IAT.score+(1+Task|Spk),baseline.cog),
                           do.my.model(COG~Label*Sex+Cond+IAT.score+IAT.order+(1+Task|Spk),baseline.cog))


# Model on shadowing data only

shadowing.cog = cog.data[cog.data$Task=="shadowing",]

models.cog.shadowing = list(do.my.model(COG~Label+Sex+Cond+(1+Task|Spk),shadowing.cog),
                           do.my.model(COG~Label+Sex+Cond+IAT.score+(1+Task|Spk),shadowing.cog),
                           do.my.model(COG~Label*Sex+Cond+IAT.score+IAT.order+(1+Task|Spk),shadowing.cog))


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

