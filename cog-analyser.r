
# Luke Zhou. May/June 2017.
# COG analyser.

rm(list = ls())

library(lattice)
library(plyr)
library(reshape2)
library(lme4)
library(ggplot2)

setwd("/home/luke/Dropbox/LIN1290/Graphing")

source('lib-fileIO.r')
def.globals()

############################################################

bm.comp.boxplot = function(plot,colour="black",nudge_x=0.5){
  
  RED = "black" #"#BB0000"
  BLUE = "black" #"turquoise2"
  
  plot + ylab("CoG (Hz)") + geom_boxplot() +
    stat_summary(fun.y=mean, geom="point", shape=5, size=3) +
    geom_hline(aes(yintercept=bm.stim.s), linetype="dashed", colour=RED) +
    geom_hline(aes(yintercept=bm.stim.sh), linetype="dashed", colour=BLUE) +
    geom_text(aes(0, bm.stim.s ,label = "s", vjust = -0.5),nudge_x=nudge_x,colour=RED) +
    geom_text(aes(0, bm.stim.sh ,label = SH, vjust = -0.5),nudge_x=nudge_x,colour=BLUE)
}

############################################################
# AGGREGATE BOXPLOTS 
############################################################

panel.bm.comp = function(...) {
  panel.abline(h=c(bm.stim.sh,bm.stim.s), col="black",lty=2,alpha=0.7)
  panel.bwplot(...)
}

png(filename="COG-task-label-boxplots.png",width=out.width,height=out.height,res=out.res)
#bwplot(COG~Label | Task, data=cog.data, ylab="CoG (Hz)",layout=c(3,1),panel=panel.bm.comp)
bm.comp.boxplot(ggplot(cog.data, aes(x=Label, y=COG, fill=Label)) +
  facet_grid(. ~ Task) + ylab("CoG (Hz)") + theme(legend.position="none") 
  , colour="#BB0000", nudge_x=1.5) 
dev.off()

png(filename="COG-task-label-hist.png",width=out.width,height=out.height*1.3,res=out.res)
histogram(~ COG | Label*Task, data=cog.data)
dev.off()

png(filename="COG-task-label-density.png",width=out.width,height=out.height*1.3,res=out.res)
densityplot(~ COG | Label*Task, data=cog.data)
dev.off()

############################################################
# PER SPEAKER BOXPLOTS
############################################################

png(filename="COG-pos-spk-task-boxes.png",width=out.width*1.1,height=out.height,res=out.res/1.2)
#bwplot(COG~Task | Spk+Label, ylab="CoG (Hz)",data=cog.tm[cog.tm$Spk %in% POS.SPK,],panel=panel.bm.comp)
bm.comp.boxplot(ggplot(cog.tm[cog.tm$Spk %in% POS.SPK,], aes(x=Label, y=COG,fill=Label)) + 
  facet_grid(. ~ Spk*Task) + xlab("Phone") + theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=3,position=position_dodge(width=0.75)), nudge_x=1.5)
dev.off()

png(filename="COG-neg-spk-task-boxes.png",width=out.width*1.1,height=out.height,res=out.res/1.2)
#bwplot(COG~Task | Spk+Label, ylab="CoG (Hz)",data=cog.tm[cog.tm$Spk %in% NEG.SPK,],panel=panel.bm.comp)
bm.comp.boxplot(ggplot(cog.tm[cog.tm$Spk %in% NEG.SPK,], aes(x=Label, y=COG,fill=Label)) + 
  facet_grid(. ~ Spk*Task) + xlab("Phone") + theme(legend.position="none"), nudge_x=1.5)
dev.off()



############################################################
# Mean CoG for each speaker + STDs
############################################################

means.agg = tapply(filt.data$COG, list(filt.data$Task, filt.data$Label), mean)
sd.agg = tapply(filt.data$COG, list(filt.data$Task, filt.data$Label), sd)

means.per.spk.task = tapply(filt.data$COG, list(filt.data$SpkTask, filt.data$Label), mean)
sd.per.spk.task = tapply(filt.data$COG, list(filt.data$SpkTask, filt.data$Label), sd)



############################################################
# Between-task differences vs. social info
############################################################

calc.task.mean = function(df,spk,task,label){
  mean(df[df$Spk==spk & df$Task==task & df$Label==label,"COG"])
}

chg.per.spk = data.frame(
  Spk = TP.SPEAKERS,
  s.base = as.vector(sapply(TP.SPEAKERS,function(spk) calc.task.mean(cog.tm,spk,"baseline","s") )),
  s.imit = as.vector(sapply(TP.SPEAKERS,function(spk) calc.task.mean(cog.tm,spk,"shadowing","s") )),
  sh.base = as.vector(sapply(TP.SPEAKERS,function(spk) calc.task.mean(cog.tm,spk,"baseline",SH) )),
  sh.imit = as.vector(sapply(TP.SPEAKERS,function(spk) calc.task.mean(cog.tm,spk,"shadowing",SH) ))
)

chg.per.spk$s.sh.base = chg.per.spk$s.base - chg.per.spk$sh.base
chg.per.spk$s.sh.imit = chg.per.spk$s.imit - chg.per.spk$sh.imit
chg.per.spk$Chg = chg.per.spk$s.sh.imit - chg.per.spk$s.sh.base

chg.per.spk = merge(chg.per.spk,soc.data)



png(filename="s-sh-chg-vs-iat.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=chg.per.spk,aes(x=IAT.score,y=Chg)) + 
  geom_point(aes(shape=Cond)) + geom_smooth(method='lm') +
  ylab("∆CoGD(s,ʃ) (Hz)") + xlab("IAT score") + theme(legend.text=element_text(size=14))
dev.off()

cor(x=chg.per.spk$IAT,y=chg.per.spk$Chg)


# Between-task (pair-wise) differences vs. baseline separation of the consonants

png(filename="s-sh-chg-vs-base.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=chg.per.spk,aes(x=s.sh.base,y=Chg)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("∆CoGD(s,ʃ) (Hz)") + xlab("CoGD(s,ʃ,baseline) (Hz)")
dev.off()

cor(x=chg.per.spk$s.sh.base,y=chg.per.spk$Chg)


# Betweek-task differences vs. baseline separation of the consonants

png(filename="s-sh-chg-vs-base-abs.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=chg.per.spk,aes(x=s.sh.base,y=abs(Chg))) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("|∆CoGD(s,ʃ)| (Hz)") + xlab("CoGD(s,ʃ,baseline) (Hz)")
dev.off()

cor(x=chg.per.spk$s.sh.base,y=abs(chg.per.spk$Chg))


# Imitation separation vs. baseline separation

png(filename="s-sh-imit-vs-base.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=chg.per.spk,aes(x=s.sh.base,y=s.sh.imit)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("CoGD(s,ʃ,shadowing) (Hz)") + xlab("CoGD(s,ʃ,baseline) (Hz)")
dev.off()

cor(x=chg.per.spk$s.sh.base,y=chg.per.spk$s.sh.imit)

############################################

# Distance FROM BM
bm.mean.s = mean(filt.data[filt.data$Spk=="BM1" & filt.data$Label=="s","COG"])
bm.mean.sh = mean(filt.data[filt.data$Spk=="BM1" & filt.data$Label==SH,"COG"])


bm.s = bm.stim.s
bm.sh = bm.stim.sh


# Distance of each speaker from BM

chg.per.spk$s.from.bm.base = chg.per.spk$s.base - bm.s
chg.per.spk$sh.from.bm.base = chg.per.spk$sh.base - bm.sh
chg.per.spk$s.from.bm.imit = chg.per.spk$s.imit - bm.s
chg.per.spk$sh.from.bm.imit = chg.per.spk$sh.imit - bm.sh

chg.per.spk$chg.s = chg.per.spk$s.imit - chg.per.spk$s.base
chg.per.spk$chg.sh = chg.per.spk$sh.imit - chg.per.spk$sh.base



png(filename="s-dev-chg-vs-iat.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=chg.per.spk,aes(x=IAT,y=chg.s)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("IAT score") + 
  theme(legend.text=element_text(size=14))
dev.off()
cor(x=chg.per.spk$IAT,y=chg.per.spk$chg.s)

png(filename="sh-dev-chg-vs-iat.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=chg.per.spk,aes(x=IAT,y=chg.sh)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("IAT score") +
  theme(legend.text=element_text(size=14))
dev.off()
cor(x=chg.per.spk$IAT,y=chg.per.spk$chg.sh)



png(filename="s-dev-chg-vs-base-sepn.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=chg.per.spk,aes(x=s.sh.base,y=chg.s)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("Baseline separation (Hz)") 
dev.off()
cor(x=chg.per.spk$s.sh.base,y=chg.per.spk$chg.s)

png(filename="sh-dev-chg-vs-base-sepn.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=chg.per.spk,aes(x=s.sh.base,y=chg.sh)) + 
  geom_point(aes(shape=Condition)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("Baseline separation (Hz)")
dev.off()
cor(x=chg.per.spk$s.sh.base,y=chg.per.spk$chg.sh)

############################################


# Graphs: each spk's DeltaCOGD-related values

concat.data = melt(chg.per.spk)
concat.data$Condition = mapvalues(concat.data$Condition, from = c("-","+"), to = c("neg","pos"))
concat.data$Condition = factor(concat.data$Condition, levels = c("pos","neg"))
concat.data$Gender = factor(concat.data$Gender, levels = c("M","F"))

phone.wise.chg = concat.data[concat.data$variable %in% c("chg.s","chg.sh"),]

png(filename="chg-cog-phone-wise.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=phone.wise.chg, aes(x=Spk, y=value, fill=factor(variable,labels=c("s","ʃ")), label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  xlab("Speaker") + ylab("Change in CoG (Hz)") +
  facet_grid(~Condition+Gender, switch = "x", scales = "free_x", space = "free_x") + 
  theme(legend.title = element_blank())
dev.off()

range(abs(phone.wise.chg[phone.wise.chg$variable=="chg.s","value"]))
range(abs(phone.wise.chg[phone.wise.chg$variable=="chg.sh","value"]))


pair.wise.sep = concat.data[concat.data$variable %in% c("s.sh.base","s.sh.imit"),]

png(filename="chg-cog-pair-wise.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=pair.wise.sep, aes(x=Spk, y=value, fill=factor(variable,labels=c("baseline","shadowing")), label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  xlab("Speaker") + ylab("CoG separation between s & ʃ (Hz)") +
  facet_grid(~Condition+Gender, switch = "x", scales = "free_x", space = "free_x") + 
  theme(legend.title = element_blank())
dev.off()


pair.wise.chg = concat.data[concat.data$variable == "Chg",]

png(filename="chg-val-cog-pair-wise.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=pair.wise.chg, aes(x=Spk, y=value, label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  xlab("Speaker") + ylab("∆CoGD(s,ʃ) (Hz)") +
  facet_grid(~Condition+Gender, switch = "x", scales = "free_x", space = "free_x") + 
  theme(legend.title = element_blank())
dev.off()
