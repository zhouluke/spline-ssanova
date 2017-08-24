
# Luke Zhou. May/June 2017.
# COG analyser.

rm(list = ls())

library(lattice)
library(plyr)
library(reshape2)
library(lme4)
library(ggplot2)
library(grid)

setwd("/home/luke/Dropbox/LIN1290/Graphing")

source('lib-fileIO.r')
def.globals()

############################################################

bm.comp.boxplot = function(plot,colour="black",nudge_x=1.5){
  
  RED = "black" #"#BB0000"
  BLUE = "black" #"turquoise2"
  
  g = plot + ylab("CoG (Hz)") + geom_boxplot() +
    stat_summary(fun.y=mean, geom="point", shape=5, size=3) +
    geom_hline(aes(yintercept=bm.stim.s), linetype="dashed", colour=RED) +
    geom_hline(aes(yintercept=bm.stim.sh), linetype="dashed", colour=BLUE) +
    theme(plot.margin=unit(c(1, 3.4, 0.5, 0.5), "lines"))
    #geom_text(aes(-1.1, bm.stim.s ,label = "s", vjust = -0.5),nudge_x=nudge_x,colour=RED) #+
    #geom_text(aes(0, bm.stim.sh ,label = SH, vjust = -0.5),nudge_x=nudge_x,colour=BLUE)
  
  g = g + 
    annotation_custom(grob=textGrob("[s] (BM)",hjust=0,gp=gpar(fontsize=10)), xmin=3, xmax=Inf, ymin=bm.stim.s, ymax=bm.stim.s) + 
    annotation_custom(grob=textGrob(paste("[",SH,"] (BM)",sep=""), hjust=0, gp=gpar(fontsize=10)), xmin=3, xmax=Inf, ymin=bm.stim.sh, ymax=bm.stim.sh)
  gt <- ggplot_gtable(ggplot_build(g))
  gt$layout[grepl("panel", gt$layout$name), ]$clip <- "off"
  grid.draw(gt)
}

############################################################
# AGGREGATE BOXPLOTS 
############################################################

#panel.bm.comp = function(...) {
#  panel.abline(h=c(bm.stim.sh,bm.stim.s), col="black",lty=2,alpha=0.7)
#  panel.bwplot(...)
#}

#ann_text_s <- data.frame(Label = "s", COG = bm.stim.s,
#                       Task = factor("model",levels = c("baseline","shadowing","model")))

png(filename="COG-task-label-boxplots.png",width=out.width,height=out.height,res=out.res)
#bwplot(COG~Label | Task, data=cog.data, ylab="CoG (Hz)",layout=c(3,1),panel=panel.bm.comp)
bm.comp.boxplot(ggplot(cog.data, aes(x=Label, y=COG, fill=Label)) +
  facet_grid(. ~ Task) + ylab("CoG (Hz)") + theme(legend.position="none") #+
    #geom_text(data=ann_text_s,label = "s (BM)",nudge_x=2,vjust = -0.5,aes(group=NULL)) 
)
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
  stat_summary(fun.y=mean, geom="point", shape=5, size=3,position=position_dodge(width=0.75)))
dev.off()

png(filename="COG-neg-spk-task-boxes.png",width=out.width*1.1,height=out.height,res=out.res/1.2)
#bwplot(COG~Task | Spk+Label, ylab="CoG (Hz)",data=cog.tm[cog.tm$Spk %in% NEG.SPK,],panel=panel.bm.comp)
bm.comp.boxplot(ggplot(cog.tm[cog.tm$Spk %in% NEG.SPK,], aes(x=Label, y=COG,fill=Label)) + 
  facet_grid(. ~ Spk*Task) + xlab("Phone") + theme(legend.position="none"))
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

# Correlations w/ IAT score

png(filename="s-sh-chg-vs-iat.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=cog.chg.per.spk,aes(x=IAT.score,y=chg)) + 
  geom_point(aes(shape=Cond)) + geom_smooth(method='lm') +
  ylab("∆CoGS (Hz)") + xlab("IAT score") + theme(legend.text=element_text(size=14))
dev.off()

cor(x=cog.chg.per.spk$IAT,y=cog.chg.per.spk$Chg)


# Between-task (pair-wise) differences vs. baseline separation of the consonants

png(filename="s-sh-chg-vs-base.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=cog.chg.per.spk,aes(x=s.sh.base,y=chg)) + 
  geom_point(aes(shape=Cond)) + geom_smooth(method='lm') +
  ylab("∆CoGS (Hz)") + xlab("CoGS(baseline) (Hz)")
dev.off()

cor(x=cog.chg.per.spk$s.sh.base,y=cog.chg.per.spk$chg)


# Betweek-task differences vs. baseline separation of the consonants

png(filename="s-sh-chg-vs-base-abs.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=cog.chg.per.spk,aes(x=s.sh.base,y=abs(chg))) + 
  geom_point(aes(shape=Cond)) + geom_smooth(method='lm') +
  ylab("|∆CoGS| (Hz)") + xlab("CoGS(baseline) (Hz)")
dev.off()

cor(x=cog.chg.per.spk$s.sh.base,y=abs(cog.chg.per.spk$Chg))


# Imitation separation vs. baseline separation

png(filename="s-sh-imit-vs-base.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=cog.chg.per.spk,aes(x=s.sh.base,y=s.sh.imit)) + 
  geom_point(aes(shape=Cond)) + geom_smooth(method='lm') +
  ylab("CoGS(shadowing) (Hz)") + xlab("CoGS(baseline) (Hz)")
dev.off()

cor(x=cog.chg.per.spk$s.sh.base,y=cog.chg.per.spk$s.sh.imit)

############################################

# Distance FROM BM
bm.mean.cog.s = mean(cog.bm[cog.bm$Label=="s","COG"])
bm.mean.cog.sh = mean(cog.bm[cog.bm$Label==SH,"COG"])


# Distance of each speaker from BM

png(filename="s-dev-chg-vs-iat.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=cog.chg.per.spk,aes(x=IAT.score,y=chg.s)) + 
  geom_point(aes(shape=Cond)) + geom_smooth(method='lm') +
  ylab("∆CoGB (Hz)") + xlab("IAT score") + 
  theme(legend.text=element_text(size=14))
dev.off()
cor(x=cog.chg.per.spk$IAT,y=cog.chg.per.spk$chg.s)

png(filename="sh-dev-chg-vs-iat.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=cog.chg.per.spk,aes(x=IAT.score,y=chg.sh)) + 
  geom_point(aes(shape=Cond)) + geom_smooth(method='lm') +
  ylab("∆CoGB(Hz)") + xlab("IAT score") +
  theme(legend.text=element_text(size=14))
dev.off()
cor(x=cog.chg.per.spk$IAT.score,y=cog.chg.per.spk$chg.sh)



png(filename="s-dev-chg-vs-base-sepn.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=cog.chg.per.spk,aes(x=s.sh.base,y=chg.s)) + 
  geom_point(aes(shape=Cond)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("Baseline separation (Hz)") 
dev.off()
cor(x=cog.chg.per.spk$s.sh.base,y=cog.chg.per.spk$chg.s)

png(filename="sh-dev-chg-vs-base-sepn.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=cog.chg.per.spk,aes(x=s.sh.base,y=chg.sh)) + 
  geom_point(aes(shape=Cond)) + geom_smooth(method='lm') +
  ylab("Change in distance from BM (Hz)") + xlab("Baseline separation (Hz)")
dev.off()
cor(x=cog.chg.per.spk$s.sh.base,y=cog.chg.per.spk$chg.sh)

############################################


# Graphs: each spk's DeltaCOGD-related values

concat.cog = melt(cog.chg.per.spk)
#concat.cog$Condition = mapvalues(concat.cog$Condition, from = c("-","+"), to = c("neg","pos"))
concat.cog$Condition = concat.cog$Cond
concat.cog$Condition = factor(concat.cog$Condition, levels = c("pos","neg"))
concat.cog$Sex = factor(concat.cog$Sex, levels = c("M","F"))

phone.wise.chg = concat.cog[concat.cog$variable %in% c("chg.s","chg.sh"),]

png(filename="chg-cog-phone-wise.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=phone.wise.chg, aes(x=Spk, y=value, fill=factor(variable,labels=c("s",SH)), label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  xlab("Speaker") + ylab("∆CoGB (Hz)") +
  facet_grid(~Condition, switch = "x", scales = "free_x", space = "free_x") + 
  theme(legend.title = element_blank())
dev.off()

range(abs(phone.wise.chg[phone.wise.chg$variable=="chg.s","value"]))
range(abs(phone.wise.chg[phone.wise.chg$variable=="chg.sh","value"]))


pair.wise.sep = concat.cog[concat.cog$variable %in% c("s.sh.base","s.sh.imit"),]
pair.wise.sep$Cond <- factor(pair.wise.sep$Cond, levels=c("pos","neg"))

png(filename="chg-cog-pair-wise.png",width=out.width,height=out.height.small,res=out.res)

plot = ggplot(data=pair.wise.sep, aes(x=Spk, y=value, fill=factor(variable,labels=c("baseline","shadowing")), label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  xlab("Speaker") + ylab("CoGS (Hz)") +
  facet_grid(~Cond, switch = "x", scales = "free_x", space = "free_x") + 
  theme(legend.title = element_blank()) +
  geom_hline(aes(yintercept=bm.stim.s-bm.stim.sh), linetype="dashed") + 
  annotation_custom(grob=textGrob("BM",hjust=0,gp=gpar(fontsize=10)), xmin=5, xmax=Inf, 
                    ymin=bm.stim.s-bm.stim.sh, ymax=bm.stim.s-bm.stim.sh) 
gt <- ggplot_gtable(ggplot_build(plot))
gt$layout[grepl("panel", gt$layout$name), ]$clip <- "off"
grid.draw(gt)

dev.off()


pair.wise.chg = concat.cog[concat.cog$variable == "chg",]

png(filename="chg-val-cog-pair-wise.png",width=out.width,height=out.height.small,res=out.res)
ggplot(data=pair.wise.chg, aes(x=Spk, y=value, label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  xlab("Speaker") + ylab("∆CoGS (Hz)") +
  facet_grid(~Cond, switch = "x", scales = "free_x", space = "free_x") + 
  theme(legend.title = element_blank())
dev.off()
