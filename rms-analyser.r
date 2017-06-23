
# Luke Zhou. June 2017.
# RMS analyzer.

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

# Plots each task's RMS for each consonant pair

rms.s.sh = rms.data[rms.data$Type %in% c("s.sh.base","s.sh.imit"),]
rms.k.t = rms.data[rms.data$Type %in% c("k.t.base","k.t.imit"),]


png(filename="rms-sep-per-task-s-sh.png",width=out.width,height=out.height,res=out.res)
ggplot(data=rms.s.sh, aes(x=Spk, y=y, fill=Type, label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  facet_grid(~Cond, switch = "x", scales = "free_x", space = "free_x") + 
  xlab("Speaker") + ylab(paste0("RMSD between [s] and [",SH,"] (mm)")) +
  theme(legend.title = element_blank())
dev.off()

png(filename="rms-sep-per-task-k-t.png",width=out.width,height=out.height,res=out.res)
ggplot(data=rms.k.t, aes(x=Spk, y=y, fill=Type, label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  facet_grid(~Cond, switch = "x", scales = "free_x", space = "free_x") +
  xlab("Speaker") + ylab("RMSD between [k] and [t] (mm)") +
  theme(legend.title = element_blank())
dev.off()

task.pair.rms = rbind(rms.s.sh,rms.k.t)
task.pair.rms$Graph = ifelse(grepl("s.sh.",task.pair.rms$Type),"s.sh","k.t")

ggplot(data=task.pair.rms, aes(x=Spk, y=y, fill=Type, label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  facet_grid(~Cond+Sex, switch = "x", scales = "free_x", space = "free_x") #+
  #facet_grid(~Graph)

#################################################

# Plots between-task changes for each consonant pair

rms.changes.only = rms.data[rms.data$Type!="chg.quotient" & rms.data$Type %in% c("s.sh.tasks","k.t.tasks","chg.quotient"),]

png(filename="drmsd.png",width=out.width,height=480,res=out.res)
ggplot(data=rms.changes.only[rms.changes.only$rot.tasks=="N",], aes(x=Spk, y=y, fill=Type)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  facet_grid(~Cond, switch = "x", scales = "free_x", space = "free_x") +
  xlab("Speaker") + ylab("∆RMSD between baseline & shadowing (mm)") +
  theme(legend.title = element_blank())
dev.off()

range(rms.changes.only[rms.changes.only$rot.tasks=="N" & rms.changes.only$Type=="k.t","y"])
range(abs(rms.changes.only[rms.changes.only$rot.tasks=="N" & rms.changes.only$Type=="s.sh","y"]))


# LINE GRAPHS

per.task.data = concat.data[concat.data$Type %in% c("s.sh.base","s.sh.imit","k.t.base","k.t.imit"),]
per.task.data$is.base = ifelse(per.task.data$Type == "s.sh.base" | per.task.data$Type == "k.t.base", 0,1)

s.sh.per.task.data = per.task.data[per.task.data$Type %in% c("s.sh.base","s.sh.imit"),]
k.t.per.task.data = per.task.data[per.task.data$Type %in% c("k.t.base","k.t.imit"),]


s.sh.pos = s.sh.per.task.data[s.sh.per.task.data$Spk %in% POS.SPK,]
s.sh.neg = s.sh.per.task.data[s.sh.per.task.data$Spk %in% NEG.SPK,]

# +ve speakers only
ggplot(data=s.sh.pos, 
       aes(x=Type, y=y, group=Spk, shape=factor(s.sh.pos$Spk))) + 
  geom_point() + geom_line() + theme(legend.title = element_blank())

# -ve speakers only
ggplot(data=s.sh.neg, 
       aes(x=Type, y=y, group=Spk, shape=factor(s.sh.neg$Spk))) + 
  geom_point() + geom_line() + theme(legend.title = element_blank())

# Both conditions' speakers
ggplot(data=s.sh.per.task.data, 
       aes(x=Type, y=y, group=Spk, linetype=factor(s.sh.per.task.data$Spk))) + 
  geom_point() + geom_line() + theme(legend.title = element_blank())


# s-sh-base vs. s-sh-imit
ggplot(data=filt.by.spk,aes(x=s.sh.base,y=s.sh.imit)) + geom_point() + geom_smooth(method='lm')
cor(x=filt.by.spk$s.sh.base,y=filt.by.spk$s.sh.imit)

# s-sh-base vs. s-sh-tasks
ggplot(data=filt.by.spk,aes(x=s.sh.base,y=s.sh.tasks)) + geom_point() + geom_smooth(method='lm')
cor(x=filt.by.spk$s.sh.base,y=filt.by.spk$s.sh.tasks)

# iat vs. s-sh-tasks
ggplot(data=filt.by.spk,aes(x=IAT.score,y=s.sh.tasks)) + geom_point() + geom_smooth(method='lm')
cor(x=filt.by.spk$IAT.score,y=filt.by.spk$s.sh.tasks)

######################################
# k and t
ggplot(data=k.t.per.task.data, aes(x=type, y=y,colour=spk,group=spk)) + 
  geom_point() + geom_line()        
    