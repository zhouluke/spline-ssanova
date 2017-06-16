
# Luke Zhou. June 2017.
# RMS analyzer.


rm(list = ls())

library(lattice)
library(plyr)
library(gss)

out.width = 840
out.height = 560
out.res = 144

setwd("/home/luke/Dropbox/LIN1290/Graphing")

###############################################

data.filename = "rms-data.txt"

KEEP.SPK = c("TP2","TP3","TP4","TP5","TP6","TP7","TP8")
SPK.ORDER = c("TP3","TP8","TP7","TP4","TP2","TP5","TP6")

POS.SPK = c("TP3","TP8","TP7","TP4")
NEG.SPK = c("TP2","TP5","TP6")

orig.data = read.table(data.filename, sep="\t", header=TRUE)

filt.by.spk = orig.data[orig.data$Spk %in% KEEP.SPK,]
filt.by.spk$s.sh.tasks = filt.by.spk$s.sh.imit - filt.by.spk$s.sh.base
filt.by.spk$k.t.tasks = filt.by.spk$k.t.imit - filt.by.spk$k.t.base
filt.by.spk$chg.quotient = abs(filt.by.spk$s.sh.tasks / filt.by.spk$k.t.tasks)

filt.by.spk$Cond <- factor(filt.by.spk$Cond, levels = c("pos","neg"))

NUM.SPK = length(KEEP.SPK)

concat.data = data.frame(
  Spk = factor(rep(KEEP.SPK,7)),
  Type = factor(c(rep("s.sh.base",NUM.SPK),rep("s.sh.imit",NUM.SPK),
                    c(rep("k.t.base",NUM.SPK),rep("k.t.imit",NUM.SPK),
                        rep("s.sh",NUM.SPK),rep("k.t",NUM.SPK),rep("chg.quotient",NUM.SPK)))),
  y = c(filt.by.spk$s.sh.base,filt.by.spk$s.sh.imit,
        filt.by.spk$k.t.base,filt.by.spk$k.t.imit,
        filt.by.spk$s.sh.tasks,filt.by.spk$k.t.tasks,filt.by.spk$chg.quotient)
)

concat.data = merge(concat.data,filt.by.spk[,c("Spk","Cond","Sex","rot.tasks","IAT.id")])

# Reorders speakers for graphing
concat.data$spk.order = match(concat.data$Spk,SPK.ORDER)
concat.data = concat.data[order(concat.data$spk.order),]

# Plots each task's RMS for each consonant pair
s.sh.only = concat.data[grepl("s.sh.",concat.data$Type),]
k.t.only = concat.data[grepl("k.t.",concat.data$Type),]


png(filename="rms-sep-per-task-s-sh.png",width=out.width,height=out.height,res=out.res)
ggplot(data=s.sh.only, aes(x=Spk, y=y, fill=Type, label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  facet_grid(~Cond, switch = "x", scales = "free_x", space = "free_x")
dev.off()

png(filename="rms-sep-per-task-k-t.png",width=out.width,height=out.height,res=out.res)
ggplot(data=k.t.only, aes(x=Spk, y=y, fill=Type, label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  facet_grid(~Cond, switch = "x", scales = "free_x", space = "free_x")
dev.off()

task.pair.rms = rbind(s.sh.only,k.t.only)
task.pair.rms$Graph = ifelse(grepl("s.sh.",task.pair.rms$Type),"s.sh","k.t")

ggplot(data=task.pair.rms, aes(x=Spk, y=y, fill=Type, label=Spk)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  facet_grid(~Cond, switch = "x", scales = "free_x", space = "free_x") #+
  #facet_grid(~Graph)


# Plots between-task changes for each consonant pair
changes.only = concat.data[concat.data$type!="chg.quotient" & concat.data$type %in% c("s.sh","k.t","chg.quotient"),]
ggplot(data=changes.only, aes(x=spk, y=y, fill=type)) +
  geom_bar(stat="identity",position = "dodge",width=.75) +
  scale_x_discrete(limits=SPK.ORDER)


per.task.data = concat.data[concat.data$type %in% c("s.sh.base","s.sh.imit","k.t.base","k.t.imit"),]
per.task.data$is.base = ifelse(per.task.data$type == "s.sh.base" | per.task.data$type == "k.t.base", 0,1)

s.sh.per.task.data = per.task.data[per.task.data$type %in% c("s.sh.base","s.sh.imit"),]
k.t.per.task.data = per.task.data[per.task.data$type %in% c("k.t.base","k.t.imit"),]


s.sh.pos = s.sh.per.task.data[s.sh.per.task.data$spk %in% POS.SPK,]
s.sh.neg = s.sh.per.task.data[s.sh.per.task.data$spk %in% NEG.SPK,]

# +ve speakers only
ggplot(data=s.sh.pos, 
       aes(x=type, y=y, group=spk, shape=factor(s.sh.pos$spk))) + 
  geom_point() + geom_line() + theme(legend.title = element_blank())

# -ve speakers only
ggplot(data=s.sh.neg, 
       aes(x=type, y=y, group=spk, shape=factor(s.sh.neg$spk))) + 
  geom_point() + geom_line() + theme(legend.title = element_blank())

# Both conditions' speakers
ggplot(data=s.sh.per.task.data, 
       aes(x=type, y=y, group=spk, linetype=factor(s.sh.per.task.data$spk))) + 
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
    