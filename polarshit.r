
# Luke Zhou. May/June 2017.
# Polar SSANOVA tongue spline plotter
# Based on the scripts by Jennifer Nycz and Shuo Zhang

#install.packages("gss")
#install.packages("NISTunits", dependencies = TRUE)
#install.packages("ggplot2")
#install.packages("plyr")

rm(list = ls())

#load libraries
library(ggplot2)
library(gss)
library(NISTunits)
library(plyr)

setwd("/home/luke/Dropbox/LIN1290/spline-ssanova")

#################################################################
# CONFIGURATION! -- edit me freely!
#################################################################

speaker = "BM"
prefix = ""
postfix = "-cart.txt"
speaker.filename = paste(prefix,speaker,postfix,sep="")

bp.filename = "bite-planes.txt"
task.filter = "base"
bp.spk.nm = paste(speaker,"-",task.filter,sep="")

show.comp.cons = FALSE
show.pal = TRUE

colour.palate = "Dark2"

# Settings from AAA
ORIGIN.X = 123.472
ORIGIN.Y = 0

#################################################################
# FILE I/O
#################################################################

# SPLINE IMPORT:
# Reads in a set of Cartesian coordinates organized in a tab-delimited .txt file
# Expected columns:
# Speaker	  Task  	Label	  TokNum	  X 	Y 	AAAmeta

spk.orig.data = read.table(speaker.filename, sep="\t", header=TRUE)
spk.orig.data$X = spk.orig.data$X - ORIGIN.X
spk.orig.data$Y = spk.orig.data$Y - ORIGIN.Y
spk.orig.data$tokID = paste(spk.orig.data$Speaker,spk.orig.data$Task,spk.orig.data$Label,spk.orig.data$TokNum,sep="-")

# Renames label values
NEW.LABELS = c("s", "ʃ", "ɕ")
spk.orig.data$Label = mapvalues(spk.orig.data$Label, from = c("s", "S", "x"), to = NEW.LABELS)
#c("[s]", "[ʃ]", "[ɕ]")
COMP.CONS = c("t","k")

# Data read-in sanity check: plots all splines. One plot per set of labels.
myPlotCart <- ggplot(spk.orig.data, aes(x=X, y=Y, group = tokID, colour=Label))
myPlotCart + geom_line(aes(y=Y), alpha = 1, size=0.5) + 
  ylab("") + xlab("") + scale_color_brewer(type = "qual", palette = colour.palate) +
  facet_wrap(~ Label + Task)  + theme(legend.position="none") + theme(strip.text.x=element_text(size=30))


# BITE PLANE FILE IMPORT
# Expected columns:   X   Y   Spk-Task
bp.data = read.table(bp.filename, sep="\t", header=TRUE)
bp.spk = bp.data[bp.data$SpkTask==bp.spk.nm,]

# Calculates rotation angle (in radians)
rot.angle = -1 * atan(abs(bp.spk[2,"Y"]-bp.spk[1,"Y"])/(bp.spk[2,"X"]-bp.spk[1,"X"]))


#################################################################
# CONVERSION TO POLAR COORDINATES
#################################################################

spk.orig.data$R = sqrt(spk.orig.data$X*spk.orig.data$X + spk.orig.data$Y*spk.orig.data$Y)
spk.orig.data$Theta = (atan(spk.orig.data$Y/(spk.orig.data$X)))
spk.orig.data$Theta = ifelse(spk.orig.data$X<0,pi-abs(spk.orig.data$Theta),spk.orig.data$Theta)

# Sanity check: plots the polar data on a real polar graph
#library(plotrix)
#polar.plot(spk.orig.data$R, NISTradianTOdeg(spk.orig.data$Theta), labels="",rp.type="s",radial.lim=range(0,80))

#################################################################
# PRE-PROCESSING & FILTERING
#################################################################

# Rotation with respect to bite plane
spk.orig.data$Theta = spk.orig.data$Theta - (rot.angle)

# Extraction of palate trace data
pal.traces = spk.orig.data[spk.orig.data$Task=="pal",]

# Filtering tongue splines by task type 
spk.task.filt = spk.orig.data[spk.orig.data$Task==task.filter,]
spk.task.filt$Task = factor(spk.task.filt$Task)
spk.task.filt$Label = factor(spk.task.filt$Label)


# OPTIONAL FILTERING: Elimination of task-label combos having fewer than four r-values. 

EPSILON = 1.0*pi/180

find.strong.rays = function(row) {
  row.Theta = as.numeric(row["Theta"])
  nrow(spk.task.filt[(spk.task.filt$Theta>row.Theta-EPSILON & 
                    spk.task.filt$Theta<row.Theta+EPSILON & 
                    spk.task.filt$Speaker==row["Speaker"] &
                    spk.task.filt$Label==row["Label"] 
                    ),]) >= 4
}

strong.rays = apply(spk.task.filt,1,find.strong.rays)

spk.filt = na.omit(spk.task.filt[strong.rays,])
#spk.filt$Theta = as.numeric(spk.filt$Theta)
nrow(spk.filt)

spk.filt = spk.task.filt

#################################################################
# SSANOVA IN POLAR COORDINATES
#################################################################

# Creation of the SSANOVA model
spk.model <- ssanova(R ~ Label + Theta + Label:Theta, data=spk.filt)
#summary(spk.model)

# Generates predicted radius values for each Theta-ray
spk.new.data <- expand.grid(Theta=seq(min(spk.filt$Theta), max(spk.filt$Theta), length.out=100), 
                          Label=levels(spk.filt$Label), Task=levels(spk.filt$Task)) 
spk.new.data$R <- predict(spk.model, newdata = spk.new.data, se = T)$fit

# Generates associated standard errors for each Theta-ray 
spk.new.data$SE<- predict(spk.model, newdata = spk.new.data, se = T)$se.fit 
#head(spk.new.data)

#################################################################
# PLOTTING OF THE SSANOVA MODEL
#################################################################

# Conversion to Cartesian for plotting purposes
spk.new.data$X = spk.new.data$R * cos((spk.new.data$Theta))
spk.new.data$Y = spk.new.data$R * sin((spk.new.data$Theta))

# Calculates Cartesian coordinates for the standard errors
TWO.STD = 1.9545
rows = nrow(spk.new.data)

SE.low = data.frame(X=rep(as.double(NA),rows),Y=rep(as.double(NA),rows),stringsAsFactors=FALSE) 
SE.hi = data.frame(X=rep(as.double(NA),rows),Y=rep(as.double(NA),rows),stringsAsFactors=FALSE) 

SE.low$X = (spk.new.data$R - spk.new.data$SE*TWO.STD) * cos((spk.new.data$Theta))
SE.low$Y = (spk.new.data$R - spk.new.data$SE*TWO.STD) * sin((spk.new.data$Theta))
SE.hi$X = (spk.new.data$R + spk.new.data$SE*TWO.STD) * cos((spk.new.data$Theta))
SE.hi$Y = (spk.new.data$R + spk.new.data$SE*TWO.STD) * sin((spk.new.data$Theta))

SE.low = SE.low[order(-SE.low$X),]
SE.hi = SE.hi[order(-SE.hi$X),]

# Separation based on whether the consonants are our main ones or the "comparison" ones
main.ind = spk.new.data$Label %in% NEW.LABELS
comp.ind = spk.new.data$Label %in% COMP.CONS

main.data = spk.new.data[main.ind,]
comp.data = spk.new.data[comp.ind,]

main.SE.low = SE.low[main.ind,]
main.SE.hi = SE.hi[main.ind,]
comp.SE.low = SE.low[comp.ind,]
comp.SE.hi = SE.hi[comp.ind,]

# Plots average contours for each label
spk.graph = ggplot(main.data, aes(x = X, colour = Label))
spk.graph = spk.graph + geom_line(aes(y = Y), size=1.5, alpha=1) + 
  ylim(10,80) + xlim(-30,50) + ylab("") + xlab("") + 
  scale_color_brewer(type = "qual", palette = colour.palate) + 
  # Draws the SE range
  geom_line(data=main.SE.hi,aes(x=X, y = Y), lty=3, alpha=1) + 
  geom_line(data=main.SE.low,aes(x=X, y = Y), lty=3, alpha=1) +
  # Draws the palate trace
  { if(show.pal) 
    geom_line(data=pal.traces,aes(x=pal.traces$X, y = pal.traces$Y), size=1,lty=1, alpha=1) 
  } +
  # Draws the legend
  theme(legend.position=c(0.8, 0.3)) + theme(legend.text=element_text(size=20)) + 
  theme(legend.title=element_text(size=0)) 
  
# Draws the comparison consonants + SE range
if(show.comp.cons) {
  spk.graph = spk.graph +
  geom_line(data=comp.data,aes(x=comp.data$X, y = comp.data$Y), size=0.7, lty=1, alpha=1) +
  geom_line(data=comp.data,aes(x=comp.SE.hi$X, y = comp.SE.hi$Y), lty=3, alpha=1) +
  geom_line(data=comp.data,aes(x=comp.SE.low$X, y = comp.SE.low$Y), lty=3, alpha=1)
}

spk.graph

