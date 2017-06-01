
# Luke Zhou. May/June 2017.
# Polar SSANOVA tongue spline plotter
# Based on the scripts by Jennifer Nycz and Shuo Zhang

#install.packages("gss")
#install.packages("NISTunits", dependencies = TRUE)
#install.packages("ggplot2")
#install.packages("plyr")

#load libraries
library(ggplot2)
library(gss)
library(NISTunits)
library(plyr)

setwd("/home/luke/Dropbox/LIN1290/spline-ssanova")

ORIGIN.X = 123.472
ORIGIN.Y = 0

#################################################################
# FILE I/O
#################################################################

speaker = "BM"
speaker.filename = paste(speaker,"-s-sh-cart.txt",sep="")

task.filter = "base"

bp.filename = "bite-planes.txt"
bp.spk.nm = paste(speaker,"-",task.filter,sep="")


# SPLINE IMPORT:
# Reads in a set of Cartesian coordinates organized in a tab-delimited .txt file
# Expected columns:
# Speaker	  Task  	Label	  TokNum	  X 	Y 	AAAmeta

spkCart = read.table(speaker.filename, sep="\t", header=TRUE)
spkCart$X = spkCart$X - ORIGIN.X
spkCart$Y = spkCart$Y - ORIGIN.Y
spkCart$tokID = paste(spkCart$Speaker,spkCart$Task,spkCart$Label,spkCart$TokNum,sep="-")

# Renames label values
NEW.LABELS = c("s", "ʃ", "ɕ")
spkCart$Label = mapvalues(spkCart$Label, from = c("s", "S", "x"), to = NEW.LABELS)
#c("[s]", "[ʃ]", "[ɕ]")
COMP.CONS = c("t","k")

# Data read-in sanity check: plots all splines. One plot per set of labels.
myPlotCart <- ggplot(spkCart, aes(x=X, y=Y, group = tokID, colour=Label))
myPlotCart + geom_line(aes(y=Y), alpha = 1, size=1) + 
  ylab("") + xlab("") + scale_color_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(~ Label + Task)  + theme(legend.position="none") + theme(strip.text.x=element_text(size=30))


# BITE PLANE FILE IMPORT
# Expected columns:   X   Y   Spk-Task
bp.data = read.table(bp.filename, sep="\t", header=TRUE)
bp.spk = bp.data[bp.data$SpkTask==bp.spk.nm,]

# Calculates rotation angle in radians
rot.angle = -1 * atan(abs(bp.spk[2,"Y"]-bp.spk[1,"Y"])/(bp.spk[2,"X"]-bp.spk[1,"X"]))


#################################################################
# POLAR CONVERSION
#################################################################

# Conversion of the data to polars
spkCart$r = sqrt(spkCart$X*spkCart$X + spkCart$Y*spkCart$Y)
spkCart$theta = NISTradianTOdeg(atan(spkCart$Y/(spkCart$X)))
spkCart$theta = ifelse(spkCart$X<0,180-abs(spkCart$theta),spkCart$theta)

#library(plotrix)
#polar.plot(spkCart$r, spkCart$theta, labels="",rp.type="s",radial.lim=range(0,80))

#################################################################
# BITE PLANE ROTATION + PALATE TRACE ROW STORAGE
#################################################################

spkCart$theta = spkCart$theta - NISTradianTOdeg(rot.angle)

pal.traces = spkCart[spkCart$Task=="pal",]

#################################################################
# SSANOVA IN POLAR COORDINATES
#################################################################

# Filtering by task type
spk.task.filt = spkCart[spkCart$Task==task.filter,]
spk.task.filt$Task = factor(spk.task.filt$Task)
spk.task.filt$Label = factor(spk.task.filt$Label)

# Creation of the SSANOVA model
spkCartModel <- ssanova(r ~ Label + theta + Label:theta, data=spk.task.filt)
#summary(spkCartModel)

# Generates predicted radius values for each theta-ray
spk.new.data <- expand.grid(theta=seq(min(spk.task.filt$theta), max(spk.task.filt$theta), length.out=100), 
                          Label=levels(spk.task.filt$Label), Task=levels(spk.task.filt$Task)) 
spk.new.data$r <- predict(spkCartModel, newdata = spk.new.data, se = T)$fit

# Generates associated standard errors for each theta-ray 
spk.new.data$SE<- predict(spkCartModel, newdata = spk.new.data, se = T)$se.fit 
#head(spk.new.data)

#################################################################
# PLOTTING OF THE SSANOVA MODEL
#################################################################

# Conversion to Cartesian for plotting purposes
spk.new.data$X = spk.new.data$r * cos(NISTdegTOradian(spk.new.data$theta))
spk.new.data$Y = spk.new.data$r * sin(NISTdegTOradian(spk.new.data$theta))

# Calculates Cartesian coordinates for the standard errors
spk.new.data$SE.low.x = (spk.new.data$r - spk.new.data$SE*1.96) * cos(NISTdegTOradian(spk.new.data$theta))
spk.new.data$SE.low.y = (spk.new.data$r - spk.new.data$SE*1.96) * sin(NISTdegTOradian(spk.new.data$theta))
spk.new.data$SE.hi.x = (spk.new.data$r + spk.new.data$SE*1.96) * cos(NISTdegTOradian(spk.new.data$theta))
spk.new.data$SE.hi.y = (spk.new.data$r + spk.new.data$SE*1.96) * sin(NISTdegTOradian(spk.new.data$theta))
#head(spk.new.data)

main.cons = spk.new.data[spk.new.data$Label %in% NEW.LABELS,]
comp.cons = spk.new.data[spk.new.data$Label %in% COMP.CONS,]

# Plots average contours
spkComp = ggplot(main.cons, aes(x = X, colour = Label))
spkComp + geom_line(aes(y = Y), size=1.5, alpha=1) + 
  scale_color_brewer(type = "qual", palette = "Dark2") + ylab("") + xlab("") + 
  geom_line(aes(x=SE.hi.x, y = SE.hi.y), lty=2, alpha=1) + 
  geom_line(aes(x=SE.low.x, y = SE.low.y), lty=2, alpha=1) +
  # Draws the comparison consonants + SE range
  geom_line(data=comp.cons,aes(x=comp.cons$X, y = comp.cons$Y), size=1,lty=1, alpha=0.4) +
  geom_line(data=comp.cons,aes(x=SE.hi.x, y = SE.hi.y), lty=2, alpha=0.4) + 
  geom_line(data=comp.cons,aes(x=SE.low.x, y = SE.low.y), lty=2, alpha=0.4) +
  # Draws the palate trace
  #geom_line(data=pal.traces,aes(x=pal.traces$X, y = pal.traces$Y), size=1,lty=1, alpha=1) +
  theme(legend.position=c(0.8, 0.3)) + theme(legend.text=element_text(size=20)) + 
  theme(legend.title=element_text(size=0))

