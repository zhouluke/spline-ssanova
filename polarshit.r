
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

speaker = "TP5"
speaker.filename = paste(speaker,"-s-sh-cart.txt",sep="")

task.filter = "base"

# Reads in a set of Cartesian coordinates organized in a tab-delimited .txt file

# Expected columns:
# Speaker	  Task  	Label	  TokNum	  X 	Y 	AAAmeta

spkCart = read.table(speaker.filename, sep="\t", header=TRUE)
spkCart$X = spkCart$X - ORIGIN.X
spkCart$Y = spkCart$Y - ORIGIN.Y
spkCart$tokID = paste(spkCart$Speaker,spkCart$Task,spkCart$Label,spkCart$TokNum,sep="-")

# Renames label values
spkCart$Label = mapvalues(spkCart$Label, from = c("s", "S", "x"), to = c("s", "ʃ", "ɕ"))
#c("[s]", "[ʃ]", "[ɕ]")

# Data read-in sanity check: plots all splines. One plot per set of labels.
myPlotCart <- ggplot(spkCart, aes(x=X, y=Y, group = tokID, colour=Label))
myPlotCart + geom_line(aes(y=Y), alpha = 1, size=1) + 
  ylab("") + xlab("") + scale_color_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(~ Label + Task)  + theme(legend.position="none") + theme(strip.text.x=element_text(size=30))

#################################################################
# POLAR CONVERSION
#################################################################

# Conversion of the data to polars
spkCart$r = sqrt(spkCart$X*spkCart$X + spkCart$Y*spkCart$Y)
spkCart$thetaTmp = NISTradianTOdeg(atan(spkCart$Y/(spkCart$X)))
spkCart$theta = ifelse(spkCart$X<0,180-abs(spkCart$thetaTmp),spkCart$thetaTmp)

head(spkCart)

#library(plotrix)
polar.plot(spkCart$r, spkCart$theta, labels="",rp.type="s",radial.lim=range(0,80))

#################################################################
# SSANOVA IN POLAR COORDINATES
#################################################################

# Filters by task type
spk.task.filt = spkCart[spkCart$Task==task.filter,]

# Creation of the SSANOVA model
spkCartModel <- ssanova(r ~ Label + theta + Label:theta, data=spk.task.filt)
summary(spkCartModel)

# Generates predicted radius values for each theta-ray
spkNewData <- expand.grid(theta=seq(min(spk.task.filt$theta), max(spk.task.filt$theta), length.out=100), 
                          Label=levels(spk.task.filt$Label), Task=levels(spk.task.filt$Task)) 
spkNewData$r <- predict(spkCartModel, newdata = spkNewData, se = T)$fit

# Generates associated standard errors for each theta-ray 
spkNewData$SE<- predict(spkCartModel, newdata = spkNewData, se = T)$se.fit 
head(spkNewData)

#################################################################
# PLOTTING OF THE SSANOVA MODEL
#################################################################

# Conversion to Cartesian for plotting purposes
spkNewData$X = spkNewData$r * cos(NISTdegTOradian(spkNewData$theta))
spkNewData$Y = spkNewData$r * sin(NISTdegTOradian(spkNewData$theta))

# Calculates Cartesian coordinates for the standard errors
spkNewData$SE.low.x = (spkNewData$r - spkNewData$SE*1.96) * cos(NISTdegTOradian(spkNewData$theta))
spkNewData$SE.low.y = (spkNewData$r - spkNewData$SE*1.96) * sin(NISTdegTOradian(spkNewData$theta))
spkNewData$SE.hi.x = (spkNewData$r + spkNewData$SE*1.96) * cos(NISTdegTOradian(spkNewData$theta))
spkNewData$SE.hi.y = (spkNewData$r + spkNewData$SE*1.96) * sin(NISTdegTOradian(spkNewData$theta))
head(spkNewData)

# Plots average contours
spkComp = ggplot(spkNewData, aes(x = X, colour = Label))
spkComp + geom_line(aes(y = Y), size=1.5, alpha=1) + 
  scale_color_brewer(type = "qual", palette = "Dark2") + ylab("") + xlab("") + 
  geom_line(aes(x=SE.hi.x, y = SE.hi.y), lty=2, alpha=1) + 
  geom_line(aes(x=SE.low.x, y = SE.low.y), lty=2, alpha=1) +  
  theme(legend.position=c(0.12, 0.3)) + theme(legend.text=element_text(size=20)) + 
  theme(legend.title=element_text(size=0))

