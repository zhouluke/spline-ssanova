
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
# CONFIGURATION! -- edit me freely!
#################################################################

speaker = "BM"
speaker.filename = paste(speaker,"-s-sh-cart.txt",sep="")

task.filter = "base"

bp.filename = "bite-planes.txt"
bp.spk.nm = paste(speaker,"-",task.filter,sep="")

show.comp.cons = FALSE
show.pal = TRUE

colour.palate = "Dark2"

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

spk.orig.data$r = sqrt(spk.orig.data$X*spk.orig.data$X + spk.orig.data$Y*spk.orig.data$Y)
spk.orig.data$theta = (atan(spk.orig.data$Y/(spk.orig.data$X)))
spk.orig.data$theta = ifelse(spk.orig.data$X<0,pi-abs(spk.orig.data$theta),spk.orig.data$theta)

# Sanity check: plots the polar data on a real polar graph
#library(plotrix)
#polar.plot(spk.orig.data$r, NISTradianTOdeg(spk.orig.data$theta), labels="",rp.type="s",radial.lim=range(0,80))

#################################################################
# PRE-PROCESSING & FILTERING
#################################################################

# Rotation with respect to bite plane
spk.orig.data$theta = spk.orig.data$theta - (rot.angle)

# Extraction of palate trace data
pal.traces = spk.orig.data[spk.orig.data$Task=="pal",]

# Filtering tongue splines by task type 
spk.task.filt = spk.orig.data[spk.orig.data$Task==task.filter,]
spk.task.filt$Task = factor(spk.task.filt$Task)
spk.task.filt$Label = factor(spk.task.filt$Label)


# OPTIONAL FILTERING: Elimination of task-label combos having fewer than four r-values. 

EPSILON = 1.0*pi/180

filter.weak.rays = function(row) {
  row.theta = as.numeric(row["theta"])
  nrow(spk.task.filt[(spk.task.filt$theta>row.theta-EPSILON & 
                    spk.task.filt$theta<row.theta+EPSILON & 
                    spk.task.filt$Speaker==row["Speaker"] &
                    spk.task.filt$Label==row["Label"] 
                    ),]) >= 4
}

strong.rays = apply(spk.task.filt,1,filter.weak.rays)

spk.filt = na.omit(spk.task.filt[strong.rays,])
#spk.filt$theta = as.numeric(spk.filt$theta)
nrow(spk.filt)

#################################################################
# SSANOVA IN POLAR COORDINATES
#################################################################

# Creation of the SSANOVA model
spk.model <- ssanova(r ~ Label + theta + Label:theta, data=spk.filt)
#summary(spk.model)

# Generates predicted radius values for each theta-ray
spk.new.data <- expand.grid(theta=seq(min(spk.filt$theta), max(spk.filt$theta), length.out=100), 
                          Label=levels(spk.filt$Label), Task=levels(spk.filt$Task)) 
spk.new.data$r <- predict(spk.model, newdata = spk.new.data, se = T)$fit

# Generates associated standard errors for each theta-ray 
spk.new.data$SE<- predict(spk.model, newdata = spk.new.data, se = T)$se.fit 
#head(spk.new.data)

#################################################################
# PLOTTING OF THE SSANOVA MODEL
#################################################################

# Conversion to Cartesian for plotting purposes
spk.new.data$X = spk.new.data$r * cos((spk.new.data$theta))
spk.new.data$Y = spk.new.data$r * sin((spk.new.data$theta))

# Calculates Cartesian coordinates for the standard errors
TWO.STD = 1.9545
spk.new.data$SE.low.x = (spk.new.data$r - spk.new.data$SE*TWO.STD) * cos((spk.new.data$theta))
spk.new.data$SE.low.y = (spk.new.data$r - spk.new.data$SE*TWO.STD) * sin((spk.new.data$theta))
spk.new.data$SE.hi.x = (spk.new.data$r + spk.new.data$SE*TWO.STD) * cos((spk.new.data$theta))
spk.new.data$SE.hi.y = (spk.new.data$r + spk.new.data$SE*TWO.STD) * sin((spk.new.data$theta))
#head(spk.new.data)

main.cons = spk.new.data[spk.new.data$Label %in% NEW.LABELS,]
comp.cons = spk.new.data[spk.new.data$Label %in% COMP.CONS,]



# Plots average contours for each label
spk.graph = ggplot(main.cons, aes(x = X, colour = Label))
spk.graph = spk.graph + geom_line(aes(y = Y), size=1.5, alpha=1) + 
  ylim(10,80) + xlim(-30,50) +
  scale_color_brewer(type = "qual", palette = colour.palate) + ylab("") + xlab("") + 
  # Draws the SE range
  geom_line(aes(x=SE.hi.x, y = SE.hi.y), lty=3, alpha=1) + 
  geom_line(aes(x=SE.low.x, y = SE.low.y), lty=3, alpha=1) +
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
  geom_line(data=comp.cons,aes(x=comp.cons$X, y = comp.cons$Y), size=0.7, lty=1, alpha=1) +
  geom_line(data=comp.cons,aes(x=SE.hi.x, y = SE.hi.y), lty=3, alpha=1) +
  geom_line(data=comp.cons,aes(x=SE.low.x, y = SE.low.y), lty=3, alpha=1)
}

spk.graph
  
  

