
# Luke Zhou. May/June 2017.
# Polar SSANOVA tongue spline plotter
# Based on the scripts by Jennifer Nycz and Shuo Zhang

#install.packages("gss")
#install.packages("ggplot2")
#install.packages("plyr")

rm(list = ls())

#load libraries
library(ggplot2)
library(gss)
library(plyr)

setwd("/home/luke/Dropbox/LIN1290/Graphing")

#################################################################
# CONFIGURATION! -- edit me freely!
#################################################################

speaker = "TP5"
task = "imit"

data.filename = "all-cart-data.txt"
bp.filename = "bite-planes.txt"

show.comp.cons = TRUE
show.pal = FALSE
show.bp = FALSE

colour.palette = "Dark2"
MIN.PTS.PER.RAY = 4

# Settings from AAA
ORIGIN.X = 123.472
ORIGIN.Y = 0

# For renaming labels
OLD.LABELS = c("s","S","x")
NEW.LABELS = c("s", "ʃ", "ɕ")
COMP.CONS = c("t","k")

# Graph dimensions for image creation
out.width = 800
out.height = 640
out.res = 72


#################################################################
# HELPER FUNCTIONS
#################################################################

sq = function(x) x*x

xy.get.radius = function(XY) return(sqrt(sq(XY$X) + sq(XY$Y)))

xy.get.theta = function(XY) {
  Theta = (atan(XY$Y/XY$X))
  # Adjustment based on whether the angle is in quadrant I or II
  Theta = ifelse(XY$X<0,pi-abs(Theta),Theta)
  return(Theta)
}

rt.get.x = function(RT) return(RT$R*cos(RT$Theta))
rt.get.y = function(RT) return(RT$R*sin(RT$Theta))


#################################################################
# FILE I/O
#################################################################

# SPLINE IMPORT:
# Reads in a set of Cartesian coordinates organized in a tab-delimited .txt file
# Expected columns:
# Speaker	  Task  	Label	  TokNum	  X 	Y 	AAAmeta

orig.data = read.table(data.filename, sep="\t", header=TRUE)
orig.data$X = orig.data$X - ORIGIN.X
orig.data$Y = orig.data$Y - ORIGIN.Y
orig.data$tokID = paste(orig.data$Speaker,orig.data$Task,orig.data$Label,orig.data$TokNum,sep="-")

# Renames label values
orig.data$Label = mapvalues(orig.data$Label, from = OLD.LABELS, to = NEW.LABELS)


# BITE PLANE FILE IMPORT
# Expected columns:   X   Y   Spk-Task
bp.data = read.table(bp.filename, sep="\t", header=TRUE)
bp.data$R = xy.get.radius(bp.data)
bp.data$Theta = xy.get.theta(bp.data)


# CONVERSION TO POLARS
orig.data$R = xy.get.radius(orig.data)
orig.data$Theta = xy.get.theta(orig.data)

# Sanity check: plots the polar data on a real polar graph
#library(plotrix)
#polar.plot(spk.orig.data$R, spk.orig.data$Theta*180/pi, labels="",rp.type="s",radial.lim=range(0,80))


#################################################################
# ITERATING OVER EACH SPEAKER
#################################################################
    
do.one.spk = function(speaker,task) {
  
    spk.orig.data = orig.data[orig.data$Speaker==speaker & orig.data$Task==task,]
    spk.orig.data$Task = factor(spk.orig.data$Task)
    spk.orig.data$Label = factor(spk.orig.data$Label)
    spk.task.str = paste(speaker,"-",task,sep="")
    
    #################################################################
    # PRE-PROCESSING & FILTERING
    #################################################################
        
    # Data read-in sanity check: plots all splines for this speaker-task combo. One plot per set of labels.
    out.file.nm = paste("raw/",spk.task.str,"-raw.png",sep="")
    png(filename=out.file.nm,width=out.width,height=out.height,res=out.res)
    myPlotCart <- ggplot(spk.orig.data, aes(x=X, y=Y, group = tokID, colour=Label))
    myPlotCart = myPlotCart + geom_line(aes(y=Y), alpha = 1, size=0.5) + 
      ylab("") + xlab("") + scale_color_brewer(type = "qual", palette = colour.palette) +
      facet_wrap(~ Label + Task)  + theme(legend.position="none") + theme(strip.text.x=element_text(size=30))
    print(myPlotCart)
    dev.off()
    
    Sys.sleep(0)
    
    # Grabs speaker's bite plane data
    bp.spk = bp.data[bp.data$SpkTask==spk.task.str,]
    
    # Calculates rotation angle (in radians)
    bp.len.sqd = sq(bp.spk[2,"Y"]-bp.spk[1,"Y"])+sq(bp.spk[2,"X"]-bp.spk[1,"X"])
    rot.angle = acos((sq(bp.spk[2,"R"])+bp.len.sqd-sq(bp.spk[1,"R"]))/(2*bp.spk[2,"R"]*sqrt(bp.len.sqd)))-asin(bp.spk[2,"Y"]/bp.spk[2,"R"])
    
    # Rotation of main with respect to bite plane
    spk.orig.data$Theta = spk.orig.data$Theta + rot.angle
    
    # Rotate the bite plane data too
    bp.spk$Theta = bp.spk$Theta + rot.angle
    bp.spk$X = rt.get.x(bp.spk)
    bp.spk$Y = rt.get.y(bp.spk)
    
    # Sanity check: plots the bite plane for this speaker 
    #library(plotrix)
    #polar.plot(bp.spk$"R", bp.spk$Theta*180/pi, labels="",rp.type="s",radial.lim=range(0,23))
    
    
    # Extraction of palate trace data
    pal.traces = spk.orig.data[spk.orig.data$Task=="pal",]
    
    
    
    # OPTIONAL FILTERING: Elimination of task-label combos having fewer than MIN.PTS.PER.RAY r-values. 
    
    EPSILON = 1.0*pi/180
    
    find.strong.rays = function(row) {
      row.Theta = as.numeric(row["Theta"])
      nrow(spk.orig.data[(spk.orig.data$Theta>row.Theta-EPSILON & 
                            spk.orig.data$Theta<row.Theta+EPSILON & 
                            spk.orig.data$Speaker==row["Speaker"] &
                            spk.orig.data$Label==row["Label"] 
      ),]) >= MIN.PTS.PER.RAY
    }
    
    strong.rays = apply(spk.orig.data,1,find.strong.rays)
    
    spk.filt = na.omit(spk.orig.data[strong.rays,])
    nrow(spk.filt)
    
    #################################################################
    # SSANOVA (in polar coordinates)
    #################################################################
    
    # Creation of the SSANOVA model
    spk.model <- ssanova(R ~ Label + Theta + Label:Theta, data=spk.filt)
    #summary(spk.model)
    
    # Generates predicted radius values for each Theta-ray
    spk.new.data <- expand.grid(Theta=seq(min(spk.filt$Theta), max(spk.filt$Theta), length.out=1000), 
                                Label=levels(spk.filt$Label), Task=levels(spk.filt$Task)) 
    spk.new.data$R <- predict(spk.model, newdata = spk.new.data, se = T)$fit
    
    # Generates associated standard errors for each Theta-ray 
    spk.new.data$SE<- predict(spk.model, newdata = spk.new.data, se = T)$se.fit 
    #head(spk.new.data)
    
    #################################################################
    # PLOTTING OF THE SSANOVA MODEL
    #################################################################
    
    # Conversion to Cartesian for plotting purposes
    spk.new.data$X = rt.get.x(spk.new.data)
    spk.new.data$Y = rt.get.y(spk.new.data)
    
    # Calculates Cartesian coordinates for the standard errors
    TWO.STD = 1.9545
    spk.new.data$SE.low.x = (spk.new.data$R - spk.new.data$SE*TWO.STD) * cos((spk.new.data$Theta))
    spk.new.data$SE.low.y = (spk.new.data$R - spk.new.data$SE*TWO.STD) * sin((spk.new.data$Theta))
    spk.new.data$SE.hi.x = (spk.new.data$R + spk.new.data$SE*TWO.STD) * cos((spk.new.data$Theta))
    spk.new.data$SE.hi.y = (spk.new.data$R + spk.new.data$SE*TWO.STD) * sin((spk.new.data$Theta))
    #head(spk.new.data)
    
    main.cons = spk.new.data[spk.new.data$Label %in% NEW.LABELS,]
    comp.cons = spk.new.data[spk.new.data$Label %in% COMP.CONS,]
    
    
    # Plots average contours for each label
    spk.graph = ggplot(main.cons, aes(x = X, colour = Label))
    spk.graph = spk.graph + geom_line(aes(y = Y), size=1.5, alpha=1) + 
      ylim(0,100) + xlim(-50,50) +
      scale_color_brewer(type = "qual", palette = colour.palette) + ylab("") + xlab("") +
      # Draws the SE range
      geom_line(aes(x=SE.hi.x, y = SE.hi.y), lty=3, alpha=1) + 
      geom_line(aes(x=SE.low.x, y = SE.low.y), lty=3, alpha=1) +
      # Draws the legend
      theme(legend.position=c(0.8, 0.3)) + theme(legend.text=element_text(size=20)) + 
      theme(legend.title=element_text(size=0)) 
    
    # Draws the comparison consonants + their SE range
    if(show.comp.cons) {
      spk.graph = spk.graph +
        geom_line(data=comp.cons, aes(x=X, y = Y), size=0.7, lty=1, alpha=0.5) +
        geom_line(data=comp.cons, aes(x=SE.hi.x, y = SE.hi.y), lty=3, alpha=0.5) +
        geom_line(data=comp.cons, aes(x=SE.low.x, y = SE.low.y), lty=3, alpha=0.5)
    }
    
    # Draws the palate trace
    if(show.pal) {
      spk.graph = spk.graph + geom_line(data=pal.traces,aes(x=X, y=Y), size=1,lty=1, alpha=1) 
  }
    
    # Draws the bite plane
    if(show.bp) {
      bp.spk[,c("Task","SE","SE.low.x","SE.low.y","SE.hi.x","SE.hi.y")] = factor(c(rep(NA,nrow(bp.spk))))
      bp.spk[,"Label"] = factor(c(rep("bite plane",nrow(bp.spk))))
      spk.graph = spk.graph + geom_line(data=bp.spk, aes(x=X, y=Y), size=1, lty=1,alpha=0.8) + 
        theme(legend.position=c(0.2, 0.3))
    }
    
    
    # Makes the plot and saves it to disk in the cwd
    out.file.nm = paste(spk.task.str,".png",sep="")
    png(filename=out.file.nm,width=out.width,height=out.height,res=out.res)
    print(spk.graph)
    dev.off()
    
    Sys.sleep(0)
    
  }



#################################################################
# MAIN METHOD
#################################################################

for (i in 2:8){
  
  speaker = paste("TP",i,sep="")
  my.tasks = levels(orig.data[orig.data$Speaker==speaker,"Task"])
  
  lapply(my.tasks[my.tasks!="pal"],function (task) do.one.spk(speaker,task))
}



