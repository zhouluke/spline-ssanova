
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


spkCart = read.table("BM-s-sh-cart.txt", sep="\t", header=TRUE)
spkCart$X = spkCart$X - ORIGIN.X
spkCart$Y = spkCart$Y - ORIGIN.Y
spkCart$tokID = paste(spkCart$Speaker,spkCart$Task,spkCart$Label,spkCart$TokNum,sep="-")
myPlotCart <- ggplot(spkCart, aes(x=X, y=Y, group = tokID, colour=Label))
myPlotCart + geom_line(aes(y=Y), alpha = 0.8, size=1) 

spkCart$r = sqrt(spkCart$X*spkCart$X + spkCart$Y*spkCart$Y)
spkCart$thetaTmp = NISTradianTOdeg(atan(spkCart$Y/(spkCart$X)))
spkCart$theta = ifelse(spkCart$X<0,180-abs(spkCart$thetaTmp),spkCart$thetaTmp)

# +  + ylab("") + xlab("") + scale_color_brewer(type = "qual", palette = "Spectral") +facet_wrap(~ word)  + theme(legend.position="none") + theme(strip.text.x=element_text(size=30))

#library(plotrix)
polar.plot(spkCart$r, spkCart$theta, labels="",rp.type="s",radial.lim=range(0,80))


spkCartModel <- ssanova(r ~ Label + theta + Label:theta, data=spkCart)
summary(spkCartModel)

spkNewData <- expand.grid(theta=seq(min(spkCart$theta), max(spkCart$theta), length.out=100), Label=levels(spkCart$Label)) 

# Generates predicted radius values (in polars!)
spkNewData$r <- predict(spkCartModel, newdata = spkNewData, se = T)$fit
# Generates associated standard errors (in polars!)
spkNewData$SE<- predict(spkCartModel, newdata = spkNewData, se = T)$se.fit 
head(spkNewData)

# Conversion to Cartesian for plotting purposes
spkNewData$X = spkNewData$r * cos(NISTdegTOradian(spkNewData$theta)) #* 123.472 + 123.472
spkNewData$Y = spkNewData$r * sin(NISTdegTOradian(spkNewData$theta)) #* 123.472 + 123.472

# Calculates Cartesian coordinates for the standard errors
spkNewData$SE.low.x = (spkNewData$r - spkNewData$SE*1.96) * cos(NISTdegTOradian(spkNewData$theta))
spkNewData$SE.low.y = (spkNewData$r - spkNewData$SE*1.96) * sin(NISTdegTOradian(spkNewData$theta))
spkNewData$SE.hi.x = (spkNewData$r + spkNewData$SE*1.96) * cos(NISTdegTOradian(spkNewData$theta))
spkNewData$SE.hi.y = (spkNewData$r + spkNewData$SE*1.96) * sin(NISTdegTOradian(spkNewData$theta))
head(spkNewData)

# Renames label values
spkNewData$Label = mapvalues(spkNewData$Label, from = c("s", "S", "x"), to = c("s", "ʃ", "ɕ"))
#c("[s]", "[ʃ]", "[ɕ]")

# Plots average contours
spkComp <- ggplot(spkNewData, aes(x = X, colour = Label))
spkComp + geom_line(aes(y = Y), size=2, alpha=1) + 
  scale_color_brewer(type = "qual", palette = "Spectral") + ylab("") + xlab("") + 
  geom_line(aes(x=SE.hi.x, y = SE.hi.y), lty=2, alpha=1) + 
  geom_line(aes(x=SE.low.x, y = SE.low.y), lty=2, alpha=1) +  
  theme(legend.position=c(0.12, 0.8)) + theme(legend.text=element_text(size=20)) + 
  theme(legend.title=element_text(size=0))
