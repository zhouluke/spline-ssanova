
# Based on the scripts by Jennifer Nycz and Shuo Zhang

#Installs gss 
#install.packages("gss")
#install.packages("NISTunits", dependencies = TRUE)

#load libraries
library(ggplot2)
library(gss)
library(NISTunits)

setwd("/home/luke/Dropbox/LIN1290/spline-ssanova")

ORIGIN.X = 123.472
ORIGIN.Y = 0

# spk = read.table("BM-s-sh.txt", sep="\t", header=TRUE)
# spk$X = spk$r * cos(NISTdegTOradian(spk$theta+90)) #* 123.472 + 123.472
# spk$Y = spk$r * sin(NISTdegTOradian(spk$theta+90)) #* 123.472 + 123.472
# spk$tokID = paste(spk$Speaker,spk$Task,spk$Label,spk$AAA.ID,sep="-")
# summary(spk)
# 
# myPlot <- ggplot(spk, aes(x=X*(-1), y=Y, group = tokID, colour=Label))
# myPlot + geom_line(aes(y=Y), alpha = 0.8, size=1)



spkCart = read.table("BM-s-sh-cart.txt", sep="\t", header=TRUE)
spkCart$X = spkCart$X - ORIGIN.X
spkCart$Y = spkCart$Y - ORIGIN.Y
spkCart$tokID = paste(spkCart$Speaker,spkCart$Task,spkCart$Label,spkCart$TokNum,sep="-")
myPlotCart <- ggplot(spkCart, aes(x=X, y=Y, group = tokID, colour=Label))
myPlotCart + geom_line(aes(y=Y), alpha = 0.8, size=1) 

spkCart$r = sqrt(spkCart$X*spkCart$X + spkCart$Y*spkCart$Y)
spkCart$theta = NISTradianTOdeg(atan(spkCart$Y/(spkCart$X)))
spkCart$theta2 = ifelse(spkCart$X<0,180-abs(spkCart$theta),spkCart$theta)

# +  + ylab("") + xlab("") + scale_color_brewer(type = "qual", palette = "Spectral") +facet_wrap(~ word)  + theme(legend.position="none") + theme(strip.text.x=element_text(size=30))

#library(plotrix)
polar.plot(spkCart$r, spkCart$theta2, labels="",rp.type="s",radial.lim=range(0,80))


spkCartModel <- ssanova(r ~ Label + theta + label:theta, data=spkCart)
