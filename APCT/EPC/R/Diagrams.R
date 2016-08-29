setwd("/home/tim/git/APCT/APCT/BFDW")

# Author: tim
###############################################################################
source("/home/tim/git/APCT/APCT/R/Functions.R")
llcol <- gray(.3)
##################################################
# APC with life lines 4.
##################################################
pdf(file.path("Figures","APCrt.pdf"),height=5,width=8)
par(mai=c(.5, .5, .5, .5), xaxs = "i", yaxs = "i", xpd=TRUE)
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2060, 
		col1 = muted(AssignColour("P"), l = 70, c = 50),
		col2 = muted(AssignColour("A"), l = 70, c = 50),
		col3 = muted(AssignColour("C"), l = 70, c = 50), N = 20, 
		xpd=TRUE,lty=1,lwd=1)
text(1820,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE,cex=.7,col=gray(.3))
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE,cex=.7,col=gray(.3))
segments(1820,0,1820,100,lwd=2,col=AssignColour("A"))
segments(1820,0,2060,0,lwd=2,col=AssignColour("P"))
# life lines
segments(c(1920-85,1930-75,1905,1922,1965,1995),c(0,0,0,0,0,0),
		c(1900,1900,1905,1922,1965,1995)+c(20,30,65,75,25,50),
		c(65,45,0,0,0,0) + c(20,30,65,75,25,50), col = llcol, lwd=2)
points(c(1920-85,1930-75,1905,1922,1965,1995),rep(0,6),pch=19,col=llcol,xpd=TRUE)
points(c(1900,1900,1905,1922,1965,1995)+c(20,30,65,75,25,50),
		c(65,45,0,0,0,0) + c(20,30,65,75,25,50), pch=13,cex=1.3,col=llcol,lwd=2)
#rect(1820,0,1900,100,border=NA,col="#00000020")
#rect(2000,0,2060,100,border=NA,col="#00000020")
text(1950,-10,"Period",xpd=TRUE,pos=1, cex = .8)
text(1805,80,"Chronological age",xpd=TRUE,pos=2,srt=90,cex=.8)
text(1930,35,"Birth cohort",srt=45,col="black",cex=.8)
dev.off()

######################################################
# LCD
######################################################

pdf(file.path("Figures","LCDrt.pdf"),height=5,width=8)

par(mai = c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2060, 
		col1 = muted(AssignColour("C"), l = 70, c = 50), 
		col2 = muted(AssignColour("L"), l = 70, c = 50), 
		col3 = muted(AssignColour("D"), l = 70, c = 50), 
		N = 20, chrono = FALSE, xpd=TRUE,lty=1,lwd=.5)
segments(1820,0,1820,100,lwd=2,col=AssignColour("L"))
segments(1820,0,2060,0,lwd=2,col=AssignColour("C"))
#
text(1820,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE)
# life lines
#segments(c(1920-85,1930-75,1905,1922,1965,1995),c(0,0,0,0,0,0),
#		c(1920-85,1930-75,1905,1922,1965,1995),
#		c(85,75,65,75,25,50), col = gray(.7), lwd=1.5)
#points(c(1920-85,1930-75,1905,1922,1965,1995),c(0,0,0,0,0,0),pch=19,col="black",xpd=TRUE)
points(c(1920-85,1930-75,1905,1922,1965,1995),
		c(85,75,65,75,25,50), pch=13,cex=1.3,col=llcol,lwd=2)
#polygon(c(1820,1900,1820),c(0,0,80),border=NA,col="#00000020")
#polygon(c(2000,2060,2060,1900),c(0,0,100,100),border=NA,col="#00000020")
text(1950,-10,"Birth Cohort",xpd=TRUE,pos=1)
text(1805,70,"Lifespan",xpd=TRUE,pos=2,srt=90)
text(1925,35,"Death cohort",srt=-45,col="black")
dev.off()

################################################
# TAL
################################################
lifelines <- c(85,75,65,75,25,50)
pdf("Figures/TALrt.pdf",height=6,width=6)
par(mai = c(.7,.9,.1,.3), xaxs="i",yaxs="i",xpd=TRUE)
plot(NULL, type = "n", xlim = c(0,100), ylim=c(0,100), asp = 1, axes = FALSE, xlab = "", ylab = "")
ATLRefN(0:100, N = 20, 
		col1 = muted(AssignColour("A"), l = 70, c = 50), 
		col2 = muted(AssignColour("T"), l = 70, c = 50), 
		col3 = muted(AssignColour("L"), l = 70, c = 50),
		lty=1,lwd=1.7)
segments(0,0,0,100,col=AssignColour("T"),lwd=3)
segments(0,0,100,0,col=AssignColour("A"),lwd=3)
text(0,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE,col=gray(.3))
text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,xpd=TRUE,col=gray(.3))
text(50,-5,"Chronological age", pos = 1, xpd=TRUE,cex=1.2)
text(-10,50,"Time to death", pos = 1, srt=90, xpd=TRUE,cex=1.2)
segments(0,lifelines,lifelines,0,col=llcol,lwd=3)
points(rep(0,length(lifelines)),lifelines, pch=19,col=llcol,xpd=TRUE,cex=1.5)
points(lifelines, rep(0,length(lifelines)), pch=13,cex=1.5,col=llcol,xpd=TRUE)
text(27,27,"Lifespan",srt=-45,col="black",cex=1.2)
dev.off()

#######################################################
# TPD
#######################################################
pdf(file.path("Figures","TPDrt.pdf"),height=5,width=8)
par(mai=c(.5,.5,.5,.5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2060, 
		col1 = muted(AssignColour("P"), l = 70, c = 50), 
		col2 = muted(AssignColour("T"), l = 70, c = 50),
		col3 = muted(AssignColour("D"), l = 70, c = 50),
		N = 20, chrono = FALSE, xpd=TRUE, lwd = .5, lty = 1)
segments(1820,0,1820,100,lwd=2,col=AssignColour("T"))
segments(1820,0,2060,0,lwd=2,col=AssignColour("P"))
#
text(1820,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE)

segments(c(1920-85,1930-75,1970-65,1997-75,1990-25,1995),
		c(85,75,65,75,25,50),
		c(1920,1930,1970,1997,1990,2045),
		c(0,0,0,0,0,0),col = llcol, lwd=2)
points(c(1920-85,1930-75,1970-65,1997-75,1990-25,1995),
		c(85,75,65,75,25,50),pch=19,col=llcol,xpd=TRUE) # births
points(c(1920,1930,1970,1997,1990,2045),
		c(0,0,0,0,0,0), pch=13,cex=1.3,col=llcol,lwd=2,xpd=TRUE)
#rect(1820,0,1900,100,border=NA,col="#00000020")
#rect(2000,0,2060,100,border=NA,col="#00000020")
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1805,80,"Thanatological age",xpd=TRUE,pos=2,srt=90)
text(1925,35,"Death cohort",srt=-45,col="black")
dev.off()
