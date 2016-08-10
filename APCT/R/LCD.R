# Author: tim
###############################################################################

# LCD, Lifespan, Birth Cohort, Death Cohort. crayzay
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/APCT/APCT")
} else {
	if (system("hostname",intern=TRUE) == "PC-403478"){
		# on MPIDR PC
		setwd("U://git//APCT//APCT")
	} else {
		# in that case I'm on Berkeley system, and other people in the dept can run this too
		setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/APCT/APCT"))
	}
}
getwd()

library(scales)

#library(LexisUtils)
source(file.path("R","Functions.R"))
# LCD  right angle, with life lines
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
		c(85,75,65,75,25,50), pch=13,cex=1.3,col="black",lwd=2)
#polygon(c(1820,1900,1820),c(0,0,80),border=NA,col="#00000020")
#polygon(c(2000,2060,2060,1900),c(0,0,100,100),border=NA,col="#00000020")
text(1950,-10,"Birth Cohort",xpd=TRUE,pos=1)
text(1805,70,"Lifespan",xpd=TRUE,pos=2,srt=90)
text(1925,35,"Death cohort",srt=-45,col="black")
dev.off()

# isotropic
pdf(file.path("Figures","LCDeq.pdf"),height=5,width=8)
par(mai = c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2060, 
		col1 = muted(AssignColour("C"), l = 70, c = 50), 
		col2 = muted(AssignColour("L"), l = 70, c = 50), 
		col3 = muted(AssignColour("D"), l = 70, c = 50), 
		N = 20, chrono = FALSE, xpd=TRUE,lty=1,lwd=.5,equilateral=TRUE)
segments(1820,0,1820+50,100*sqrt(3)/2,lwd=2,col=AssignColour("L"))
segments(1820,0,2060,0,lwd=2,col=AssignColour("C"))
#
text(1820+seq(0,100,by=20)*.5,seq(0,100,by=20)*sqrt(3)/2,seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE)
# life lines

xc <- c(1920-85,1930-75,1905,1922,1965,1995)
yc <- c(0,0,0,0,0,0)
xd <- c(1920-85,1930-75,1905,1922,1965,1995)
yd <- c(85,75,65,75,25,50)

#segments(xc,yc,xc+.5*yd,yd*sqrt(3)/2, col = gray(.7), lwd=1.5)
#points(xc,yc,pch=19,col="black",xpd=TRUE)
points(xc+.5*yd,yd*sqrt(3)/2, pch=13,cex=1.3,col="black",lwd=2)
#polygon(c(1820,1900,1820),c(0,0,80),border=NA,col="#00000020")
#polygon(c(2000,2060,2060,1900),c(0,0,100,100),border=NA,col="#00000020")
text(1950,-10,"Birth Cohort",xpd=TRUE,pos=1)
text(1805+35,80*sqrt(3)/2,"Lifespan",xpd=TRUE,pos=2,srt=60)
text(1925+35/2,35*sqrt(3)/2,"Death cohort",srt=-60,col="black")
dev.off()


