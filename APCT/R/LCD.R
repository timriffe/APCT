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


#library(LexisUtils)
source(file.path("R","Functions.R"))
# LCD  right angle, with life lines
pdf(file.path("Figures","LCD.pdf"),height=5,width=8)
par(mai = c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2060, col = "#A5A5A5", N = 20, chrono = FALSE, xpd=TRUE)
#
text(1820,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE)
# life lines
segments(c(1920-85,1930-75,1905,1922,1965,1995),c(0,0,0,0,0,0),
		c(1920-85,1930-75,1905,1922,1965,1995),
		c(85,75,65,75,25,50), col = "blue", lwd=2)
points(c(1920-85,1930-75,1905,1922,1965,1995),c(0,0,0,0,0,0),pch=19,col="blue",xpd=TRUE)
points(c(1920-85,1930-75,1905,1922,1965,1995),
		c(85,75,65,75,25,50), pch=13,cex=1.3,col="blue",lwd=2)
polygon(c(1820,1900,1820),c(0,0,80),border=NA,col="#00000020")
polygon(c(2000,2060,2060,1900),c(0,0,100,100),border=NA,col="#00000020")
text(1950,-10,"Birth Cohort",xpd=TRUE,pos=1)
text(1805,70,"Lifespan",xpd=TRUE,pos=2,srt=90)
text(1925,35,"Death cohort",srt=-45,col="blue")
dev.off()


