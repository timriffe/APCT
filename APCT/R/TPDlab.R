
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
##########################################################
# TPD 1  right angle
pdf(file.path("Figures","LabPres","TPD1.pdf"),height=5,width=8)
par(mai=c(.5,.5,.5,.5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1900:2000, col = "#A5A5A5", N = 20, chrono = FALSE, xpd=TRUE)
#
text(1900,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1900,2000,by=20),0,seq(1900,2000,by=20),pos=1,xpd=TRUE)
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1885,70,"Thano age",xpd=TRUE,pos=2,srt=90)
dev.off()

# TPD 2  right angle, with life lines
pdf(file.path("Figures","LabPres","TPD2.pdf"),height=5,width=8)
par(mai=c(.5,.5,.5,.5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2060, col = "#A5A5A5", N = 20, chrono = FALSE, xpd=TRUE)
#
text(1820,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE)

segments(c(1920-85,1930-75,1970-65,1997-75,1990-25,1995),
		c(85,75,65,75,25,50),
		c(1920,1930,1970,1997,1990,2045),
		c(0,0,0,0,0,0),col = "blue", lwd=2)
points(c(1920-85,1930-75,1970-65,1997-75,1990-25,1995),
		c(85,75,65,75,25,50),pch=19,col="blue",xpd=TRUE) # births
points(c(1920,1930,1970,1997,1990,2045),
		c(0,0,0,0,0,0), pch=13,cex=1.3,col="blue",lwd=2,xpd=TRUE)
rect(1820,0,1900,100,border=NA,col="#00000020")
rect(2000,0,2060,100,border=NA,col="#00000020")
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1805,70,"Thano age",xpd=TRUE,pos=2,srt=90)
text(1925,35,"Death cohort",srt=-45,col="blue")
dev.off()

# TPD 3 equilateral
pdf(file.path("Figures","LabPres","TPD3.pdf"),height=5,width=8)
par(mai=c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2060, col = "#A5A5A5", N = 20, chrono=FALSE, equilateral = TRUE, xpd=TRUE)
text(1820+seq(0,100,by=20)*.5,seq(0,100,by=20)*sqrt(3)/2,seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE)
# life lines
xb <- c(1920-85,1930-75,1970-65,1997-75,1990-25,1995)
yb <- c(85,75,65,75,25,50)
xd <- c(1920,1930,1970,1997,1990,2045)
yd <- c(0,0,0,0,0,0)

segments(xd,yd,
		xb + yb * .5,
		yb*sqrt(3)/2, col = "blue", lwd=2)

points(xb + yb * .5,
		yb*sqrt(3)/2,pch=19,col="blue",xpd=TRUE)
points(xd, yd, pch=13,cex=1.3,col="blue",lwd=2)
polygon(c(1820,1820+50,1900+50,1900),c(0,100*sqrt(3)/2,100*sqrt(3)/2,0),border=NA,col="#00000020")
polygon(c(2000,2000+50,2060+50,2060),c(0,100*sqrt(3)/2,100*sqrt(3)/2,0),border=NA,col="#00000020", xpd=TRUE)
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1805+35,70*sqrt(3)/2,"Thano age",xpd=TRUE,pos=2,srt=60)
text(1925+35/2,35*sqrt(3)/2,"Death cohort",srt=-60,col="blue")
dev.off()


