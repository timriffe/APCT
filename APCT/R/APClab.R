# this script uses the HMDresults object to search for common patterns to the various defined measures.
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

# APC 1 right angle. Go from 0 to 100 in jumps of 10 or 20.
pdf(file.path("Figures","LabPres","APC1.pdf"),height=5,width=8)
#graphics.off();dev.new(width=8,height=5)
par(mai=c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1900:2000, col = "#A5A5A5", N = 20, xpd=TRUE)
text(1900,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1900,2000,by=20),0,seq(1900,2000,by=20),pos=1,xpd=TRUE)
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1885,70,"Chrono age",xpd=TRUE,pos=2,srt=90)
dev.off()

# APC with life lines 2.
pdf(file.path("Figures","LabPres","APC2.pdf"),height=5,width=8)
par(mai=c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1900:2000, col = "#A5A5A5", N = 20, xpd=TRUE)
text(1900,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1900,2000,by=20),0,seq(1900,2000,by=20),pos=1,xpd=TRUE)
# life lines
segments(c(1900,1900,1905,1922,1965,1995),c(65,45,0,0,0,0),
		c(1900,1900,1905,1922,1965,1995)+c(20,30,65,75,25,5),
		c(65,45,0,0,0,0) + c(20,30,65,75,25,5), col = "blue", lwd=2)
points(c(1905,1922,1965,1995),rep(0,4),pch=19,col="blue",xpd=TRUE)
points(c(1900,1900,1905,1922,1965)+c(20,30,65,75,25),
		c(65,45,0,0,0) + c(20,30,65,75,25), pch=13,cex=1.3,col="blue",lwd=2)
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1885,70,"Chrono age",xpd=TRUE,pos=2,srt=90)
text(1930,35,"Birth cohort",srt=45,col="blue")
#rect(1820,0,1900,100,border=NA,col="#00000020")
dev.off()




# APC with life lines 3.
pdf(file.path("Figures","LabPres","APC3.pdf"),height=5,width=8)
par(mai=c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2000, col = "#A5A5A5", N = 20, xpd=TRUE)
text(1820,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2000,by=20),0,seq(1820,2000,by=20),pos=1,xpd=TRUE)
# life lines
segments(c(1920-85,1930-75,1905,1922,1965,1995),c(0,0,0,0,0,0),
		c(1900,1900,1905,1922,1965,1995)+c(20,30,65,75,25,5),
		c(65,45,0,0,0,0) + c(20,30,65,75,25,5), col = "blue", lwd=2)
points(c(1920-85,1930-75,1905,1922,1965,1995),rep(0,6),pch=19,col="blue",xpd=TRUE)
points(c(1900,1900,1905,1922,1965)+c(20,30,65,75,25),
		c(65,45,0,0,0) + c(20,30,65,75,25), pch=13,cex=1.3,col="blue",lwd=2)
rect(1820,0,1900,100,border=NA,col="#00000020")
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1805,70,"Chrono age",xpd=TRUE,pos=2,srt=90)
text(1930,35,"Birth cohort",srt=45,col="blue")
dev.off()

# APC with life lines 4.
pdf(file.path("Figures","LabPres","APC4.pdf"),height=5,width=8)
par(mai=c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2060, col = "#A5A5A5", N = 20, xpd=TRUE)
text(1820,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE)
# life lines
segments(c(1920-85,1930-75,1905,1922,1965,1995),c(0,0,0,0,0,0),
		c(1900,1900,1905,1922,1965,1995)+c(20,30,65,75,25,50),
		c(65,45,0,0,0,0) + c(20,30,65,75,25,50), col = "blue", lwd=2)
points(c(1920-85,1930-75,1905,1922,1965,1995),rep(0,6),pch=19,col="blue",xpd=TRUE)
points(c(1900,1900,1905,1922,1965,1995)+c(20,30,65,75,25,50),
		c(65,45,0,0,0,0) + c(20,30,65,75,25,50), pch=13,cex=1.3,col="blue",lwd=2)
rect(1820,0,1900,100,border=NA,col="#00000020")
rect(2000,0,2060,100,border=NA,col="#00000020")
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1805,70,"Chrono age",xpd=TRUE,pos=2,srt=90)
text(1930,35,"Birth cohort",srt=45,col="blue")
dev.off()

# APC 5, equilateral.

pdf(file.path("Figures","LabPres","APC5.pdf"),height=5,width=8)
par(mai=c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1770,2090), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
LexRefN(0:100, 1820:2060, col = "#A5A5A5", N = 20, equilateral = TRUE, xpd=TRUE)
text(1820-seq(0,100,by=20)*.5,seq(0,100,by=20)*sqrt(3)/2,seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE)
# life lines
x2 <- c(1900,1900,1905,1922,1965,1995)+c(20,30,65,75,25,50)
y2 <- c(65,45,0,0,0,0) + c(20,30,65,75,25,50)
segments(c(1920-85,1930-75,1905,1922,1965,1995),c(0,0,0,0,0,0),
		x2 - y2 * .5,
		y2*sqrt(3)/2, col = "blue", lwd=2)
points(c(1920-85,1930-75,1905,1922,1965,1995),rep(0,6),pch=19,col="blue",xpd=TRUE)
points(x2 - y2 * .5,
	   y2*sqrt(3)/2, pch=13,cex=1.3,col="blue",lwd=2)
polygon(c(1820,1820-50,1900-50,1900),c(0,100*sqrt(3)/2,100*sqrt(3)/2,0),border=NA,col="#00000020")
polygon(c(2000,2000-50,2060-50,2060),c(0,100*sqrt(3)/2,100*sqrt(3)/2,0),border=NA,col="#00000020")
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1805-15,30*sqrt(3)/2,"Chrono age",srt=300,xpd=TRUE,pos=2)
text(1930-35/2,35*sqrt(3)/2,"Birth cohort",srt=60,col="blue")
dev.off()

