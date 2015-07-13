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

# some lifelines, for general use
set.seed(1)
lifelines <- runif(10) * 100
# get dx for density
LT    <- readHMDweb("FRATNP","fltper_1x1",username = us, password = pw)
dx    <- LT$dx[LT$Year == 1970][1:101]
dx    <- dx / sum(dx)
scale <- 280
##############################

# a basic ATL, timeless
pdf("Figures/LabPres/ATL1.pdf",height=6,width=6)
par(mai = c(.7,.9,.1,.3), xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(0,100), ylim=c(0,100), asp = 1, axes = FALSE, xlab = "", ylab = "")
ATLRefN(0:100, N = 20, col = "#A5A5A5")
text(0,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,xpd=TRUE)
text(50,-5,"chronological age", pos = 1, xpd=TRUE)
text(-10,50,"thanatological age", pos = 1, srt=90, xpd=TRUE)
dev.off()

# add lifelines
pdf("Figures/LabPres/ATL2.pdf",height=6,width=6)
par(mai = c(.7,.9,.1,.3), xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(0,100), ylim=c(0,100), asp = 1, axes = FALSE, xlab = "", ylab = "")
ATLRefN(0:100, N = 20, col = "#A5A5A5")
text(0,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,xpd=TRUE)
text(50,-5,"chronological age", pos = 1, xpd=TRUE)
text(-10,50,"thanatological age", pos = 1, srt=90, xpd=TRUE)
segments(0,lifelines,lifelines,0,col="blue",lwd=2)
points(rep(0,10),lifelines, pch=19,col="blue",xpd=TRUE)
points(lifelines, rep(0,10), pch=13,cex=1.3,col="blue",xpd=TRUE)
dev.off()

##############
# add density on left
pdf("Figures/LabPres/ATL3.pdf",height=6,width=6)
par(mai = c(.7,.9,.1,.3), xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(0,100), ylim=c(0,100), asp = 1, axes = FALSE, xlab = "", ylab = "")
ATLRefN(0:100, N = 20, col = "#A5A5A5")
polygon(c(-dx * scale,rep(0,101)),c(0:100,100:0), xpd = TRUE, border = "#A5A5A5", col = "#FF000080")
#text(0,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
#text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,xpd=TRUE)
text(50,-5,"chronological age", pos = 1, xpd=TRUE)
text(-10,50,"thanatological age", pos = 1, srt=90, xpd=TRUE)
segments(0,lifelines,lifelines,0,col="blue",lwd=2)
points(rep(0,10),lifelines, pch=19,col="blue",xpd=TRUE)
points(lifelines, rep(0,10), pch=13,cex=1.3,col="blue",xpd=TRUE)
dev.off()

# add density on bottom
pdf("Figures/LabPres/ATL4.pdf",height=6,width=6)
par(mai = c(.7,.9,.1,.3), xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(0,100), ylim=c(0,100), asp = 1, axes = FALSE, xlab = "", ylab = "")
ATLRefN(0:100, N = 20, col = "#A5A5A5")
polygon(c(-dx * scale,rep(0,101)),c(0:100,100:0), xpd = TRUE, border = "#A5A5A5", col = "#FF000080")
polygon(c(0:100,100:0),c(-dx * scale,rep(0,101)), xpd = TRUE, border = "#A5A5A5", col = "#FF000080")
#text(0,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
#text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,xpd=TRUE)
text(50,-5,"chronological age", pos = 1, xpd=TRUE)
text(-10,50,"thanatological age", pos = 1, srt=90, xpd=TRUE)
segments(0,lifelines,lifelines,0,col="blue",lwd=2)
points(rep(0,10),lifelines, pch=19,col="blue",xpd=TRUE)
points(lifelines, rep(0,10), pch=13,cex=1.3,col="blue",xpd=TRUE)
dev.off()

# now back to basic: 
# a cohort slice:
pdf("Figures/LabPres/ATL5.pdf",height=6,width=6)
par(mai = c(.7,.9,.1,.3), xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(0,100), ylim=c(0,100), asp = 1, axes = FALSE, xlab = "", ylab = "")
ATLRefN(0:100, N = 20, col = "#A5A5A5")
segments(0,lifelines,lifelines,0,col="blue",lwd=2)
points(rep(0,10),lifelines, pch=19,col="blue",xpd=TRUE)
points(lifelines, rep(0,10), pch=13,cex=1.3,col="blue",xpd=TRUE)
text(0,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,xpd=TRUE)
text(50,-5,"chronological age", pos = 1, xpd=TRUE)
text(50,-5,"                                         = year", pos = 1, col="blue",xpd=TRUE)
text(-10,50,"thanatological age", pos = 1, srt=90, xpd=TRUE)
text(seq(0,100,by=20),-10,seq(1900,2000,by=20),pos=1,col="blue",xpd=TRUE)
dev.off()

# equilateral
pdf("Figures/LabPres/ATL6.pdf",height=6,width=6)
par(mai = c(.7,.9,.1,.3), xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(0,100), ylim=c(0,100), asp = 1, axes = FALSE, xlab = "", ylab = "")
ATLRefN(0:100, N = 20, col = "#A5A5A5", equilateral = TRUE)
segments(lifelines*.5,lifelines*sqrt(3)/2,lifelines,0,col="blue",lwd=2)
points(lifelines*.5,lifelines*sqrt(3)/2, pch=19,col="blue",xpd=TRUE)
points(lifelines, rep(0,10), pch=13,cex=1.3,col="blue",xpd=TRUE)
text(seq(0,100,by=20)*.5,seq(0,100,by=20)*sqrt(3)/2,seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,xpd=TRUE)
text(50,-5,"chronological age", pos = 1, xpd=TRUE)
text(50,-5,"                                         = year", pos = 1, col="blue",xpd=TRUE)
text(16,50,"thanatological age", pos = 1, srt=60, xpd=TRUE)
text(seq(0,100,by=20),-10,seq(1900,2000,by=20),pos=1,col="blue",xpd=TRUE)
dev.off()




