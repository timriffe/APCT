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

lifelines <- c(85,75,65,75,25,50)
# get dx for density
#LT    <- readHMDweb("FRATNP","fltper_1x1",username = us, password = pw)
#dx    <- LT$dx[LT$Year == 1970][1:101]
#dx    <- dx / sum(dx)
#scale <- 280
##############################
llcol <- gray(.3)

# TAL, timeless, with lifelines
pdf("Figures/TALrt.pdf",height=6,width=6)
par(mai = c(.7,.9,.1,.3), xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(0,100), ylim=c(0,100), asp = 1, axes = FALSE, xlab = "", ylab = "")
ATLRefN(0:100, N = 20, 
		col1 = muted(AssignColour("A"), l = 70, c = 50), 
		col2 = muted(AssignColour("T"), l = 70, c = 50), 
		col3 = muted(AssignColour("L"), l = 70, c = 50),
		lty=1,lwd=.5)
segments(0,0,0,100,col=AssignColour("T"),lwd=2)
segments(0,0,100,0,col=AssignColour("A"),lwd=2)
text(0,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,xpd=TRUE)
text(50,-5,"Chronological age", pos = 1, xpd=TRUE)
text(-10,50,"Thanatological age", pos = 1, srt=90, xpd=TRUE)

segments(0,lifelines,lifelines,0,col=llcol,lwd=2)
points(rep(0,length(lifelines)),lifelines, pch=19,col=llcol,xpd=TRUE)
points(lifelines, rep(0,length(lifelines)), pch=13,cex=1.3,col=llcol,xpd=TRUE)
text(27,27,"Lifespan",srt=-45,col="black")
dev.off()




# equilateral
pdf("Figures/TALeq.pdf",height=6,width=6)
par(mai = c(.7,.9,.1,.3), xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(0,100), ylim=c(0,100), asp = 1, axes = FALSE, xlab = "", ylab = "")
ATLRefN(0:100, N = 20, 
		col1 = muted(AssignColour("L"), l = 70, c = 50), 
		col2 = muted(AssignColour("A"), l = 70, c = 50), 
		col3 = muted(AssignColour("T"), l = 70, c = 50),
		lty=1,lwd=.5, equilateral = TRUE)
segments(0,0,50,100*sqrt(3)/2,col=AssignColour("T"),lwd=2)
segments(0,0,100,0,col=AssignColour("A"),lwd=2)
segments(lifelines*.5,lifelines*sqrt(3)/2,lifelines,0,col=llcol,lwd=2)
points(lifelines*.5,lifelines*sqrt(3)/2, pch=19,col=llcol,xpd=TRUE)
points(lifelines, rep(0,length(lifelines)), pch=13,cex=1.3,col=llcol,xpd=TRUE)
text(seq(0,100,by=20)*.5,seq(0,100,by=20)*sqrt(3)/2,seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(0,100,by=20),0,seq(0,100,by=20),pos=1,xpd=TRUE)
text(50,-5,"Chronological age", pos = 1, xpd=TRUE)
#text(50,-5,"                                         = year", pos = 1, col="blue",xpd=TRUE)
text(16,50,"Thanatological age", pos = 1, srt=60, xpd=TRUE)
#text(seq(0,100,by=20),-10,seq(1900,2000,by=20),pos=1,col="blue",xpd=TRUE)
text(27+13.5,27*sqrt(3)/2,"Lifespan",srt=-60,col="black")
dev.off()

############################################################3
# TAL presentation:

pdf("PAApresentation/Figures/TALrt.pdf",height=6,width=6)
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
llcol <- gray(.3)
segments(0,lifelines,lifelines,0,col=llcol,lwd=3)
points(rep(0,length(lifelines)),lifelines, pch=19,col=llcol,xpd=TRUE,cex=1.5)
points(lifelines, rep(0,length(lifelines)), pch=13,cex=1.5,col=llcol,xpd=TRUE)
text(27,27,"Lifespan",srt=-45,col="black",cex=1.2)
dev.off()

