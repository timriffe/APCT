setwd("/home/tim/git/APCT/APCT/BFDW")
# Author: tim
###############################################################################
source("/home/tim/git/APCT/APCT/R/Functions.R")
library(plotrix)

make.tri <- function(e1,e2,e3){
	e1 <- toupper(e1)
	e2 <- toupper(e2)
	e3 <- toupper(e3)
	stopifnot(all(c(e1,e2,e3)%in% c("A","P","C","T","D","L")))
	par(mai=c(.3,.3,.3,.3))
	plot(NULL, xlim = c(0,1),ylim=c(0,1), asp=1,axes=FALSE, xlab="",ylab="")
	segments(1,0,.5,sqrt(3)/2,col=AssignColour(e1),lwd=3)
	segments(0,0,1,0,col=AssignColour(e2),lwd=3)
	segments(0,0,.5,sqrt(3)/2,col=AssignColour(e3),lwd=3)

	boxed.labels(.5,0,e2,col = AssignColour(e2),cex=3,font=2,border=FALSE)                          # South edge
	boxed.labels(.75,sqrt(3)/4,e1,col = AssignColour(e1),cex=3,font=2,border=FALSE)                 # NE edge
	boxed.labels(.25,sqrt(3)/4,e3,col = AssignColour(e3),cex=3,font=2,border=FALSE)                 # NW edge
   
	points(.5,sqrt(3)/2,col = "black",cex=2,pch=19)                                  # top vert
	points(1,0,col = "black",cex=2,pch=19)                                           # bottom right vert
	points(0,0,col = "black",cex=2,pch=19)                                           # bottom left vert
}

pdf("Figures/APCid.pdf",width=4,height=4)
make.tri("A","P","C") # APC
dev.off()

pdf("Figures/TALid.pdf",width=4,height=4)
make.tri("L","A","T") # TAL
dev.off()

pdf("Figures/TPDid.pdf",width=4,height=4)
make.tri("D","P","T") # TPD
dev.off()

pdf("Figures/LCDid.pdf",width=4,height=4)
make.tri("D","C","L")
dev.off()



# another vesion with vertices not numbered. save as pdf then convert to svg
pdf("Figures/TetraHedronEdgesOnly.pdf",width=4,height=4)
par(mai=c(.3,.3,.3,.3))
plot(NULL, xlim = c(0,1),ylim=c(0,1), asp=1,axes=FALSE, xlab="",ylab="")
segments(1,0,.5,sqrt(3)/2,lwd=2,col=AssignColour("A"))
segments(0,0,1,0,lwd=2,col=AssignColour("P"))
segments(0,0,.5,sqrt(3)/2,lwd=2,col=AssignColour("C"))
segments(0,0,.5,sqrt(3)/6,lwd=2,col=AssignColour("D"))
segments(1,0,.5,sqrt(3)/6,lwd=2,col=AssignColour("T"))
segments(.5,sqrt(3)/2,.5,sqrt(3)/6,lwd=2,col=AssignColour("L"))
boxed.labels(.5,0,"P",col = AssignColour("P"),cex=2,font=2,border=FALSE)                          # South edge
boxed.labels(.75,sqrt(3)/4,"A",col = AssignColour("A"),cex=2,font=2,border=FALSE)                 # NE edge
boxed.labels(.25,sqrt(3)/4,"C",col = AssignColour("C"),cex=2,font=2,border=FALSE)                 # NW edge
boxed.labels(.75,sqrt(3)/12,"T",col = AssignColour("T"),cex=2,font=2,border=FALSE)                # inner SE edge
boxed.labels(.25,sqrt(3)/12,"D",col = AssignColour("D"),cex=2,font=2,border=FALSE)                # inner SW edge
boxed.labels(.5, sqrt(3)/2-sqrt(3)/6,"L",col = AssignColour("L"),cex=2,font=2,border=FALSE)       # inner N edge
points(.5,sqrt(3)/2,col = "black",cex=2,pch=19)                                  # top vert
points(1,0,col = "black",cex=2,pch=19)                                           # bottom right vert
points(0,0,col = "black",cex=2,pch=19)                                           # bottom left vert
points(.5,sqrt(3)/6,col = "black",cex=2,pch=19)                                  # middle vert
dev.off()
