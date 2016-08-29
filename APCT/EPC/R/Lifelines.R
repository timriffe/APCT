
# Author: tim
###############################################################################

# some generic images of lifelines to indicate emographi time measures

source("/home/tim/git/APCT/APCT/R/Functions.R")
setwd("/home/tim/git/APCT/APCT/EPC")


l <- .5
r <- 9.5
h <- .5
A <- 5
gc <- gray(.6)
lwd1 <- 3.5
lwd2 <- 5
cex1 <- 3
cex2 <- 4.5

#graphics.off()

# Chronological Age
#dev.new(width=5,height=.5)
pdf("Figures/llA.pdf",width=5,height=.5)
par(xaxs="i",yaxs="i", mai = c(0,0,0,0))
plot(NULL,type="n",xlim=c(0,10),ylim=c(0,1), axes = FALSE, xlab = "", ylab = "")
segments(l,h,r,h, col = gc, lwd=lwd1)
points(l,h,pch=16,col=gc,cex=cex1)
points(r,h,pch=13,col=gc,cex=cex1, lwd=2)
arrows(l,h,A,h,lwd=lwd2,col=AssignColour("A"))
arrows(A,h,l,h,lwd=lwd2,col=AssignColour("A"))
dev.off()
# Thanatological Age
pdf("Figures/llT.pdf",width=5,height=.5)
par(xaxs="i",yaxs="i", mai = c(0,0,0,0))
plot(NULL,type="n",xlim=c(0,10),ylim=c(0,1), axes = FALSE, xlab = "", ylab = "")
segments(l,h,r,h, col = gc, lwd=lwd1)
points(l,h,pch=16,col=gc,cex=cex1)
points(r,h,pch=13,col=gc,cex=cex1, lwd=2)
arrows(r,h,A,h,lwd=lwd2,col=AssignColour("T"))
arrows(A,h,r,h,lwd=lwd2,col=AssignColour("T"))
dev.off()
# Lifespan
pdf("Figures/llL.pdf",width=5,height=.5)
par(xaxs="i",yaxs="i", mai = c(0,0,0,0))
plot(NULL,type="n",xlim=c(0,10),ylim=c(0,1), axes = FALSE, xlab = "", ylab = "")
points(l,h,pch=16,col=gc,cex=cex1)
points(r,h,pch=13,col=gc,cex=cex1, lwd=2)
segments(l,h,r,h, col = AssignColour("L"), lwd=lwd2)
arrows(r,h,l,h,lwd=lwd2,col=AssignColour("L"))
arrows(l,h,r,h,lwd=lwd2,col=AssignColour("L"))
dev.off()
# Birth cohort
pdf("Figures/llC.pdf",width=5,height=.5)
par(xaxs="i",yaxs="i", mai = c(0,0,0,0))
plot(NULL,type="n",xlim=c(0,10),ylim=c(0,1), axes = FALSE, xlab = "", ylab = "")
segments(l,h,r,h, col = gc, lwd=lwd1)
points(l,h,pch=16,col=AssignColour("C"),cex=cex2)
points(r,h,pch=13,col=gc,cex=cex1, lwd=cex1)
dev.off()
# Death cohort
pdf("Figures/llD.pdf",width=5,height=.5)
par(xaxs="i",yaxs="i", mai = c(0,0,0,0))
plot(NULL,type="n",xlim=c(0,10),ylim=c(0,1), axes = FALSE, xlab = "", ylab = "")
segments(l,h,r,h, col = gc, lwd=lwd1)
points(l,h,pch=16,col=gc,cex=cex1)
points(r,h,pch=13,col=AssignColour("D"),cex=cex2, lwd=lwd2)
dev.off()

AssignColour("P")
