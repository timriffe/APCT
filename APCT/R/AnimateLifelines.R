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


N         <- 12
maxL      <- 100
steps     <- 1

lifespans <- runif(N) * maxL

y         <- 1:N
xl        <- rep(0,N)
xr        <- lifespans

gap       <- maxL - lifespans
moves     <- 0:maxL

XL <- outer(xl,moves,"+")
XR <- outer(xr,moves,"+")

Nframes <- min(which(colSums(XR > maxL) == N))
XR <- XR[,1:Nframes]
XL <- XL[,1:Nframes]

XR[XR > maxL] <- maxL

GAP <- matrix(gap,nrow=N,ncol=Nframes)
dim(XL) == dim(GAP)
XL[XL > GAP]  <- GAP[XL > GAP]

for (i in 1:Nframes){
	name <- paste0("frame_",i,".pdf")
	pdf(file.path("Figures","LabPres","AnimateLifeLines",name),width=4,height=4)
    plot(NULL, type = "n", xlim = c(0,100), ylim = c(0,N), axes = FALSE, xlab = "", ylab = "")
    segments(XL[,i],y,XR[,i],y,lwd=2,col = "blue")
	points(XL[,i],y,pch=19,col = "blue")
	points(XR[,i],y,pch=13,col = "blue",cex=1.3)
    dev.off()
}


#pdf(file.path("Figures","LabPres","AnimateLifeLines","coverframe"),width=4,height=4)
#plot(NULL, type = "n", xlim = c(0,100), ylim = c(0,N), axes = FALSE, xlab = "", ylab = "")
#segments(XL[,1],y,XR[,1],y,lwd=2,col = "blue")
#points(XL[,1],y,pch=19,col = "blue")
#points(XR[,1],y,pch=13,col = "blue",cex=1.3)
#dev.off()


