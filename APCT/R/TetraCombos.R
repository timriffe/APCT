
###############################################################################
# what are the combis of the 6 perspectives that give 3d?

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
persps <- c("A","P","C","T","D","L")
COMBS <- combn(persps,3)
COMBS[,1]
redundant <- rep(FALSE, 20)
for (i in 2:20){
	redundant[i] <- all(COMBS[,1] %in% COMBS[,i])
}

# then there are 16 3d combos and 4 2d combos.

# 2d coords:
D2 <- matrix(c("A","P","C",
			   "C","D","L",
			   "A","T","L",
			   "T","P","D"),
		nrow=3,ncol=4)
for (i in 1:4){
	check <- D2[,i]
	for (p in 1:20){
		if  (all(check %in% COMBS[,p])){
			COMBS[,p] <- NA
		}
	}
}
COMBS <- COMBS[,colSums(is.na(COMBS))==0]
table(c(COMBS))
sort(apply(apply(COMBS,2,sort),2,paste,collapse=""))

as.data.frame(t(COMBS))

library(plotrix)

pdf("Figures/Tetra4.pdf", height=4,width=5.5)
par(mai=c(.3,.3,.3,.3))
plot(NULL, xlim = c(0,1),ylim=c(0,1), asp=1,axes=FALSE, xlab="",ylab="")
polygon(c(0,1,.5),c(0,0,sqrt(3)/2),lwd=2,border=gray(.5))
segments(0,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(1,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(.5,sqrt(3)/2,.5,sqrt(3)/6,lwd=2,col=gray(.5))
boxed.labels(.5,0,"P",col = "blue",cex=2,font=2,border=FALSE)                          # South edge
boxed.labels(.75,sqrt(3)/4,"A",col = "blue",cex=2,font=2,border=FALSE)                 # NE edge
boxed.labels(.25,sqrt(3)/4,"C",col = "blue",cex=2,font=2,border=FALSE)                 # NW edge
boxed.labels(.75,sqrt(3)/12,"T",col = "blue",cex=2,font=2,border=FALSE)                # inner SE edge
boxed.labels(.25,sqrt(3)/12,"D",col = "blue",cex=2,font=2,border=FALSE)                # inner SW edge
boxed.labels(.5, sqrt(3)/2-sqrt(3)/6,"L",col = "blue",cex=2,font=2,border=FALSE)       # inner N edge
text(.5,sqrt(3)/2,"1",col = "red",cex=2,font=2,pos=3)                                  # top vert
text(1,0,"2",col = "red",cex=2,font=2,pos=4)                                           # bottom right vert
text(0,0,"3",col = "red",cex=2,font=2,pos=2)                                           # bottom left vert
boxed.labels(.5,sqrt(3)/6,"4",col = "red",cex=2,font=2,border=FALSE)                   # middle vert
dev.off()



# 1 in the middle
pdf("Figures/Tetra1.pdf", height=4,width=5.5)
par(mai=c(.3,.3,.3,.3))
plot(NULL, xlim = c(0,1),ylim=c(0,1), asp=1,axes=FALSE, xlab="",ylab="")
polygon(c(0,1,.5),c(0,0,sqrt(3)/2),lwd=2,border=gray(.5))
segments(0,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(1,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(.5,sqrt(3)/2,.5,sqrt(3)/6,lwd=2,col=gray(.5))

boxed.labels(.5,0,"D",col = "blue",cex=2,font=2,border=FALSE)                          # South edge
boxed.labels(.75,sqrt(3)/4,"T",col = "blue",cex=2,font=2,border=FALSE)                 # NE edge
boxed.labels(.25,sqrt(3)/4,"P",col = "blue",cex=2,font=2,border=FALSE)                 # NW edge

boxed.labels(.75,sqrt(3)/12,"L",col = "blue",cex=2,font=2,border=FALSE)                # inner SE edge
boxed.labels(.25,sqrt(3)/12,"C",col = "blue",cex=2,font=2,border=FALSE)                # inner SW edge
boxed.labels(.5, sqrt(3)/2-sqrt(3)/6,"A",col = "blue",cex=2,font=2,border=FALSE)       # inner N edge

text(.5,sqrt(3)/2,"2",col = "red",cex=2,font=2,pos=3)                                  # top vert
text(1,0,"4",col = "red",cex=2,font=2,pos=4)                                           # bottom right vert
text(0,0,"3",col = "red",cex=2,font=2,pos=2)                                           # bottom left vert
boxed.labels(.5,sqrt(3)/6,"1",col = "red",cex=2,font=2,border=FALSE)                   # middle vert
dev.off()

# 2 in the middle
pdf("Figures/Tetra2.pdf", height=4,width=5.5)
par(mai=c(.3,.3,.3,.3))
plot(NULL, xlim = c(0,1),ylim=c(0,1), asp=1,axes=FALSE, xlab="",ylab="")
polygon(c(0,1,.5),c(0,0,sqrt(3)/2),lwd=2,border=gray(.5))
segments(0,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(1,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(.5,sqrt(3)/2,.5,sqrt(3)/6,lwd=2,col=gray(.5))

boxed.labels(.5,0,"D",col = "blue",cex=2,font=2,border=FALSE)                          # South edge
boxed.labels(.75,sqrt(3)/4,"C",col = "blue",cex=2,font=2,border=FALSE)                 # NE edge
boxed.labels(.25,sqrt(3)/4,"L",col = "blue",cex=2,font=2,border=FALSE)                 # NW edge

boxed.labels(.75,sqrt(3)/12,"P",col = "blue",cex=2,font=2,border=FALSE)                # inner SE edge
boxed.labels(.25,sqrt(3)/12,"T",col = "blue",cex=2,font=2,border=FALSE)                # inner SW edge
boxed.labels(.5, sqrt(3)/2-sqrt(3)/6,"A",col = "blue",cex=2,font=2,border=FALSE)       # inner N edge

text(.5,sqrt(3)/2,"1",col = "red",cex=2,font=2,pos=3)                                  # top vert
text(1,0,"3",col = "red",cex=2,font=2,pos=4)                                           # bottom right vert
text(0,0,"4",col = "red",cex=2,font=2,pos=2)                                           # bottom left vert
boxed.labels(.5,sqrt(3)/6,"2",col = "red",cex=2,font=2,border=FALSE)                   # middle vert
dev.off()

# 3 in the middle
pdf("Figures/Tetra3.pdf", height=4,width=5.5)
par(mai=c(.3,.3,.3,.3))
plot(NULL, xlim = c(0,1),ylim=c(0,1), asp=1,axes=FALSE, xlab="",ylab="")
polygon(c(0,1,.5),c(0,0,sqrt(3)/2),lwd=2,border=gray(.5))
segments(0,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(1,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(.5,sqrt(3)/2,.5,sqrt(3)/6,lwd=2,col=gray(.5))

boxed.labels(.5,0,"T",col = "blue",cex=2,font=2,border=FALSE)                          # South edge
boxed.labels(.75,sqrt(3)/4,"L",col = "blue",cex=2,font=2,border=FALSE)                 # NE edge
boxed.labels(.25,sqrt(3)/4,"A",col = "blue",cex=2,font=2,border=FALSE)                 # NW edge

boxed.labels(.75,sqrt(3)/12,"D",col = "blue",cex=2,font=2,border=FALSE)                # inner SE edge
boxed.labels(.25,sqrt(3)/12,"P",col = "blue",cex=2,font=2,border=FALSE)                # inner SW edge
boxed.labels(.5, sqrt(3)/2-sqrt(3)/6,"C",col = "blue",cex=2,font=2,border=FALSE)       # inner N edge

text(.5,sqrt(3)/2,"1",col = "red",cex=2,font=2,pos=3)                                  # top vert
text(1,0,"4",col = "red",cex=2,font=2,pos=4)                                           # bottom right vert
text(0,0,"2",col = "red",cex=2,font=2,pos=2)                                           # bottom left vert
boxed.labels(.5,sqrt(3)/6,"3",col = "red",cex=2,font=2,border=FALSE)                   # middle vert
dev.off()














