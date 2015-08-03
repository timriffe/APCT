
###############################################################################

# what are the combis of the 6 perspectives that give 3d?

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

as.data.frame(t(COMBS))


pdf("")
plot(NULL, xlim = c(0,1),ylim=c(0,1), asp=1,axes=FALSE, xlab="",ylab="")
polygon(c(0,1,.5),c(0,0,sqrt(3)/2))
segments(0,0,.5,sqrt(3)/6)
segments(1,0,.5,sqrt(3)/6)
segments(.5,sqrt(3)/2,.5,sqrt(3)/6)
text(.5,0,"P",col = "blue",cex=2)
text(.75,sqrt(3)/4,"A",col = "blue",cex=2)
text(.25,sqrt(3)/4,"C",col = "blue",cex=2)
text(.75,sqrt(3)/12,"T",col = "blue",cex=2)
text(.25,sqrt(3)/12,"D",col = "blue",cex=2)
text(.5, sqrt(3)/2-sqrt(3)/6,"L",col = "blue",cex=2)


