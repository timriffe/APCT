
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
COMBS <- apply(COMBS,2,sort)
COMBS <- COMBS[,order(apply(COMBS,2,paste,collapse=""))]

as.data.frame(t(COMBS))

library(plotrix)

# Figure 1
pdf("Figures/TetraHedronVerticesEdges.pdf", height=4,width=5.5)
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

# Figure 2
pdf("Figures/tet4vert1.pdf", height=4,width=5.5)
par(mai=c(.3,.3,.3,.3))
plot(NULL, xlim = c(0,1),ylim=c(0,1), asp=1,axes=FALSE, xlab="",ylab="")
polygon(c(0,1,.5),c(0,0,sqrt(3)/2),lwd=2,border=gray(.7))
segments(0,0,.5,sqrt(3)/6,lwd=2,col=gray(.7))
segments(1,0,.5,sqrt(3)/6,lwd=2,col=gray(.7))
segments(.5,sqrt(3)/2,.5,sqrt(3)/6,lwd=3,col="black")
segments(0,0,.5,sqrt(3)/2,lwd=3,col="black")
segments(1,0,.5,sqrt(3)/2,lwd=3,col="black")

boxed.labels(.5,0,"P",col = "#0000FF50",cex=2,font=2,border=FALSE)                          # South edge
boxed.labels(.75,sqrt(3)/4,"A",col = "blue",cex=2,font=2,border=FALSE)                 # NE edge
boxed.labels(.25,sqrt(3)/4,"C",col = "blue",cex=2,font=2,border=FALSE)                 # NW edge
boxed.labels(.75,sqrt(3)/12,"T",col = "#0000FF50",cex=2,font=2,border=FALSE)                # inner SE edge
boxed.labels(.25,sqrt(3)/12,"D",col = "#0000FF50",cex=2,font=2,border=FALSE)                # inner SW edge
boxed.labels(.5, sqrt(3)/2-sqrt(3)/6,"L",col = "blue",cex=2,font=2,border=FALSE)       # inner N edge
text(.5,sqrt(3)/2,"1",col = "red",cex=2,font=2,pos=3)                                  # top vert
text(1,0,"2",col = "red",cex=2,font=2,pos=4)                                           # bottom right vert
text(0,0,"3",col = "red",cex=2,font=2,pos=2)                                           # bottom left vert
boxed.labels(.5,sqrt(3)/6,"4",col = "red",cex=2,font=2,border=FALSE)                   # middle vert
dev.off()

# table of figures: identity criteria. experimental.

tet4drawhexsegment <- function(index = "A", ...){
	index <- toupper(index)
	stopifnot(index %in% c("A", "P", "C", "T", "D", "L"))
	if (index == "A"){
		segments(1, 0, .5, sqrt(3)/2, ...) 
	}
	if (index == "P"){
		segments(0, 0, 1, 0, ...) 
	}
	if (index == "C"){
		segments(0, 0, .5, sqrt(3)/2, ...) 
	}
	if (index == "T"){
		segments(1, 0, .5, sqrt(3)/6, ...) 
	}
	if (index == "D"){
		segments(0, 0, .5, sqrt(3)/6, ...) 
	}
	if (index == "L"){
		segments(.5, sqrt(3)/2, .5, sqrt(3)/6, ...) 
	}
}
tet4drawhexlabel <- function(index = "A", ...){
	index <- toupper(index)
	stopifnot(index %in% c("A", "P", "C", "T", "D", "L"))
	if (index == "A"){
		boxed.labels(.75,sqrt(3)/4,"A",font=2,border=FALSE,...)
	}
	if (index == "P"){
		boxed.labels(.5,0,"P",font=2,border=FALSE,...) 
	}
	if (index == "C"){
		boxed.labels(.25,sqrt(3)/4,"C",font=2,border=FALSE,...)  
	}
	if (index == "T"){
		boxed.labels(.75,sqrt(3)/12,"T",font=2,border=FALSE,...)
	}
	if (index == "D"){
		boxed.labels(.25,sqrt(3)/12,"D",font=2,border=FALSE,...)  
	}
	if (index == "L"){
		boxed.labels(.5, sqrt(3)/2-sqrt(3)/6,"L",font=2,border=FALSE,...)  
	}
}
tet4drawcombo <- function(combo =c("A","P","C")){
	ids       <- c("A", "P", "C", "T", "D", "L")
	combo     <- toupper(combo)
	stopifnot(all(combo %in% ids))
	highlight <- ids %in% combo

	mapply( function(id,hi){
				if(hi){
					tet4drawhexsegment(id, col = "black", lwd = 3)
					#tet4drawhexlabel(id, col = "blue")
				} else {
					tet4drawhexsegment(id, col = gray(.9), lwd = 1)
				}
			},ids, highlight)
	points(c(0,1,.5,.5),c(0,0,sqrt(3)/2,sqrt(3)/6),pch=19,col="red",cex=1.2)
}

for (comb in 1:ncol(COMBS)){
	triad <- COMBS[,comb]
	name <- paste0("triad",paste(triad,collapse=""),".pdf")
	pdf(paste0("Figures/triadtable/",name), height=1.1,width=1.2)
	#dev.new(height=1,width=1.1)
	par(mai=c(0,0,0,0),yaxs="i",xaxs="i")
	plot(NULL, xlim = c(-.1,1.1),ylim=c(-.1,.9), asp=1,axes=FALSE, xlab="",ylab="")
	tet4drawcombo(triad)
	dev.off()
}

#
planes <- matrix(c("A","P","C","C","D","L","A","T","L","T","P","D"),ncol=3,byrow=TRUE)

for (comb in 1:nrow(planes)){
	triad <- planes[comb, ]
	name <- paste0("triad",paste(triad,collapse=""),".pdf")
	pdf(paste0("Figures/triadtable/",name), height=1.1,width=1.2)
	#dev.new(height=1,width=1.1)
	par(mai=c(0,0,0,0),yaxs="i",xaxs="i")
	plot(NULL, xlim = c(-.1,1.1),ylim=c(-.1,.9), asp=1,axes=FALSE, xlab="",ylab="")
	tet4drawcombo(triad)
	dev.off()
}
pdf(paste0("Figures/triadtable/","ATLC.pdf"), height=1.1,width=1.2)
#dev.new(height=1,width=1.1)
par(mai=c(0,0,0,0),yaxs="i",xaxs="i")
plot(NULL, xlim = c(-.1,1.1),ylim=c(-.1,.9), asp=1,axes=FALSE, xlab="",ylab="")
tet4drawcombo(c("A","T","L","C"))
dev.off()


# Figure 2
pdf("Figures/tet4vert1.pdf", height=4,width=5.5)
par(mai=c(.3,.3,.3,.3))
plot(NULL, xlim = c(0,1),ylim=c(0,1), asp=1,axes=FALSE, xlab="",ylab="")
polygon(c(0,1,.5),c(0,0,sqrt(3)/2),lwd=2,border=gray(.7))

segments(0,0,1,0,lwd=2,col=gray(.7)) # P
segments(0,0,.5,sqrt(3)/6,lwd=2,col=gray(.7)) # D
segments(1,0,.5,sqrt(3)/6,lwd=2,col=gray(.7)) # T
segments(.5,sqrt(3)/2,.5,sqrt(3)/6,lwd=3,col="black") # L
segments(0,0,.5,sqrt(3)/2,lwd=3,col="black") # C
segments(1,0,.5,sqrt(3)/2,lwd=3,col="black") # A
boxed.labels(.5,0,"P",col = "blue",cex=2,font=2,border=FALSE)                          # South edge
boxed.labels(.75,sqrt(3)/4,"A",col = "blue",cex=2,font=2,border=FALSE)                 # NE edge
boxed.labels(.25,sqrt(3)/4,"C",col = "blue",cex=2,font=2,border=FALSE)                 # NW edge
boxed.labels(.75,sqrt(3)/12,"T",col = "blue",cex=2,font=2,border=FALSE)                # inner SE edge
boxed.labels(.25,sqrt(3)/12,"D",col = "blue",cex=2,font=2,border=FALSE)                # inner SW edge
boxed.labels(.5, sqrt(3)/2-sqrt(3)/6,"L",col = "blue",cex=2,font=2,border=FALSE)   

text(.5,sqrt(3)/2,"1",col = "red",cex=2,font=2,pos=3)                                  # top vert
text(1,0,"2",col = "red",cex=2,font=2,pos=4)                                           # bottom right vert
text(0,0,"3",col = "red",cex=2,font=2,pos=2)                                           # bottom left vert
boxed.labels(.5,sqrt(3)/6,"4",col = "red",cex=2,font=2,border=FALSE)                   # middle vert
dev.off()






# appendices
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



#########################################
# make planes parallel to the edges?
# doesn't look so good in 2d. No parallax,
# then it covers up the depth, no?

par(mai=c(.3,.3,.3,.3),bg="lightcyan")
plot(NULL, xlim = c(-1,2),ylim=c(-1,2), asp=1,axes=FALSE, xlab="",ylab="")
abline(v=seq(-1,2,by=.5),col=gray(.8))
ai <- seq(-10*sqrt(3)/3,10*sqrt(3)/3,by=sqrt(3)/3)
for(a in ai){
	abline(a=a,b=2*sqrt(3)/2,col=gray(.8))
}
ai <- seq(-10*sqrt(3)/2,10*sqrt(3)/2,by=sqrt(3)/3)
for(a in ai){
	abline(a=a,b=2*sqrt(3)/6,col=gray(.8))
}
polygon(c(0,1,.5),c(0,0,sqrt(3)/2),lwd=2,border=gray(.5),col="#FFFFFF50")
segments(0,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(1,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(.5,sqrt(3)/2,.5,sqrt(3)/6,lwd=2,col=gray(.5))


###############
par(mai=c(.3,.3,.3,.3))
plot(NULL, xlim = c(-1,2),ylim=c(-1,2), asp=1,axes=FALSE, xlab="",ylab="")
polygon(c(0,1,.5),c(0,0,sqrt(3)/2),lwd=2,border=gray(.5))
segments(0,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(1,0,.5,sqrt(3)/6,lwd=2,col=gray(.5))
segments(.5,sqrt(3)/2,.5,sqrt(3)/6,lwd=2,col=gray(.5))
abline(v=seq(-1,2,by=.5),col=gray(.8))
ai <- seq(-10*sqrt(3)/3,10*sqrt(3)/3,by=sqrt(3)/3)
for(a in ai){
	abline(a=a,b=-2*sqrt(3)/2,col=gray(.8))
}
ai <- seq(-10*sqrt(3)/2,10*sqrt(3)/2,by=sqrt(3)/3)
for(a in ai){
	abline(a=a,b=-2*sqrt(3)/6,col=gray(.8))
}



#boxed.labels(.5,0,"P",col = "blue",cex=2,font=2,border=FALSE)                          # South edge
#boxed.labels(.75,sqrt(3)/4,"A",col = "blue",cex=2,font=2,border=FALSE)                 # NE edge
#boxed.labels(.25,sqrt(3)/4,"C",col = "blue",cex=2,font=2,border=FALSE)                 # NW edge
#boxed.labels(.75,sqrt(3)/12,"T",col = "blue",cex=2,font=2,border=FALSE)                # inner SE edge
#boxed.labels(.25,sqrt(3)/12,"D",col = "blue",cex=2,font=2,border=FALSE)                # inner SW edge
#boxed.labels(.5, sqrt(3)/2-sqrt(3)/6,"L",col = "blue",cex=2,font=2,border=FALSE)       # inner N edge
#text(.5,sqrt(3)/2,"1",col = "red",cex=2,font=2,pos=3)                                  # top vert
#text(1,0,"2",col = "red",cex=2,font=2,pos=4)                                           # bottom right vert
#text(0,0,"3",col = "red",cex=2,font=2,pos=2)                                           # bottom left vert
#boxed.labels(.5,sqrt(3)/6,"4",col = "red",cex=2,font=2,border=FALSE) 
#
#






