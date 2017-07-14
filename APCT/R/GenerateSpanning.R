
# Author: tim
###############################################################################
# install.packages("expm")
# Theorem 3 P^n <-> M^n for mixed point durations.
source("/home/tim/git/APCT/APCT/R/Timelines.R")

# -----------------------------------
#

n4 <- lapply(generateSpanningTrees(4), sort)
plot.order <- matrix(1:125,ncol=5)
xc         <- (col(plot.order) - 1) * 2.2 + 1
yc         <- (row(plot.order)-1) * 2.2 + 1
yc         <- abs(yc-max(yc)) + 1
pal10 <- c("#ffbaa8","#61df6a","#6a41a6","#c5d643",
		"#c1b5ff","#b69600","#00b2c1","#c70034","#98d9c7","#d36500")

pdf("/home/tim/git/APCT/APCT/Figures/n4spanningtrees.pdf",width=4,height=10)
par(xpd=TRUE,bg="#000000", xaxs="i",yaxs="i",mai=c(.5,.5,.5,.5))
plot(NULL,type='n',xlim=range(xc)+c(-1,1),ylim=range(yc)+c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
for (i in 1:length(n4)){
	draw.tree(4,n4[[i]],label=FALSE, add = TRUE,x=xc[i], y = yc[i], col = pal10, lwd = 2)
}
#draw.tree(4,c(paste0("d",1:6),paste0("p",1:4)),label=TRUE, add = TRUE,x=19, y = -1, col = pal10, lwd = 2)
dev.off()

pdf("/home/tim/git/APCT/APCT/Figures/n4spanningtreeskey.pdf",width=2.5,height=2.5)
par(xpd=TRUE,bg="#000000", xaxs="i",yaxs="i",mai=c(.2,.2,.2,.2))
draw.tree(4,c(paste0("d",1:6),paste0("p",1:4)),label=TRUE, col = pal10, lwd = 2)
dev.off()

# Figure tetraheron square:

draw.tree(4,n4[[i]],label=FALSE, add = TRUE,x=xc[i], y = yc[i], col = pal10, lwd = 2)



# TODO: add color to segments and vertices
timeline.graph <- function(n,dcol,vcol){
	p     <- rep(1,n)
	verts <- decide.verts(p)
	durs  <- DefaultDurationOrdering(p)
	par(xpd=TRUE)
	plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
# plot durations
	for (i in 1:nrow(durs)){
		fr <- durs$pfrom[i]
		to <- durs$pto[i]
		d  <- to - fr
		lpropi <- ifelse(d == 1 | d == (n - 1), .5, lprop)
		
		x1 <- verts$x[fr]
		x2 <- verts$x[to]
		y1 <- verts$y[fr]
		y2 <- verts$y[to]
		segments(x1,y1,x2,y2)
		
		lx <- x1*lpropi+x2*(1-lpropi)
		ly <- y1*lpropi+y2*(1-lpropi)
		points(lx,ly,pch=22,cex=3.5,col="white",bg="white")
		text(lx,ly,substitute(d[x], list(x = i)))
	}
# plot events
	for (i in 1:length(p)){
		points(verts$x[i],verts$y[i],pch=21,cex=3.5,col="red",bg="white")
		text(verts$x[i],verts$y[i],substitute(p[x], list(x = i)))
	}
}

# notes, each of the 10 edges appears 26 times.

#dev.new()
#star.timeline.edges.only(p=rep(1,4))
#locator(1)
# test d1,d3,p1,p3
#
#adj <- np_adjacency(n=4,edges= c("d1","d3","p1","p3"))
#c("d1","d3","p1","p3")
#adj$edgeids %in% c("d1","d3","p1","p3")
#adj$adj
##adj$adj[4,4] <- 0
##solve(adj$adj)
#
#np_expand( n = 4, edges = c("p1","p2","p3","p4"))
#
##install_github("jolars/qualpalr")
##library(devtools)
##install.packages("devtools")
#
## some colors from I want Hue
#
#
#

AssignColour <- function (x) {
	if (x == "A") result <- "#D23737"
	if (x == "P") result <- "#3191C9"
	if (x == "C") result <- "#D2BC2D"
	if (x == "T") result <- "#4EC93B"
	if (x == "D") result <- "#881F93"
	if (x == "L") result <- "#C5752B"
	return(result)
}
edges <- c("p1","p2","p3","d1","d2","d3")
names(edges) <- c("#D2BC2D","#3191C9","#881F93","#D23737","#C5752B","#4EC93B")
args(draw.tree)


star.timeline.edges.only(c(0.,.5,1),.4)
outdir <-"/home/tim/git/StAndrewsTalk/StAndrewsTalk/Figures"
pdf(file.path(outdir,"APCTDLsq.pdf"),width=5,height=5)
draw.tree(3,c("p1","p2","p3","d1","d2","d3"),label=TRUE, col = names(edges), lwd = 2,lprop=.4)
dev.off()
system(paste("pdfcrop",file.path(outdir,"APCTDLsq.pdf"),file.path(outdir,"APCTDLsq.pdf")))


# -------------------------------------------
# what about an animated gif of the n4 spanning trees?
#rads <- seq(0,pi*2,length=1000)
#par(xpd=TRUE,bg="#000000", xaxs="i",yaxs="i",mai=c(.1,.1,.1,.1))
#for (i in 1:length(n4)){
#	plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
#	lines(cos(rads),sin(rads))
#	draw.tree(4,n4[[1]],label=FALSE, add = TRUE,x=0, y = 0, col = pal10, lwd = 4)
#	Sys.sleep(.15)
#}
#
# unit circle:

# now have functions to determine if graphs are rotations of one another.
# there should be 25 patterns each w 5 rotations.
n4 <- generateSpanningTrees(4)
edge.group.names   <- names(n4)
edge.group.names.i <- edge.group.names
# container to put rotations in:
rotation.groups <- list()
n4i <- n4

for (i in 1:25){
	ref.group.name  <- edge.group.names.i[1]
	this.group      <- unlist(lapply(n4i, 
			                  is.rotated, 
							  n = 4, 
							  edges.ref = n4i[[ref.group.name]]))
	rotation.groups[[i]] <- edge.group.names.i[this.group]
	edge.group.names.i   <- edge.group.names.i[!this.group]
	n4i                  <- n4i[!this.group]
}
# now we want within-group sorting.
rotation.groups.sorted <- lapply(rotation.groups, function(x,n4){
			ref <- x[1]
			rots <- rep(0,5)
			for (i in 2:5){
				rots[i] <- nr.rotations(edges2=n4[[x[i]]],4,n4[[x[1]]])
			}
			x[order(rots)]
		},n4=n4)
# and now we need to both sort rows somehow and also shift the within-row
# based on some matching scheme. 
n4[rotation.groups.sorted[[1]]]

x1 <- n4[rotation.groups.sorted[[1]]]
x2 <- n4[rotation.groups.sorted[[2]]]

names(rotation.groups.sorted) <- paste0("R",1:25)

shift.row <- function(x,shift=1){
	stopifnot(shift %in% 0:4)
	if (shift == 0){
		return(x)
	}
	xn <- names(x)
	x[c(xn[-c(1:shift)],xn[1:shift])]
}
rowmatches <- function(x1,x2){
	c(sum(x1[[1]] %in% x2[[1]]),
			sum(x1[[2]] %in% x2[[2]]),
			sum(x1[[3]] %in% x2[[3]]),
			sum(x1[[4]] %in% x2[[4]])
	)
}

highestmatch <- function(x1,x2){
	max(
			c(sum(rowmatches(x1,shift.row(x2,0))),
	          sum(rowmatches(x1,shift.row(x2,1))),
	          sum(rowmatches(x1,shift.row(x2,2))),
	          sum(rowmatches(x1,shift.row(x2,3))),
	          sum(rowmatches(x1,shift.row(x2,4)))))
    
}

lapply(rotation.groups.sorted[-1], function(g2,g1,n4){
			x1 <- n4[g1]
			x2 <- n4[g2]
			highestmatch(x1,x2)
		},g1=rotation.groups.sorted[[1]], n4=n4)

highestmatch(x1,x2)




bestrowshift <- function(x1,x2){
	
}


par(xpd=TRUE,bg="#000000", xaxs="i",yaxs="i",mai=c(.1,.1,.1,.1))
for (i in 1:25){
	this.group <- rotation.groups.sorted[[i]]
	for (j in 1:5){
	    plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
	    lines(cos(rads),sin(rads))
	    draw.tree(4,n4[[this.group[j]]],label=FALSE, add = TRUE,x=0, y = 0, col = pal10, lwd = 4)
	    Sys.sleep(1)
    }
}

xs <- 0:4 * 2.2 + 1

ys <- 24:0 * 2.2 + 1

pal10 <- c("#ffbaa8","#61df6a","#6a41a6","#c5d643",
		"#c1b5ff","#b69600","#00b2c1","#c70034","#98d9c7","#d36500")

pdf("/home/tim/git/APCT/APCT/Figures/n4spanningtreesv2.pdf",width=4,height=10)
par(xpd=TRUE,bg="#000000", xaxs="i",yaxs="i",mai=c(.5,.5,.5,.5))
plot(NULL,type='n',xlim=range(xc)+c(-1,1),ylim=range(yc)+c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
for (i in 1:25){
	this.group <- rotation.groups.sorted[[i]]
	for (j in 1:5){
		draw.tree(4, n4[[this.group[j]]],
				label=FALSE, 
				add = TRUE,
				x=xs[j], y = ys[i], col = pal10, lwd = 2)
		
	}
}
dev.off()

#install.packages("animation")
library("animation")
setwd("/home/tim/workspace/Other")

saveGIF(
		{
			par(mai=c(0,0,0,0))
		
for (i in 1:25){
	this.group <- rotation.groups.sorted[[i]]
	for (j in 1:5){
		plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="",
				panel.first = list(rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
"black")))
		
		draw.tree(4, n4[[this.group[j]]],
				label=FALSE, 
				add = TRUE,
				x=0, y = 0, col = pal10, lwd = 6)
		#Sys.sleep(.3)
	}
}}, movie.name = "Figures/n4solutions.gif", img.name = "n4soli", convert = "convert", ani.width=560,ani.height=560)




