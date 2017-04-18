
# Author: tim
###############################################################################
# install.packages("expm")
library(expm)
# Theorem 3 P^n <-> M^n for mixed point durations.
source("/home/tim/git/APCT/APCT/R/Timelines.R")

# make ugly generalized function
# A degree-n event-duration identity is identifiable from a set of n' events and m' durations
# if the graph formed by the set of n' and m' is a spanning tree of the edge-only graph of 
# said identity. That is, the graph is connected and includes each of the vertices. 

# This is the same as demanding that the minimal graph (durations as edges, events as nodes)
# is connected and includes at least one node explicitly (anchored).

# two dependencies copied from Timelines.R
GenerateTransformationMatrix <- function (n) {
	stopifnot(n>=2); n=n-1
	GenSubmatrx <- function (i) cbind(-diag(i), 1, matrix(0, ncol = n-i, nrow = i))
	do.call("rbind", lapply(1:n, "GenSubmatrx"))
}

# this produces all durations implied by a set of events, p
# including to-from vertices (not directed), as well as duration values.
# used as utility here and there.
DefaultDurationOrdering <- function(p){
	n       <- length(p)
	At      <- GenerateTransformationMatrix(n)
	# an inane way to get 
	tofromi <- t(apply(At, 1, function(x){
						c(which(x == -1),  which(x == 1))
					}))
	from    <- p[tofromi[,1]]
	to      <- p[tofromi[,2]]
	
	m 		<- length(to)
	#sum(cumsum(1:(n-1)))-n
	data.frame(d = 1:m,pfrom = tofromi[,1],pto=tofromi[,2],from = from, to = to, dur = to - from)
}

# this generates an adjacency matrix for the n+1 edge-only identity
np_adjacency <- function(n=4, edges=c("p1","p2","d1")){
	p       				<- rep(1, n)
	n1    					<- n + 1
	# get all durations
	durs  					<- DefaultDurationOrdering(p)
	dd 	  					<- paste0("d", durs$d)
	pp    					<- paste0("p", 1:n)
	# set of all durations and events, given with vertices in edge-only graph.
	edges.all  				<- data.frame(v1 = c(durs$pfrom, 1:n),
			                  			  v2 = c(durs$pto, rep(n1,n)))
	rownames(edges.all)   	<- c(dd,pp)
	
	edges.have            	<- edges.all[edges,]
	edges.have            	<- as.matrix(edges.have)
	# make an adjacency matrix
	adj 	              	<- matrix(0, ncol = n1, nrow = n1, dimnames = list(1:n1, 1:n1))   
	adj[edges.have] 		<- 1
	adj[edges.have[,c(2,1)]] <- 1
	
	# TR: 16-4-2017 OK, we only have 1s on diag 
	# for vertices that actually are touched...?
	vdiag <- rep(0,n1)
	vhave <- unique(c(edges.have))
	vdiag[vhave] <- 1
	adj                   	<- adj + diag(vdiag)
	
	# an extra object for orientation
	edges.all             	<- as.matrix(edges.all)
	A                     	<- adj * 0
	A[edges.all]          	<- rownames(edges.all)
	A[edges.all[,c(2,1)]] 	<- rownames(edges.all)
	diag(A) 				<- paste0("v",1:n1)
	
	list(adj=adj, edgeids = A)
}

# this function expands to the full set of identifiable edges
np_expand <- function(n=4, edges=c("p1","p2","d1")){
	require(expm)
	adjs 		<- np_adjacency(n, edges)
	adj 		<- adjs$adj
	ids 		<- adjs$edgeids
	
	# build out full coinnectivity by taking nth power
	# operator from expm package
	adji 		<- adj %^% n
	
	U 			<- upper.tri(adj)
	edges.identifiable <- adji > 0 & U
	
	list(edges.have = edges, 
		 edges.expanded = ids[edges.identifiable],
		 edges.all = ids[U])
} 

# this is the new function to determine if a set of events and durations (p_i and d_i) identifies an
# n-point identity. This works by defining the n+1-vertex graph consisting of all possible p_i and d_i,
# and then checking whether the specified set of edges connects the graph or not. This is done by 
# checking whether the symmetrix adjacency matrix is invertible or not. Returns TRUE or FALSE.

identifiable <- function(n=4,edges=c("p1","p2","d1")){
	adj <- np_adjacency(n, edges=edges)$adj
	# is this matrix invertible?
	determinant(adj,FALSE)$modulus != 0
}
identifiable2 <- function(n=4,edges=c("p1","p2","d1")){
	adj <- np_adjacency(n, edges=edges)$adj
	# is this matrix invertible?
	all(adj %^% n > 0)
}

identifiable(n=4, edges = c("p1","p4","d1","d3"))
identifiable2(n=4, edges = c("p1","p4","d1","d3"))

# --------------------
# test
#np_adjacency(n=3,  edges = c("p1","p2","d1"))

## APCTDL
#np_expand(n=3,  edges = c("p1","p2","d1"))   # APC
#np_expand( n = 3, edges = c("p1","p3","d1")) # APD
## a higher order identity, partial expansion
#np_expand( n = 4, edges = c("p1","p3","d1")) # APD
#
## APCTDL
#identifiable(3,  edges = c("p1","p2","d1"))  # APC
#identifiable(3, edges =  c("p1","p3","d1"))  # APD
#
#np_expand( n = 4, edges = c("p1","p3","d1")) # APD
#
#identifiable(n=5, edges =  c("p1","p3","d1"))  # APD

# would now like to generate the full set of possible spanning trees.
# got it.

# generate
generateSpanningTrees <- function(n=3){
	# generate all indices (all p will give it
	edges.fake  <- paste0("p",1:n)
	vs    		<- 1:(n+1)
	ids   		<- np_adjacency(n=n,  edges = edges.fake)$edgeids
	# n choose 2 combos of all edges yields all vertex pairs
	edges.all 	<- ids[t(combn(vs,2))]
	# then create all sets of n vertex pairs
	np1trees 	<- combn(edges.all,n)
	# given all sets of n vertex pairs (i.e. edges), which are identifiable?
	identified 	<- apply(np1trees,2,identifiable2,n=n)
	# select down
	as.list(as.data.frame(np1trees[, identified]))
}

# must be connected, use n edges, and touch all vertices.
#length(generateSpanningTrees(2)) # 3      # choose(ne(2),2)  3
#length(generateSpanningTrees(3)) # 16     # choose(ne(3),3)  20
#length(generateSpanningTrees(4)) # 125     # choose(ne(4),4)  210
#length(generateSpanningTrees(5)) # 1296    # choose(ne(5),5)  3003
#length(generateSpanningTrees(6)) # 15046  # choose(ne(6),6)  54264
#length(generateSpanningTrees(7)) # haven't run

#c(3,16,65,846,15046) / (2:6 + 1)

#ncombos <- function(n){
#	choose(ne(n),n)
#}
#n <-4
#(n+1)^(n-1)
# 2^1
# [1] 2
# > cat("Synch2377770871081\n");
#cumprod(1:6) - cumprod(0:5)
##           2, 3, 4,  5,  6
#n<- 2:6
#factorial(n)
#nspans <- c(3,16,65,846,15046)
#ncombos(2:6) # appears to approach 1/4
#plot(nspans / ncombos(2:6))
#ncombos(7) * .25 # my prediction
#ncombos(2:6) - nspans
#ne <- function(n) n+(n*(n-1)/2)
#
## it turns out Cayley's formula doesn't work for this...
#cayley <- function(n=3){
#	(n+1)^(n-1)
#}
#cayley(2:6) / nspans
#plot(c(3,16,65,846,15046),log='y')

# for n = 4, each edge appears 26 times in the solution set
# each edge pair appears 9 times
# each edge triad appears 4 times
# each edge quad appears once (n=4)
# in 65 solutions

# for n = 3, each dyad appears 3 times
# each edge appears 8 times
# in 16 solutions

#cayley(3)
#cayley(4)
#
## they're unique sets
#length(unique(unlist(lapply(n4,paste, collapse=""))))
# but are these connecting trees?

# TODO: add x and y shift
draw.tree <- function(n=4, edges, lprop = .5, x = 0, y = 0, add = FALSE, label = TRUE, col = NULL,...){
	p 			<- rep(1,n)
	n1    		<- n + 1
	# get coords for the n+1 vertices
	verts 		<- decide.verts(c(p,1))
	# get d1...dm
	durs  		<- DefaultDurationOrdering(p)
	# need comparable name, for selecting later
	durs$name 	<- paste0("d", durs$d)
	# remove unneeded columns
	durs$from 	<- NULL
	durs$to 	<- NULL
	durs$d 		<- NULL
	durs$dur 	<- NULL
	# make same for period measures, colnames must match
	pp 			<- data.frame(pfrom = 1:n, 
			                  pto = rep(n1,n), 
							  name = paste0("p",1:n))
	# join event and duration edges
	all.edges 	<- rbind(pp,durs)
	
	if (is.null(col)){
		col <- rep("black",nrow(all.edges))
	} 
	all.edges$col <- col
	# select those specified
	edges.draw 	<- all.edges[all.edges$name %in% edges, ]
	
	if (!add){
		# create empty device
		par(xpd=TRUE)
		plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
	}
	for (i in 1:nrow(edges.draw)){
		fr <- edges.draw$pfrom[i]
		to <- edges.draw$pto[i]
		d  <- to - fr
		lpropi <- ifelse(d == 1 | d == n, .5, lprop)
				
		x1 <- verts$x[fr] + x
		x2 <- verts$x[to] + x
		y1 <- verts$y[fr] + y
		y2 <- verts$y[to] + y
		segments(x1,y1,x2,y2,col=edges.draw$col[i],...)
		
		if (label){
			lx <- x1*lpropi+x2*(1-lpropi)
			ly <- y1*lpropi+y2*(1-lpropi)
			points(lx,ly,pch=22,cex=3.5,col=par("bg"),bg=par("bg"))
			text(lx,ly,edges.draw$name[i],col=gray(.5))
		}
	}

}


n4 <- lapply(generateSpanningTrees(4),sort)
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
