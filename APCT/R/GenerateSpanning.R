
# Author: tim
###############################################################################
# install.packages("expm")
library(expm)
# Theorem 3 P^n <-> M^n for mixed point durations.


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


# test
np_adjacency(n=3,  edges = c("p1","p2","d1"))

# APCTDL
np_expand(n=3,  edges = c("p1","p2","d1"))   # APC
np_expand( n = 3, edges = c("p1","p3","d1")) # APD
# a higher order identity, partial expansion
np_expand( n = 4, edges = c("p1","p3","d1")) # APD

# APCTDL
identifiable(3,  edges = c("p1","p2","d1"))  # APC
identifiable(3, edges =  c("p1","p3","d1"))  # APD

np_expand( n = 4, edges = c("p1","p3","d1")) # APD

identifiable(n=5, edges =  c("p1","p3","d1"))  # APD


# would now like to generate the full set of possible spanning trees.
vs <- 1:4
ids   <- np_adjacency(n=3,  edges = c("p1","p2","d1"))$edgeids
ids[t(combn(vs,2))]

# got it.
apply(combn(ids[t(combn(vs,2))],3),2,identifiable,n=3)

# generate
generateSpanningTrees <- function(n=3){
	edges.fake  <- paste0("p",1:n)
	vs    		<- 1:(n+1)
	ids   		<- np_adjacency(n=n,  edges = edges.fake)$edgeids
	edges.all 	<- ids[t(combn(vs,2))]
	np1trees 	<- combn(edges.all,n)
	identified 	<- apply(np1trees,2,identifiable,n=n)
	as.list(as.data.frame(np1trees[, identified]))
}
#length(generateSpanningTrees(2))
#length(generateSpanningTrees(3))
#length(generateSpanningTrees(4))
#length(generateSpanningTrees(5))


# it turns out Cayley's formula doesn't work for this...
#cayley <- function(n=3){
#	(n+1)^(n-1)
#}
#cayley(3)
#cayley(4)
#
n4 <- lapply(generateSpanningTrees(4),sort)
## they're unique sets
#length(unique(unlist(lapply(n4,paste, collapse=""))))
# but are these connecting trees?
edges <- n4[[1]]

# TODO: add x and y shift
draw.tree <- function(n=4, edges, lprop = .5, x = 0, y = 0, add = FALSE, label = TRUE){
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
	p 			<- data.frame(pfrom = 1:n, 
			                  pto = rep(n1,n), 
							  name = paste0("p",1:n))
	# join event and duration edges
	all.edges 	<- rbind(p,durs)
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
		segments(x1,y1,x2,y2)
		
		if (label){
			lx <- x1*lpropi+x2*(1-lpropi)
			ly <- y1*lpropi+y2*(1-lpropi)
			points(lx,ly,pch=22,cex=3.5,col="white",bg="white")
			text(lx,ly,edges.draw$name[i])
		}
	}

}

draw.tree(4,n4[[2]])

plot.order <- matrix(1:72,ncol=9)
xc         <- (col(plot.order) - 1) * 2.2 + 1
yc         <- (row(plot.order)-1) * 2.2 + 1
yc         <- abs(yc-max(yc)) + 1

par(xpd=TRUE)
plot(NULL,type='n',xlim=range(xc)+c(-1,1),ylim=range(yc)+c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
for (i in 1:length(n4)){
	draw.tree(4,n4[[i]],label=FALSE, add = TRUE,x=xc[i], y = yc[i])
}

dev.new()
star.timeline.edges.only(p=rep(1,4))

# test d1,d3,p1,p3

adj <- np_adjacency(n=4,edges= c("d1","d3","p1","p3"))
c("d1","d3","p1","p3")
adj$edgeids %in% c("d1","d3","p1","p3")
adj$adj
#adj$adj[4,4] <- 0
#solve(adj$adj)