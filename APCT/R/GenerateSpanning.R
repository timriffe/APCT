
# Author: tim
###############################################################################

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

DefaultDurationOrdering <- function(p){
	n       <- length(p)
	At      <- GenerateTransformationMatrix(n)
	# ugly but works
	tofromi <- t(apply(At, 1, function(x){
						c(which(x == -1),  which(x == 1))
					}))
	from    <- p[tofromi[,1]]
	to      <- p[tofromi[,2]]
	
	m 		<- length(to)
	#sum(cumsum(1:(n-1)))-n
	data.frame(d = 1:m,pfrom = tofromi[,1],pto=tofromi[,2],from = from, to = to, dur = to - from)
}

# this is the new function to determine if a set of events and durations (p_i and d_i) identifies an
# n-point identity. This works by defining the n+1-vertex graph consisting of all possible p_i and d_i,
# and then checking whether the specified set of edges connects the graph or not. This is done by 
# checking whether the symmetrix adjacency matrix is invertible or not. Returns TRUE or FALSE.

identifiable <- function(n,edges=c("p1","p2","d1")){
	p       <- rep(1,n)
	n1    	<- n + 1
	# get all durations
	durs  	<- DefaultDurationOrdering(p)
	dd 	  	<- paste0("d",durs$d)
	pp    	<- paste0("p",1:n)
	# set of all durations and events, given with vertices in edge-only graph.
	edges.all  	<- data.frame(v1 = c(durs$pfrom,1:n),
			                  v2 = c(durs$pto,rep(n1,n)))
	rownames(edges.all) <- c(dd,pp)
	   
	edges.have <- edges.all[edges,]
	# make an adjacency matrix
	adj 	<- matrix(0,ncol=n1,nrow=n1,dimnames = list(1:n1,1:n1))   
	adj[cbind(edges.have$v1,edges.have$v2)] <- 1
	adj[cbind(edges.have$v2,edges.have$v1)] <- 1
	adj     <- adj + diag(n1)
	# is this matrix invertible?
	determinant(adj,FALSE)$modulus != 0
}
# test
identifiable(3, c("p1","p2","d1")) # APC
identifiable(3, c("p1","p3","d1")) # APD
