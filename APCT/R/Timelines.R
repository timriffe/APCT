
# Author: tim
###############################################################################

# function preamble. Graph creation is toggled off so
# I can source this with no other effects.

# utility functions snagged from YearsLost repo.
degrees2radians <- function(degrees){
	degrees * (pi / 180)
}

quarterArc <- function(x, y, radius = 1, fromDegrees = 180, ...){
	xx <- degrees2radians(seq(fromDegrees, fromDegrees + 90, by = .5))
	x <- cos(xx) * radius + x
	y <- sin(xx) * radius + y
	lines(x, y, ...)
}

curlyBrace1 <- function(xl, y, length = 5, radius1 = .5, radius2 = .25, top = TRUE, ...){  
	# i.e. the pointy part on top or on bottom?
	if (top){
		quarterArc(xl + radius1, y - radius1, radius = radius1, fromDegrees = 90, ...)
		quarterArc(xl + length - radius1, y - radius1 , radius = radius1, fromDegrees = 0, ...)
		quarterArc(xl + length / 2 - radius2, y + radius2, radius = radius2, fromDegrees = 270, ...)
		quarterArc(xl + length / 2 + radius2, y + radius2, radius = radius2, fromDegrees = 180, ...)
	} else {
		quarterArc(xl + radius1, y + radius1, radius = radius1, fromDegrees = 180, ...)
		quarterArc(xl + length - radius1, y + radius1 , radius = radius1, fromDegrees = 0 - 90, ...)
		quarterArc(xl + length / 2 - radius2, y - radius2, radius = radius2, fromDegrees = 270 + 90, ...)
		quarterArc(xl + length / 2 + radius2, y - radius2, radius = radius2, fromDegrees = 180 - 90, ...)        
	}
	segments(xl + radius1, y, xl + length / 2 - radius2, y, ...)
	segments(xl + length - radius1, y, xl + length / 2 + radius2, y, ...)   
}

# from JS, 27-3-2017
GenerateTransformationMatrix <- function (n) {
	stopifnot(n>=2)
	# BEGIN change Pancho 28.04.2017
	# n=n-1
	# GenSubmatrx <- function (i) cbind(-diag(i), 1, matrix(0, ncol = n-i, nrow = i))
	# do.call("rbind", lapply(1:n, "GenSubmatrx"))
	GenSubmatrx <- function (i) cbind(matrix(0, ncol = i-1, nrow = n-i), rep(-1, n-i), diag(n-i))
	do.call("rbind", lapply(1:(n-1), "GenSubmatrx"))
	# END change Pancho 28.04.2017
}
#n <- 4
#make.At(4)
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


draw.timeline <- function(p,ylim=c(-3,0)){
	durs <- DefaultDurationOrdering(p)
	par(mai=c(.1,.1,.1,.1),xpd=TRUE)
	plot(NULL, xlim = range(p), ylim = ylim, axes = FALSE, xlab = "", ylab = "",asp=1)
	segments(min(p),0,max(p),0,lwd=2,col = gray(.5))
#points(p,rep(0,4),cex=2,pch=16)
	segments(p,-.08,p,.08,lwd=4,col = "red")
	for (i in 1:length(p)){
		text(p[i],.08,substitute(p[x], list(x = i)),pos=3)
	}
	for (i in 1:nrow(durs)){
		x <- durs$from[i]
		l <- durs$dur[i]
		y <- -i*.5
		curlyBrace1(xl = x, y = y, length = l, top = FALSE, radius1 = .1, radius2 = .08)
		# BEGIN Pancho 28.04.2017
		text(x+l/2,y-.05,substitute(d[x], list(x = paste0(durs$pfrom[i], ",", durs$pto[i]))), pos=1)
		# text(x+l/2,y-.05,substitute(d[x], list(x = i)),pos=1)
		# END Pancho 28.04.2017
	}
	
}

decide.verts <- function(p){
	n      <- length(p)
	radint <- seq((2*pi),0,length=n+1)[-(n+1)] + pi / 2
	list(x=cos(radint),y=sin(radint))
}

star.timeline <- function(p,lprop=.5){
	n     <- length(p)
	verts <- decide.verts(p)
	durs  <- DefaultDurationOrdering(p)
	par(xpd=TRUE)
	plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
	
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
		# BEGIN Pancho 28.04.2017
		points(lx,ly,pch=22,cex=5,col="white",bg="white")
		text(lx,ly,substitute(d[x], list(x = paste0(fr, ",", to))))
		# points(lx,ly,pch=22,cex=3.5,col="white",bg="white")
		# text(lx,ly,substitute(d[x], list(x = i)))
		# END Pancho 28.04.2017
	}
	for (i in 1:length(p)){
		points(verts$x[i],verts$y[i],pch=21,cex=3.5,col="red",bg="white")
		text(verts$x[i],verts$y[i],substitute(p[x], list(x = i)))
	}
}

#p <- rep(1,4)

star.timeline.edges.only <- function(p,lprop=.5){
	n     <- length(p)
	n1    <- n + 1
	verts <- decide.verts(c(p,1))
	durs  <- DefaultDurationOrdering(p)
	par(xpd=TRUE)
	plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
	
	for (i in 1:n){
		x1 <- verts$x[n1]
		x2 <- verts$x[i]
		y1 <- verts$y[n1]
		y2 <- verts$y[i]
		d  <- abs(i-n1)
		lpropi <- ifelse(d == 1 | d == n, .5, lprop)
		
		segments(x1,y1,x2,y2,col="red")
		
		lx <- x1*lpropi+x2*(1-lpropi)
		ly <- y1*lpropi+y2*(1-lpropi)
		points(lx,ly,pch=21,cex=3.5,col="red",bg="white",lwd=.5)
		text(lx,ly,substitute(p[x], list(x = i)))
	}
	
	for (i in 1:nrow(durs)){
		fr <- durs$pfrom[i]
		to <- durs$pto[i]
		d  <- to - fr
		lpropi <- ifelse(d == 1 | d == n, .5, lprop)
		
		
		x1 <- verts$x[fr]
		x2 <- verts$x[to]
		y1 <- verts$y[fr]
		y2 <- verts$y[to]
		segments(x1,y1,x2,y2)
		
		lx <- x1*lpropi+x2*(1-lpropi)
		ly <- y1*lpropi+y2*(1-lpropi)
		# BEGIN Pancho 28.04.2017
		points(lx, ly, pch=22, cex=5,col="white",bg="white")
		text(lx,ly,substitute(d[x], list(x = paste0(fr, ",", to))))
		# points(lx,ly,pch=22,cex=3.5,col="white",bg="white")
		# text(lx,ly,substitute(d[x], list(x = i)))
		# END Pancho 28.04.2017
	}
	
}


make.graphs <- FALSE
if (make.graphs){
	from <- 0; to <- 6
	p2   <- seq(from=from,to=to,length=2)
	p3   <- seq(from=from,to=to,length=3)
	p4   <- seq(from=from,to=to,length=4)
	p5   <- seq(from=from,to=to,length=5)
	
# timelines
	draw.timeline(p2) 
	draw.timeline(p3) 
	draw.timeline(p4) 
	draw.timeline(p5) 
#draw.timeline(p5,c(-4,0)) 
	
# timeline graph
	star.timeline(p2,.5)
	star.timeline(p3,.5)
	star.timeline(p4,.4)
#star.timeline(p5,.5)
#star.timeline(runif(6),.4)
	
# temporal planes graph
	star.timeline.edges.only(p2)
	star.timeline.edges.only(p3,.4)
	star.timeline.edges.only(p4)
#star.timeline.edges.only(p5,.4)
	
# system command to crop pdf white space.
#pdfcrop filename.pdf filename.pdf
	
	outdir <-"/home/tim/git/APCT/APCT/Figures/TimeLinesAndGraphs"
	pdf(file.path(outdir,"linep2.pdf"),width=5,height=5)
	draw.timeline(p2) 
	dev.off()
	
	pdf(file.path(outdir,"linep3.pdf"),width=5,height=5)
	draw.timeline(p3) 
	dev.off()
	
	pdf(file.path(outdir,"linep4.pdf"),width=5,height=5)
	draw.timeline(p4) 
	dev.off()
	
	pdf(file.path(outdir,"linep5.pdf"),width=5,height=6)
	draw.timeline(p5,ylim=c(-4,0)) 
	dev.off()
	
	pdf(file.path(outdir,"starp2.pdf"),width=5,height=5)
	star.timeline(p2) 
	dev.off()
	
	pdf(file.path(outdir,"starp3.pdf"),width=5,height=5)
	star.timeline(p3) 
	dev.off()
	
	pdf(file.path(outdir,"starp4.pdf"),width=5,height=5)
	star.timeline(p4,.4) 
	dev.off()
	
	pdf(file.path(outdir,"edgep2.pdf"),width=5,height=5)
	star.timeline.edges.only(p2) 
	dev.off()
	
	pdf(file.path(outdir,"edgep3.pdf"),width=5,height=5)
	star.timeline.edges.only(p3,.4) 
	dev.off()
	
	pdf(file.path(outdir,"edgep4.pdf"),width=5,height=5)
	star.timeline.edges.only(p4) 
	dev.off()
	
	pdf(file.path(outdir,"edgep5.pdf"),width=7,height=7)
	star.timeline.edges.only(p5,.4) 
	dev.off()
#pdfcrop filename.pdf filename.pdf
	system(paste("pdfcrop",file.path(outdir,"linep2.pdf"),file.path(outdir,"linep2.pdf")))
	system(paste("pdfcrop",file.path(outdir,"linep3.pdf"),file.path(outdir,"linep3.pdf")))
	system(paste("pdfcrop",file.path(outdir,"linep4.pdf"),file.path(outdir,"linep4.pdf")))
	system(paste("pdfcrop",file.path(outdir,"linep5.pdf"),file.path(outdir,"linep5.pdf")))
	system(paste("pdfcrop",file.path(outdir,"starp2.pdf"),file.path(outdir,"starp2.pdf")))
	system(paste("pdfcrop",file.path(outdir,"starp3.pdf"),file.path(outdir,"starp3.pdf")))
	system(paste("pdfcrop",file.path(outdir,"starp4.pdf"),file.path(outdir,"starp4.pdf")))
	system(paste("pdfcrop",file.path(outdir,"edgep2.pdf"),file.path(outdir,"edgep2.pdf")))
	system(paste("pdfcrop",file.path(outdir,"edgep3.pdf"),file.path(outdir,"edgep3.pdf")))
	system(paste("pdfcrop",file.path(outdir,"edgep4.pdf"),file.path(outdir,"edgep4.pdf")))
	system(paste("pdfcrop",file.path(outdir,"edgep5.pdf"),file.path(outdir,"edgep5.pdf")))
	
} # end graph creation 





#p8 <- seq(0,10,length=8)
#star.timeline.edges.only(p8,.43) 