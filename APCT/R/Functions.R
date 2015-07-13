
# Author: tim
###############################################################################

if (!"HMDHFDplus" %in% rownames(installed.packages())){
	devtools::install_github("timriffe/TR1/TR1/HMDHFDplus")
}
require(HMDHFDplus)


#'
#' @title LexRefN Overlays semi-transparent N-year lexis triangle outlines onto a Lexis plot
#' 
#' @description This is an optional overlay function for Lexis map plots.
#' 
#' @param ages vector of ages, e.g. \code{0:110}. These are y values.
#' @param years vector of years, e.g. \code{1900:2010}. These are x values.
#' @param N. age interval. Default of 5.
#' @param Chrono logical. Default \code{TRUE}. Birth cohorts? If \code{FALSE} it assumes we have a TPD diagram and gives death cohorts.
#' @param equilateral logical. Default \code{FALSE}. Do we want 60 degree or 90/45 degree angles?
#' @param ... optional arguments passed to \code{segments()}
#' 
#' @export
#' 

LexRefN <- function(ages, years, N = 5, chrono = TRUE, equilateral = FALSE,...){
	# vertical
	#par(mai=c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
	#plot(NULL, xlim = c(1820,2060), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
	#LexRefN(0:100, 1900:2000, col = "#A5A5A5", N = 20, xpd=TRUE)
	# ages<-0:100; years <- 1900:2000; col = "#A5A5A5"; N = 20; equilateral <- TRUE; Chrono = TRUE
	# Chrono = FALSE
	minA      <- min(ages)
	maxA      <- max(ages)
	yearshift <- ifelse(equilateral, .5, 0) * ifelse(chrono, 1, -1)
	agemult   <- ifelse(equilateral, sqrt(3) / 2, 1)
	
	
	segments(years[years %% N == 0] - minA * yearshift,
			min(ages) * agemult,
			years[years %% N == 0] - maxA * yearshift,
			max(ages) * agemult,
			...
	)
	
	# horizontal lines
	segments(min(years) - ages[ages %% N == 0] * yearshift,
			ages[ages %% N == 0] * agemult,
			max(years) - ages[ages %% N == 0] * yearshift,
			ages[ages %% N == 0] * agemult,
			...
	)
	# diag cohort references, bit more tricky:
	l.years <- (min(years) - ages) 
	coh.ext <- range(c(l.years[l.years %% N == 0], years[years %% N == 0]))
	cohs    <- seq(coh.ext[1], coh.ext[2], by = N)
	
	# bottom, left:
	xl  <- cohs + min(ages)
	yb <- rep(min(ages), length(cohs))
	yb[xl < min(years)] <- yb[xl < min(years)] + min(years) - xl[xl < min(years)]
	xl[xl < min(years)] <- min(years)
	
	# top, right:
	xr  <- cohs + max(ages)
	yt  <- rep(max(ages), length(cohs))
	yt[xr > max(years)] <- yt[xr > max(years)] - xr[xr > max(years)] + max(years)
	xr[xr > max(years)] <- max(years)
	
	# cut down one last time:
	xr <- xr[yt >= min(ages)]
	xl <- xl[yt >= min(ages)]
	yb <- yb[yt >= min(ages)]
	yt <- yt[yt >= min(ages)]
	
	# draw cohort refs:
	if (chrono){
		segments(xl - yb * yearshift, 
				yb * agemult, 
				xr - yt * yearshift, 
				yt * agemult, 
				...
				)
	} else {
		segments(rev(xr)- yb * yearshift, 
				yb * agemult, 
				rev(xl)- yt * yearshift, 
				yt * agemult,
				...
				)
	}
	
}


#'
#' @title ATLRefN Overlays an ATL grid onto an already-open plot
#' 
#' @description This is an optional overlay function for ATL diagrams
#' 
#' @param ages vector of ages, e.g. \code{0:110}. These are translated to both thanatological and chronological ages, and the diagram is assumed to be regular.
#' @param N. age interval. Default of 5.
#' @param equilateral logical. Default \code{FALSE}. Do we want 60 degree or 90/45 degree angles?
#' @param ... optional arguments passed to \code{segments()}
#' 
#' @export
#' 

ATLRefN <- function(ages, N = 5, equilateral = FALSE, ...){
	
	minA      <- min(ages)
	maxA      <- max(ages)
	Th <- C   <- sort(unique(ages[as.integer(ages) %% N == 0]))
	
	if (equilateral){
		# left-leaning
		segments(C,rep(minA, length(C)) * sqrt(3)/2 ,C*.5, Th * sqrt(3)/2, ...)
		
		# right-leaning
		segments(C,rep(minA, length(C)) * sqrt(3)/2, C + rev(Th)*.5,rev(Th) * sqrt(3)/2, ...)
		
		# horizontals.
		segments(C - .5 * Th, Th * sqrt(3) / 2, rev(C) + .5 * Th, Th * sqrt(3) / 2, ...)

	} else {
		# verticals
		xv  <- rev(C)
		y1v <- rep(minA,length(C))
		y2v <- Th
		segments(xv,y1v,xv,y2v, ...)
		
		# horizontals
		x1h <- rep(minA, length(Th))
		x2h <- rev(C)
		yh  <- Th
		segments(x1h,yh,x2h,yh, ...)
		
		# diagonals
		segments(rep(minA,length(Th)),Th,C,rep(minA,length(C)), ...)
	}
}
