
# Author: tim
###############################################################################




#'
#' @title LexRefN Overlays semi-transparent N-year lexis triangle outlines onto a Lexis plot
#' 
#' @description This is an optional overlay function for Lexis map plots.
#' 
#' @param ages vector of ages, e.g. \code{0:110}. These are y values.
#' @param years vector of years, e.g. \code{1900:2010}. These are x values.
#' @param N. age interval. Default of 5.
#' @param Chrono logical. Default \code{TRUE}. Birth cohorts? If \code{FALSE} it assumes we have a TPD diagram and gives death cohorts.
#' @param ... optional arguments passed to \code{segments()}
#' 
#' @export
#' 

LexRefN <- function(ages, years, N = 5, Chrono = TRUE, ...){
	# vertical
	segments(years[years %% N == 0],
			min(ages),
			years[years %% N == 0],
			max(ages),
			...
	
	)
	# horizontal lines
	segments(min(years),
			ages[ages %% N == 0],
			max(years),
			ages[ages %% N == 0],
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
	if (Chrono){
		segments(xl, yb, xr, yt, ...)
	} else {
		segments(rev(xr), yb, rev(xl), yt, ...)
	}
	
}


