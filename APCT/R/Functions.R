
# Author: tim
###############################################################################

if (!"HMDHFDplus" %in% rownames(installed.packages())){
	devtools::install_github("timriffe/TR1/TR1/HMDHFDplus")
}
require(HMDHFDplus)
devtools::load_all("/home/tim/workspace/DemoSurf/DemoSurf")
library(scales)

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

LexRefN <- function(ages, years, N = 5, 
		chrono = TRUE, equilateral = FALSE, 
		col1 = gray(.5), col2 = gray(.5), col3 = gray(.5), ...){
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
			col = col1,
			...
	)
	
	# horizontal lines
	segments(min(years) - ages[ages %% N == 0] * yearshift,
			ages[ages %% N == 0] * agemult,
			max(years) - ages[ages %% N == 0] * yearshift,
			ages[ages %% N == 0] * agemult,
			col = col2,
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
				col = col3,
				...
				)
	} else {
		segments(rev(xr)- yb * yearshift, 
				yb * agemult, 
				rev(xl)- yt * yearshift, 
				yt * agemult,
				col = col3,
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

ATLRefN <- function(ages, N = 5, equilateral = FALSE, col1="#A5A5A5",col2="#A5A5A5",col3="#A5A5A5",...){
	
	minA      <- min(ages)
	maxA      <- max(ages)
	Th <- C   <- sort(unique(ages[as.integer(ages) %% N == 0]))
	
	if (equilateral){
		# left-leaning
		segments(C,rep(minA, length(C)) * sqrt(3)/2 ,C*.5, Th * sqrt(3)/2, col = col1,...)
		
		# right-leaning
		segments(C,rep(minA, length(C)) * sqrt(3)/2, C + rev(Th)*.5,rev(Th) * sqrt(3)/2, col = col2, ...)
		
		# horizontals.
		segments(C - .5 * Th, Th * sqrt(3) / 2, rev(C) + .5 * Th, Th * sqrt(3) / 2, col = col3, ...)

	} else {
		# verticals
		xv  <- rev(C)
		y1v <- rep(minA,length(C))
		y2v <- Th
		segments(xv,y1v,xv,y2v, col = col1, ...)
		
		# horizontals
		x1h <- rep(minA, length(Th))
		x2h <- rev(C)
		yh  <- Th
		segments(x1h,yh,x2h,yh, col = col2, ...)
		
		# diagonals
		segments(rep(minA,length(Th)),Th,C,rep(minA,length(C)), col = col3, ...)
	}
}

# colors chosen by JS, his function. May as well use the same ones!
# here: https://github.com/jschoeley/typotime/blob/master/R/diatime.R
#AssignColour <- function (x) {
#	if (x == "A") result <- "#D23737"
#	if (x == "P") result <- "#3191C9"
#	if (x == "C") result <- "#D2BC2D"
#	if (x == "T") result <- "#4EC93B"
#	if (x == "D") result <- "#881F93"
#	if (x == "L") result <- "#C5752B"
#	return(result)
#}
#library(RColorBrewer)
#display.brewer.pal(7,"Dark2")
#brewer.pal(8,"Dark2")
AssignColour <- function (x) {
	if (x == "A") result <- "#D23737"
	if (x == "P") result <- "#3191C9"
	if (x == "C") result <- "#D2BC2D"
	if (x == "T") result <- "#4EC93B"
	if (x == "D") result <- "#881F93"
	if (x == "L") result <- "#C5752B"
	return(result)
}
#library(scales)
#cols <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
#		"#e6ab02")
#show_col(c(cols, muted(cols, l = 40, c = 100)))

xyz2ternxyz <- function(xyz){
	ternxyz   <- xyz
	
	ternxyz$x <- xyz$x - (xyz$y * .5)
	ternxyz$y <- xyz$y * sqrt(3) / 2 + xyz$z * (sqrt(3) / 2 - 1 / sqrt(3))
	ternxyz$z <- xyz$z * sqrt(6) / 3
	ternxyz
}



# Author: tim
###############################################################################
library(magrittr)
#library(DecompHoriuchi)


# Gompertz mx given a,b, and age vector
gompmx <- function(a,b,x){
	a * exp(b*x)
}

# other gompertz exact and approximate things,
# not used. Instead we go to lifetable mode
gompdx <- function(a,b,x){
	a*exp(b*x)*exp(-a/b *(exp(b*x)-1))
}
gomplx <- function(a,b,x){
	exp(-a/b*(exp(b*x)-1))
}
gompex <- function(a,b,x1,x2=Inf){
	integrate(gomplx, lower = x1, upper = x2, a = a, b = b)$value
}

gompLx <- function(a,b,x){
	N <- length(x)
	Lx <- rep(NA, N)
	ag <- c(x,Inf)
	for (i in 1:N){
		Lx[i] <- gompex(a,b,ag[i],ag[i+1])
	}
	Lx
}

# this works unless mx is high (infants, very old)
# some helper functions.

# no need for a0 because we assume starting age later in life, e.g. 60
mx2qx <- function(mx){
	mx / (1 + .5 * mx)
}

# cumprod of px
qx2lx <- function(qx){
	cumprod(1-c(0,qx))[1:length(qx)]
}

#
lx2dx <- function(lx){
	-diff(c(lx,0))
}

lx2Lx <- function(lx,closeout=0){
	(lx + c(lx[-1],closeout)) / 2
	
}

lx2ex <- function(lx){
	lx <- lx / lx[1]
	Lx <- lx2Lx(lx)
	sum(Lx)
}

mx2ex <- function(mx){
	mx %>% mx2lx %>% lx2ex
}
mx2dx <- function(mx){
	mx %>% mx2lx %>% lx2dx
}
mx2Lx <- function(mx){
	mx %>% mx2lx %>% lx2Lx
}
mx2lx <- function(mx){
	mx %>% mx2qx %>% qx2lx
}
#plot(x,mx2lx(gompmx(am,bm,x))[-1], type = 'l')
#lines(x, mx2lx(gompmx(af,bf,x))[-1],col="red")
#

#mx2ex(gompmx(am,bm,x))
#mx2ex(gompmx(af,bf,x))

############################
# morbidity by ttd and age, given a vector either chrono or thano
# dx is for dimensions. This function just makes a conformable matrix
# assuming you don't have one to start with. dx <- dxl;morb <- gy
May <- function(morb, dx, chrono = FALSE){
	N 				        <- length(dx)
	
	if (is.null(dim(morb))){
		if (chrono){
			stopifnot(length(morb) == N)
			Morb 			<- matrix(morb, N, N, byrow = TRUE)
		} else {
			if (length(morb) < N){
				morb        <- c(morb,rep(0, N - length(morb)))
			}
			Morb 			<- matrix(morb, N, N)
		}
	} else {
		# in this case we uglyly assign 0s to values outside the range.
		# to do something fancier, make Morb yourself!
		Morb                <- matrix(0, N, N)
		Morb[1:nrow(morb), 1:ncol(morb)] <- morb
	}
	Morb
}
#plot(x,mx2dx(gompmx(am,bm,x))[-1], type = 'l')
#lines(x, mx2dx(gompmx(af,bf,x))[-1],col="red")

day <- function(dx){
	# rescale to be sure
	dx 				<- dx / sum(dx)
	N 				<- length(dx)
	dmat 			<- matrix(0, N, N)
	day 			<- dx[row(dmat) + col(dmat) - 1]
	dim(day) 		<- dim(dmat)
	day[is.na(day)] <- 0
	# all(colSums(day) == rowSums(day))
	day
}

#dayf <- day(mx2dx(gompmx(af,bf,x)))
#rowSums(dayf) - colSums(dayf)

# Morb must either be a thano x chrono matrix
# of morbidity prevalence (cross classified)
# or it can be a thanatological vector. If you want 
# to model a strictly chronological process then
# you'd be the 2d matrix where values are equal within
# columns.
#dxweight <- function(dx, Morb){
#	
#	if (is.null(dim(Morb))){
#		# then we assume Morb is a thanatological vector
#		Morb <- matrix(Morb,ncol=length(dx),nrow=length(Morb))
#	}
#	stopifnot(length(dx) == ncol(Morb))
#	dxM      <- dx[row(Morb) + col(Morb) - 1]
#	dim(dxM) <- dim(Morb)
#	dxM
#}

# weight morbidity triangle by dx (density of lifelines)
getMorbWeighted <- function(dx, Morb){
	Day <- day(dx)
	
	if(!all(dim(Day) == dim(Morb))){
		Morb <- May(Morb,dx)
	}
	Day * Morb
}
#plot(colSums(getMorbWeighted(dxf,Morb), na.rm = TRUE))
#lines(mx2lx(mxf))
#lines(mx2Lx(mxf))
#plot(colSums(getMorbWeighted(dxf,Morb), na.rm = TRUE)/mx2lx(mxf))
# unhealthy expectancy
# best to do everything straight from mx, because it's cooler to perturb it rather
# than the other columns #mx <- mxf
# this gives morb prevalence that lines up with lx, not Lx.
# need to average in the same way to translate to Lx and get
# expectancy. This one is OK for plotting ga, but it conforms
# with lx, not Lx, and shouldn't be used for expectancies.
getMorbagelx <- function(mx, Morb){
	
	dx  <- mx2dx(mx)
	Mwx <- colSums(getMorbWeighted(dx, Morb), na.rm = TRUE)
	lx  <- mx2lx(mx)
	
	if (length(lx) > length(Mwx)){ # this is now guaranteed 
		Mwx <- c(Mwx, Mwx[length(Mwx)])
	}
	Mwx / lx
}
# This one is adequate for calculating expectancies (multiply into Lx)
mxgay2gaLx <- function(mx, Morb){
	N 			<- length(mx)
	mwx   		<- getMorbagelx(mx, Morb)
	Mwx 		<- lx2Lx(mwx, closeout = mwx[N])
	Mwx 
}

mxgy2gaLx <- function(mx, gy){
	Morb 		<- May(morb = gy, dx = mx, chrono = FALSE)
	Mwx  		<- mxgay2gaLx(mx, Morb)
	Mwx
}
geteM <- function(mx, Morb){
	Lx  <- mx2Lx(mx)
	Mwx <- mxgay2gaLx(mx, Morb)
	sum(Mwx * Lx)
}

# prob: the morbidity today is a function of the mortality in the future?
# then we can't use period mortality to weight current morbidity?

# healthy expectancy
# likewise we build from mx
geteH <- function(mx, Morb){
	mx2ex(mx) - geteM(mx, Morb)
}



Diagram2 <- function(Abscissae, Ordinate, AbMeasure = "A", OrdMeasure = "P", 
		AbColor = NULL, OrdColor = NULL, DerColor = NULL,
		N = 5, isotropic = FALSE, add = TRUE){
	
	# Reference lines follow ticks
	Refs  <- LexRefNinternal(
			c(Abscissae, max(Abscissae) + 1), 
			c(Ordinate, max(Ordinate + 1)), 
			AbMeasure, OrdMeasure,
			N = N, 
			isotropic = isotropic)
	dyad          <- dyadaxes(x = OrdMeasure, y = AbMeasure, verbose = FALSE)
	increasing    <- dyad$increasing
	if (Refs$constrained){
		increasing <- FALSE
	}
	if (is.na(dyad$derived)){
		# isotropic has to be FALSE here, since Cartesian is the isotropic rendering...
		isotropic <- FALSE
	}
	# determine colors
	AbColor  <- ifelse(is.null(AbColor),DefaultColors(AbMeasure),AbColor)
	OrdColor <- ifelse(is.null(OrdColor),DefaultColors(OrdMeasure),OrdColor)
	DerColor <- ifelse(is.null(DerColor),DefaultColors(dyad$derived),DerColor)
	
	# ranges for plot device
	xrange <- range(c(Refs$Horizontal$x1,Refs$Horizontal$x2))
	yrange <- range(c(Refs$Vertical$y1,Refs$Vertical$y2))
	
	# open device if we're not adding to one
	if (!add){
		plot(NULL, type = "n", xlim = xrange, ylim = yrange, axes = FALSE, xlab = "", ylab = "",asp=1)
	}
	
	

	
	# Ordinate (Age) ticks:
	O2 <- c(Ordinate,max(Ordinate)+1)
	yat 	<- ylabs   <- O2[O2 %% N == 0]
	y0  	<- min(Abscissae)
	
	# Abscissa (Year) ticks
	A2 <- c(Abscissae,max(Abscissae)+1)
	xat 	<- xlabs   <- A2[A2 %% N == 0]
	x0 		<- min(Ordinate)
	Ticks 	<- list(
			x0 = x0, 
			xat = xat, 
			xlabs = xlabs, 
			y0 = y0, 
			yat = yat, 
			ylabs = ylabs)
	
	# adjust for isotropic, if necessary
	if (isotropic){
		
		# Ticks adjust
		if (increasing){
			Ticks$xat 	<- Ticks$xat - .5 * Ticks$x0
			Ticks$y0 	<- Ticks$y0 - .5 * Ticks$yat
		} else {
			Ticks$xat 	<- Ticks$xat + .5 * Ticks$x0
			Ticks$y0 	<- Ticks$y0 + .5 * Ticks$yat
		}
		Ticks$x0 	<- Ticks$x0 * sqrt(3) / 2    # this is a y coord
		Ticks$yat 	<- Ticks$yat * sqrt(3) / 2
	}
	
	# draw elements
	
	
	# Vertical
	segments(Refs$Vertical$x1,
			Refs$Vertical$y1,
			Refs$Vertical$x2,
			Refs$Vertical$y2,
			col = scales::muted(OrdColor, l = 70, c = 50),
			lty = "dashed")
	# and its axis
	segments(min(Refs$Vertical$x1),min(Refs$Vertical$y1), max(Refs$Vertical$x1), min(Refs$Vertical$y1),
			col = OrdColor, lwd = 1.5)
	
	# Horizontal
	segments(Refs$Horizontal$x1,
			Refs$Horizontal$y1,
			Refs$Horizontal$x2,
			Refs$Horizontal$y2,
			col = scales::muted(AbColor, l = 70, c = 50),
			lty = "dashed") 
	
	# and its axis
	segments(min(Refs$Horizontal$x1),min(Refs$Horizontal$y1), min(Refs$Horizontal$x1), mmax(Refs$Horizontal$y1),
			col = AbColor, lwd = 1.5)
	# Diagonal
	if (all(!is.na(Refs$Diagonal))){
		# if there is no derived measure, we don't do diagonals
		segments(Refs$Diagonal$x1,
				Refs$Diagonal$y1,
				Refs$Diagonal$x2,
				Refs$Diagonal$y2,
				col = scales::muted(DerColor, l = 70, c = 50),
				lty = "dashed")  
	}
	
}



# to get values for FreeCAD
#assignColorRGB <- function(x){
#	colorspace::hex2RGB(AssignColour(x))@coords * 255
#}
#assignColorRGB("A")
#assignColorRGB("P")
#assignColorRGB("C")
#assignColorRGB("T")
#assignColorRGB("D")
#assignColorRGB("L")

