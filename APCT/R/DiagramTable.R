
# this script uses the HMDresults object to search for common patterns to the various defined measures.
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
library(scales)
source("R/Functions.R")

Combos <- matrix(c("A","P",
				"A","C",
				"C","P",
				
				"T","P",
				"P","D",
				"T","D",
				
				"T","A",
				"T","L",
				"A","L",
				
				"L","C",
				"C","D",
				"L","D",
				
				"L","P",
				"C","T",
				"A","D"), byrow = TRUE, ncol = 2)

noid <- function(vcol, hcol){
	x <- c(1/3,2/3)
	
	segments(x,0,x,1,col=muted(vcol, l = 70, c = 50),lty=1,lwd=1)
	segments(0,x,1,x,col=muted(hcol, l = 70, c = 50),lty=1,lwd=1)
	
	arrows(0,0,0,1,col=hcol,length=.05,lwd=2)
	arrows(0,0,1,0,col=vcol,length=.05,lwd=2)
	
}
down_rt <- function(vcol, hcol, dcol){
	x <- c(1/3,2/3)
		
	segments(x,0,x,1,col=muted(vcol, l = 70, c = 50),lty=1,lwd=1)
	segments(0,x,1,x,col=muted(hcol, l = 70, c = 50),lty=1,lwd=1)
	
	segments(0,x,x,0,col=muted(dcol, l = 70, c = 50),lty=1,lwd=1.5)
	segments(c(0,x),1,1,c(0,x),col=muted(dcol, l = 70, c = 50),lty=1,lwd=1)
	
	arrows(0,0,0,1,col=hcol,length=.05,lwd=2)
	arrows(0,0,1,0,col=vcol,length=.05,lwd=2)
	
}
up_rt <- function(vcol, hcol, dcol){
	x <- c(1/3,2/3)
		
	segments(x,0,x,1,col=muted(vcol, l = 70, c = 50),lty=1,lwd=1)
	segments(0,x,1,x,col=muted(hcol, l = 70, c = 50),lty=1,lwd=1)
	
	x1 <- c(0,0,0,1/3,2/3)
	x2 <- c(1/3,2/3,1,1,1)
	y1 <- c(2/3,1/3,0,0,0)
	y2 <- c(1,1,1,2/3,1/3)
	segments(x1,y1,x2,y2,col=muted(dcol, l = 70, c = 50),lty=1,lwd=1)
	
	arrows(0,0,0,1,col=hcol,length=.05,lwd=2)
	arrows(0,0,1,0,col=vcol,length=.05,lwd=2)
}
tri_rt <- function(vcol, hcol, dcol){
	x <- c(1/3,2/3)
	
	segments(x,0,x,rev(x),col=muted(vcol, l = 70, c = 50),lty=1,lwd=1)
	segments(0,x,rev(x),x,col=muted(hcol, l = 70, c = 50),lty=1,lwd=1)
	
	segments(0,c(x,1),c(x,1),0,col=muted(dcol, l = 70, c = 50),lty=1,lwd=1)
	
	arrows(0,0,0,1,col=hcol,length=.05,lwd=2)
	arrows(0,0,1,0,col=vcol,length=.05,lwd=2)
}
up_eq <- function(vcol, hcol, dcol){
	x <- c(1 / 3, 2 / 3)
	
	segments(x, 0, x - .5, sqrt(3) / 2, 
			col = muted(vcol, l = 70, c = 50), lty = 1, lwd = 1)
	segments(-x / 2, x * sqrt(3) / 2, 1 - x / 2, x * sqrt(3) / 2, 
			col = muted(hcol, l = 70, c = 50), lty = 1, lwd = 1)
	
	x1 <- c(0, 0, 0, 1 / 3, 2 / 3)
	x2 <- c(1 / 3, 2 / 3, 1, 1, 1)
	y1 <- c(2 / 3, 1 / 3, 0, 0, 0)
	y2 <- c(1, 1, 1, 2 / 3, 1 / 3)
	segments(x1 - y1 / 2, y1 * sqrt(3) / 2, x2 - y2 / 2, y2 * sqrt(3) / 2,
			col = muted(dcol, l = 70, c = 50), lty = 1, lwd = 1)
	
	arrows(0, 0, -.5, sqrt(3)/2, col = hcol, length = .05, lwd = 2)
	arrows(0, 0, 1, 0, col = vcol, length = .05, lwd = 2)
}
down_eq <- function(vcol, hcol, dcol){
	x <- c(1 / 3, 2 / 3)

	segments(x, 0, x + .5, sqrt(3) / 2, 
			col = muted(vcol, l = 70, c = 50), lty = 1, lwd = 1)
	segments(x / 2, x * sqrt(3) / 2, 1 + x / 2, x * sqrt(3) / 2, 
			col = muted(hcol, l = 70, c = 50), lty = 1, lwd = 1)

	segments(x / 2, x * sqrt(3) / 2, x, 0,
			col = muted(dcol, l = 70, c = 50), lty = 1, lwd = 1)
	segments(c(0, x) + .5, sqrt(3) / 2, 1 + c(0, x) / 2, c(0, x) * sqrt(3) / 2,
			col = muted(dcol, l = 70, c = 50), lty = 1, lwd = 1)
	
	arrows(0, 0, .5, sqrt(3) / 2, col = hcol, length = .05, lwd = 2)
	arrows(0, 0, 1, 0, col = vcol, length = .05, lwd = 2)
}
tri_eq <- function(vcol, hcol, dcol){
	x <- c(1/3,2/3)
	
	segments(x, 0, x + rev(x) / 2, rev(x) * sqrt(3) / 2,
			col = muted(vcol, l = 70, c = 50), lty = 1, lwd = 1)
	segments(x / 2, x * sqrt(3) / 2, rev(x) + x / 2, x * sqrt(3) / 2,
			col=muted(hcol, l = 70, c = 50), lty = 1, lwd = 1)
	
	segments(c(x,1)/2,c(x,1)*sqrt(3)/2,c(x,1),0,col=muted(dcol, l = 70, c = 50),lty=1,lwd=1)
	
	arrows(0,0,.5,sqrt(3)/2,col=hcol,length=.05, lwd = 2)
	arrows(0,0,1,0,col=vcol,length=.05, lwd = 2)
}

drawdiagram <- function(AbMeasure = "P", OrdMeasure = "A",isotropic=FALSE){
	ax <- dyadaxes(AbMeasure, OrdMeasure, verbose = FALSE)
	
	hcol <- DefaultColors(ax$y)
	vcol <- DefaultColors(ax$x)
	dcol <- DefaultColors(ax$derived)
	if (!is.na(ax$derived)){
		
		# right-angled diagrams
		if (!isotropic){
			
			# TA(L) triangle
			if (all(ax$ID == c("T", "A", "L"))){
				tri_rt(hcol = hcol, vcol = vcol, dcol = dcol)
				text(.5, .5, paste0("(",ax$derived,")"), pos = 3, col = dcol,srt=-45)
			} else {
				# AP(C)-style
				if (ax$increasing){
					up_rt(hcol = hcol, vcol = vcol, dcol = dcol)
					text(.8, .8 , paste0("(", ax$derived, ")"), col = dcol, srt = 45, pos = 3)
				} 
				# TP(D)-style
				if (!ax$increasing){
					down_rt(hcol = hcol, vcol = vcol, dcol = dcol)
					text(.9, .9, paste0("(",ax$derived,")"), col = dcol, srt=-45)
				}
			}
			text(.02, 1, ax$y, pos = 2, col = DefaultColors(ax$y))
			text(1, 0, ax$x, pos = 1, col = DefaultColors(ax$x))
		} else {
			# isotropic diagrams
			
			# TA(L) triangle
			if (all(ax$ID == c("T", "A", "L"))){
				tri_eq(hcol = hcol, vcol = vcol, dcol = dcol)
				text(.7, sqrt(3)/4+.1, paste0("(",ax$derived,")"), pos = 4, col = dcol,srt=-60)
				text(.5, sqrt(3)/2, ax$y, pos = 2, col = DefaultColors(ax$y))
			} else {
				if (ax$increasing){
					# AP(C)-style
					up_eq(hcol = hcol, vcol = vcol, dcol = dcol)
					text(-.46, sqrt(3)/2, ax$y, pos = 2, col = DefaultColors(ax$y))
					text(.42, sqrt(3) / 2 - .1 , paste0("(", ax$derived, ")"), col = dcol, srt = 60, pos = 3)
				} 
				if (!ax$increasing){
					# TP(D)-style
					down_eq(hcol = hcol, vcol = vcol, dcol = dcol)
					text(.51, sqrt(3)/2, ax$y, pos = 2, col = DefaultColors(ax$y))
					text(1.3, sqrt(3) / 2 - .1, paste0("(",ax$derived,")"), col = dcol, srt=-60)
				}
			}
		}
	} else {
		noid(hcol = hcol, vcol = DefaultColors(ax$x))
		text(.02, 1, ax$y, pos = 2, col = DefaultColors(ax$y))
	}
	text(1, 0, ax$x, pos = 1, col = DefaultColors(ax$x))
}

savediagram <- function( AbMeasure, OrdMeasure, isotropic = FALSE,folder = "Figures/DiagramTable"){
	
	ax          <- dyadaxes(AbMeasure, OrdMeasure, verbose = FALSE)
	TAL <- !all(is.na(ax$ID)) & all(ax$ID == c("T","A","L"))
	if (all(is.na(ax$ID))){
		isotropic <- FALSE
	}
	this.name 	<- file.path(folder, paste0(OrdMeasure, AbMeasure, ifelse(isotropic,"_iso","_rt"),".pdf"))
	
	width 		<- ifelse(TAL,5,ifelse(isotropic, 7, 5))
	xmin 		<- ifelse(TAL,0,ifelse(isotropic, ifelse(ax$increasing, -.5, 0), 0))
	xmax        <- ifelse(TAL,1,ifelse(isotropic, ifelse(ax$increasing, 1, 1.5), 1))
	
	if (TAL){
		width <- 5 
	}
	
	pdf(this.name, width = width/2.54, height = 5/2.54)
	# dev.new(width = width/2.54, height = 5/2.54)
	par(mai = c(.25, .25, .25, .25), xaxs = "i", yaxs = "i", xpd = TRUE)
	plot(NULL, type = "n", xlim = c(xmin, xmax), ylim = c(0, 1), 
			axes = FALSE, xlab = "", ylab = "", asp = 1)
	drawdiagram(AbMeasure, OrdMeasure, isotropic = isotropic)
	dev.off()
}

for (i in 1:nrow(Combos)){
	savediagram(AbMeasure = Combos[i,2], 
			OrdMeasure = Combos[i,1], isotropic = FALSE)
	savediagram(AbMeasure = Combos[i,2], 
			OrdMeasure = Combos[i,1], isotropic = TRUE)
}
# 1) if you regenerate figures, first remove cropped figures:
cropped <- list.files("Figures/DiagramTable")[grepl("-crop",list.files("Figures/DiagramTable"))]
file.remove(file.path("Figures/DiagramTable",cropped))
# then go to folder in navigator (not Eclipse), double-click pdfcropall.sh, and select run in terminal.
# appends -crop to each.


