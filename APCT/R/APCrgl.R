
# Author: tim
###############################################################################
# Init --------------------------------------------------------------------
# this script produces a snapshot of the 3d model- TAL highlighted as sheets.
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
library(rgl)
source("R/Functions.R")
# need a function to take an xyz coord and transform to ternary xyz.


#rgl.close()

# from Josh O'Brien on SO:
#http://stackoverflow.com/questions/16362381/save-the-orientation-of-a-rgl-plot3d-plot
## Save RGL parameters to a list object
#pp <- par3d(no.readonly=TRUE)

## Save the list to a text file
#dput(pp, file="Data/APCrglview.R", control = "all")
pp <- dget("Data/APCrglview.R")


rgl.close() 
omega <- 125 # for the sake of dimensioning
# =====================================
# set up space, a box, a prelim aspect ratio (final adjustment later)
plot3d(xlim = c(1800, 2100), ylim = c(0, 100), zlim = c(0, 100), 
		box = FALSE, 
		aspect = c(3, 1, 1),
		axes = FALSE, 
		type ='n',
		xlab = "",
		ylab = "",
		zlab = "")
#par3d(family="sans",FOV=10)
bg3d("white")

maxP <- 2000
minP <- 1800
ns   <- seq(0,omega,by=25)

# end census triangle
for (p in c(maxP,minP)){

xyztern <- xyz2ternxyz(data.frame(
				x = c(p,p,p),
				y = c(0, omega, 0),
				z = c(0, 0, omega)))
rgl.linestrips(xyztern$x[1:2],xyztern$y[1:2],xyztern$z[1:2],color = AssignColour("L"),line_antialias=TRUE,lwd=2)
rgl.linestrips(xyztern$x[2:3],xyztern$y[2:3],xyztern$z[2:3],color = AssignColour("A"),line_antialias=TRUE,lwd=2)
rgl.linestrips(xyztern$x[c(1,3)],xyztern$y[c(1,3)],xyztern$z[c(1,3)],color = AssignColour("T"),line_antialias=TRUE,lwd=2)

Lsi <- seq(25,100,by=25)
for (L in Lsi){
	lsi <- xyz2ternxyz(data.frame(
					x = c(p,p),
					y = c(L, 0),
					z = c(0, L)))
	rgl.linestrips(lsi$x,lsi$y,lsi$z,color = AssignColour("L"),line_antialias=TRUE)
}
Asi <- seq(25,100,by=25)
for (A in Asi){
	asi <- xyz2ternxyz(data.frame(
					x = c(p,p),
					y = c(A, A),
					z = c(0, omega-A)))
	rgl.linestrips(asi$x,asi$y,asi$z,color = AssignColour("A"),line_antialias=TRUE)
}
Tsi <- seq(50,100,by=25)
for (TT in Tsi){
	tsi <- xyz2ternxyz(data.frame(
					x = c(p,p),
					y = c(0, omega-TT),
					z = c(TT, TT)))
	rgl.linestrips(tsi$x,tsi$y,tsi$z,color = AssignColour("T"),line_antialias=TRUE)
}
}

#rgl.linestrips(xyztern$x[1:2],xyztern$y[1:2],xyztern$z[1:2],color = AssignColour("P"))
#rgl.linestrips(xyztern$x[3:4],xyztern$y[3:4],xyztern$z[3:4],color = AssignColour("P"))
#rgl.linestrips(xyztern$x[2:3],xyztern$y[2:3],xyztern$z[2:3],color = AssignColour("A"))
# an example rgl TAL (1900 birth cohort)
 #n <- 25

 for (n in ns){
	 xyztern <- xyz2ternxyz(data.frame(
					 x = c(minP, minP, maxP, maxP),
					 y = c(0, omega-n, omega-n, 0),
					 z = c(n,n,n,n)))
	 # apc perimeter for layers > 25 --- green for thano
	 if (n > 25){
		 rgl.linestrips(xyztern$x[1:2],xyztern$y[1:2],xyztern$z[1:2],color = AssignColour("T"),line_antialias=TRUE)
		 rgl.linestrips(xyztern$x[3:4],xyztern$y[3:4],xyztern$z[3:4],color = AssignColour("T"))
		 rgl.linestrips(xyztern$x[2:3],xyztern$y[2:3],xyztern$z[2:3],color = AssignColour("T"),line_antialias=TRUE)
	 }
	 # bottom APC perimeter front & back --- green for thano
	 if (n == 0){
		 rgl.linestrips(xyztern$x[c(1,4)],xyztern$y[c(1,4)],xyztern$z[c(1,4)],color = AssignColour("T"),line_antialias=TRUE)
		 rgl.linestrips(xyztern$x[2:3],xyztern$y[2:3],xyztern$z[2:3],color = AssignColour("T"),line_antialias=TRUE)
	 }
	 # the APC plane
	 if (n == 25){
		 cohorts <- seq(minP-omega+n,maxP,by=25)
		 # i <- 1
		 for (i in 1:length(cohorts)){ 
	         x1 <- cohorts[i]
			 y1 <- 0
			 if (x1 < minP){
				 shift <- minP - x1
				 x1 <- x1 + shift
				 y1 <- y1 + shift
			 }
			 x2 <- cohorts[i] + omega - n
			 y2 <- omega - n
			 if (x2 > maxP){
				 shift <- x2 - maxP
				 x2 <- x2 - shift
				 y2 <- y2 - shift
			 }
			 #(c(x1,x2,y1,y2))
			 cohi <- xyz2ternxyz(data.frame(x=c(x1,x2),y=c(y1,y2),z=c(n,n)+0.1))
			 rgl.linestrips(cohi$x,cohi$y,cohi$z,color = AssignColour("C"),line_antialias=TRUE)
		 }
# period lines
		 periods <- seq(minP+25,maxP,by=25)
		 for (i in 1:length(periods)){
			 peri <- xyz2ternxyz(data.frame(x=c(periods[i],periods[i]),y=c(0,omega-n),z=c(n,n)+0.1))
			 rgl.linestrips(peri$x,peri$y,peri$z,color = AssignColour("P"),line_antialias=TRUE)
		 }
# age lines
		 ages <- seq(25,omega-n,by=25)
		 for (i in 1:length(ages)){
			 agei <- xyz2ternxyz(data.frame(x=c(minP,maxP),y=ages[i],z=c(n,n)+0.1))
			 rgl.linestrips(agei$x,agei$y,agei$z,color = AssignColour("A"),line_antialias=TRUE)
		 }
		 # period axis
	 
	 }
 }
 # axes for apc
 peri <- xyz2ternxyz(data.frame(x=c(minP,minP),y=c(0,omega-25),z=c(25,25)))
 rgl.linestrips(peri$x,peri$y,peri$z,color = AssignColour("A"),line_antialias=TRUE, lwd =2)
 agei <- xyz2ternxyz(data.frame(x=c(minP,maxP),y=0,z=c(25,25)))
 rgl.linestrips(agei$x,agei$y,agei$z,color = AssignColour("P"),line_antialias=TRUE,lwd=2)
 
 xyzfront <- xyz2ternxyz(data.frame(
				 x = c(minP, maxP),
				 y = c(0, 0),
				 z = c(0,0)))
 rgl.linestrips(xyzfront$x,xyzfront$y,xyzfront$z,color = AssignColour("T"),line_antialias=TRUE)
 
 
 par3d(pp)
# x <- diff(par3d("bbox")[1:2])
# y <- diff(par3d("bbox")[3:4])
# z <- diff(par3d("bbox")[5:6])
# decorate3d(xlim = par3d("bbox")[1:2], par3d("bbox")[3:4], zlim = par3d("bbox")[5:6], 
#		 aspect = c(x, y, z), box = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "")

rgl.postscript("Figures/APCisorgl.pdf",fmt="pdf")
rgl.snapshot("Figures/APCisorgl.png")

