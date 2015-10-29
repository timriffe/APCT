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

# need a function to take an xyz coord and transform to ternary xyz.
xyz2ternxyz <- function(xyz){
	ternxyz   <- xyz
	
	ternxyz$x <- xyz$x - (xyz$y * .5)
	ternxyz$y <- xyz$y * sqrt(3) / 2 + xyz$z * (sqrt(3) / 2 - 1 / sqrt(3))
	ternxyz$z <- xyz$z * sqrt(6) / 3
	ternxyz
}
# colors chosen by JS, his function. May as well use the same ones!
# here: https://github.com/jschoeley/typotime/blob/master/R/diatime.R
AssignColour <- function (x) {
	if (x == "A") result <- "#D23737"
	if (x == "P") result <- "#3191C9"
	if (x == "C") result <- "#D2BC2D"
	if (x == "T") result <- "#4EC93B"
	if (x == "D") result <- "#881F93"
	if (x == "L") result <- "#C5752B"
	return(result)
}
#rgl.close()

# from Josh O'Brien on SO:
#http://stackoverflow.com/questions/16362381/save-the-orientation-of-a-rgl-plot3d-plot
## Save RGL parameters to a list object
#pp <- par3d(no.readonly=TRUE)

## Save the list to a text file
#dput(pp, file="Data/TALrglview.R", control = "all")
pp <- dget("Data/TALrglview.R")



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
# now 3 calendar lines
cal1 <- xyz2ternxyz(data.frame(x=c(1000,2500),y=c(0,0),z=c(0,0)))
cal2 <- xyz2ternxyz(data.frame(x=c(1000,2500),y=c(omega,omega),z=c(0,0)))
cal3 <- xyz2ternxyz(data.frame(x=c(1000,2500),y=c(0,0),z=c(omega,omega)))
rgl.linestrips(cal1$x,cal1$y,cal1$z,color = AssignColour("A"))
rgl.linestrips(cal2$x,cal2$y,cal2$z,color = AssignColour("A"))
rgl.linestrips(cal3$x,cal3$y,cal3$z,color = AssignColour("A"))

# add an apc plane on the bottom

# birth cohort lines
cohorts <- seq(1000,2500,by=25)
for (i in 1:length(cohorts)){
	cohi <- xyz2ternxyz(data.frame(x=c(cohorts[i],cohorts[i]+omega),y=c(0,omega),z=c(0,0)))
	rgl.linestrips(cohi$x,cohi$y,cohi$z,color = AssignColour("C"))
}
# period lines
periods <- seq(1000,2500,by=25)
for (i in 1:length(periods)){
	cohi <- xyz2ternxyz(data.frame(x=c(periods[i],periods[i]),y=c(0,omega),z=c(0,0)))
	rgl.linestrips(cohi$x,cohi$y,cohi$z,color = AssignColour("P"))
}
# age lines
ages <- seq(0,omega,by=25)
for (i in 1:length(ages)){
	cohi <- xyz2ternxyz(data.frame(x=c(1000,2500),y=ages[i],z=c(0,0)))
	rgl.linestrips(cohi$x,cohi$y,cohi$z,color = AssignColour("A"))
}


ns   <- seq(100,1000,by=100)
# an example rgl TAL (1900 birth cohort)
for (n in ns){
	xyztern <- xyz2ternxyz(data.frame(
					x = c(2000, 2000+omega, 2000) - n,
					y = c(0, omega, 0),
					z = c(0, 0, omega)))
	rgl.triangles(
			x = xyztern$x,
			y = xyztern$y, 
			z = xyztern$z,
			col = gray(.5),
			shininess = 0,
			lit=FALSE,
			alpha = .4)
}

# the fron TAL cohort panel (2000 birth cohort)
xyztern <- xyz2ternxyz(data.frame(
				x = c(2000, 2000+omega, 2000),
				y = c(0, omega, 0),
				z = c(0, 0, omega)))
rgl.triangles(
		x = xyztern$x,
		y = xyztern$y, 
		z = xyztern$z,
		col = gray(.9),
		shininess = 0,
		lit=FALSE,
		alpha = .85)
# add lifespan lines to 2000 TAL
ages <- seq(0,omega,by=25)
for (i in ages){
	lsi <- xyz2ternxyz(data.frame(x=c(2000,2000+i),y=c(0,i),z=c(i,0)))
	rgl.linestrips(lsi$x,lsi$y,lsi$z,color = AssignColour("L"))
}
# add thano lines to 2000 TAL
for (i in ages){
	tsi <- xyz2ternxyz(data.frame(x=c(2000,2000+i),y=c(0,i),z=omega-c(i,i)))
	rgl.linestrips(tsi$x,tsi$y,tsi$z,color = AssignColour("T"))
}
# add chrono lines to 2000 TAL
for (i in ages){
	asi <- xyz2ternxyz(data.frame(x=c(2000+i,2000+i),y=c(i,i),z=c(0,omega-i)))
	rgl.linestrips(asi$x,asi$y,asi$z,color = AssignColour("A"))
}

par3d(pp)
x <- diff(par3d("bbox")[1:2])
y <- diff(par3d("bbox")[3:4])
z <- diff(par3d("bbox")[5:6])
decorate3d(xlim = c(1800, 2100), ylim = c(0, 100), zlim = c(0, 100), 
		aspect = c(x, y, z), box = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "")
#decorate3d(aspect = c(x, y, z), box = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "")

rgl.postscript("Figures/TALisorgl.pdf",fmt="pdf")
rgl.snapshot("Figures/TALisorgl.png")
#
#rgl.material(
#		color        = c("white"),
#		alpha        = c(1.0),
#		lit          = TRUE, 
#		ambient      = "black",
#		specular     = "white", 
#		emission     = "black", 
#		shininess    = 50.0, 
#		smooth       = TRUE,
#		texture      = NULL, 
#		textype      = "rgb", 
#		texmipmap    = FALSE, 
#		texminfilter = "linear", 
#		texmagfilter = "linear",
#		texenvmap    = FALSE,
#		front        = "fill", 
#		back         = "fill",
#		size         = 3.0, 
#		lwd          = 1.0,
#		fog          = TRUE,
#		point_antialias = FALSE,
#		line_antialias = FALSE,
#		depth_mask   = TRUE,
#		depth_test   = "less",
#		...
#)
#(x, y = NULL, z = NULL, normals=NULL, texcoords=NULL, ... )

