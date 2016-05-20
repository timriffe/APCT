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

#rgl.close()

# from Josh O'Brien on SO:
#http://stackoverflow.com/questions/16362381/save-the-orientation-of-a-rgl-plot3d-plot
## Save RGL parameters to a list object
#pp <- par3d(no.readonly=TRUE)
#pp <- par3d(no.readonly=TRUE)
## Save the list to a text file
#dput(pp, file="Data/APCrglview2.R", control = "all")
pp <- dget("Data/APCrglview2.R")

startYr <- 1800
endYr <- 2100

rgl.close()
omega <- 125 # for the sake of dimensioning
# =====================================
# set up space, a box, a prelim aspect ratio (final adjustment later)
plot3d(xlim = c(startYr, endYr), ylim = c(0, omega), zlim = c(0, omega), 
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
cal1         <- xyz2ternxyz(data.frame(x = c(startYr, endYr), y = c(0, 0), z = c(0, 0)))
leftagebound <- xyz2ternxyz(data.frame(x = c(startYr, startYr), y = c(0, omega-50), z = c(50, 50)))
thanoaxis    <- xyz2ternxyz(data.frame(x = c(startYr, startYr), y = c(0, 0), z = c(0, omega)))
#cal2 <- xyz2ternxyz(data.frame(x = c(startYr, endYr), y = c(omega, omega), z = c(0, 0)))
cal3 <- xyz2ternxyz(data.frame(x = c(startYr, endYr-100), y = c(0,0), z = c(omega, omega)))
rgl.linestrips(cal1$x, cal1$y, cal1$z, color = AssignColour("P"), lwd = 2)
#rgl.linestrips(leftagebound$x, leftagebound$y, leftagebound$z, color = AssignColour("A"), lwd = 2)
#rgl.linestrips(cal2$x, cal2$y, cal2$z, color = AssignColour("P"), lwd = 2)
rgl.linestrips(cal3$x, cal3$y, cal3$z, color = AssignColour("P"), lwd = 2)
rgl.linestrips(thanoaxis$x, thanoaxis$y, thanoaxis$z, color = AssignColour("T"), lwd = 2)
# add an apc plane on the bottom
# birth cohort lines
z <- 50 # how high to draw apc plane
whitesurface <- xyz2ternxyz(data.frame(x=c(startYr,startYr,endYr,endYr+omega-z),
				y=c(omega-z,0,0,omega-z),z=c(z,z,z,z)-.02))
rgl.quads(whitesurface$x,whitesurface$y,whitesurface$z,alpha=.7,color="white",
		ambient="white",specular="white",emission="white", sinyness=0)
cohorts <- seq(startYr-omega+z, endYr, by = 25)
for (i in 1:length(cohorts)){
	cohi <- xyz2ternxyz(data.frame(x = c(ifelse(cohorts[i]<startYr,startYr,cohorts[i]), cohorts[i] + omega - z), 
					y = c(ifelse(cohorts[i]<startYr,startYr-cohorts[i],0), omega - z), z = c(z, z)))
	rgl.linestrips(cohi$x, cohi$y, cohi$z, color = AssignColour("C"))
}
# complete periods into future
# period lines
periods <- seq(startYr, endYr + omega - z, by = 25)
for (i in 1:length(periods)){
	cohi <- xyz2ternxyz(data.frame(x = c(periods[i], periods[i]), 
					y = c(ifelse(periods[i]<endYr,0,periods[i]-endYr), omega-z), z = c(z, z)))
	rgl.linestrips(cohi$x, cohi$y, cohi$z, color = AssignColour("P"))
}
# age lines
ages <- seq(0, omega-z, by = 25)
for (i in 1:length(ages)){
	cohi <- xyz2ternxyz(data.frame(x = c(startYr, endYr + ages[i]), y = ages[i], z = c(z, z)))
	rgl.linestrips(cohi$x, cohi$y, cohi$z, color = AssignColour("A"))
}

# an example rgl TAL (1800 birth cohort, at back)
xyztern <- xyz2ternxyz(data.frame(
				x = c(1800, 1800+omega, 1800),
				y = c(0, omega, 0),
				z = c(0, 0, omega)))
rgl.linestrips(xyztern$x[c(1,2)],xyztern$y[c(1,2)],xyztern$z[c(1,2)],color = gray(.5))
rgl.linestrips(xyztern$x[c(2,3)],xyztern$y[c(2,3)],xyztern$z[c(2,3)],color = gray(.5))
rgl.linestrips(xyztern$x[c(1,3)],xyztern$y[c(1,3)],xyztern$z[c(1,3)],color = gray(.5))
cohs <- seq(1800,1850,by=25)
# an example rgl TAL (2000 birth cohort)
for (co in cohs){
	xyztern <- xyz2ternxyz(data.frame(
					x = c(co, co+omega, co) ,
					y = c(0, omega, 0),
					z = c(0, 0, omega)))
	rgl.triangles(xyztern$x,xyztern$y,xyztern$z,alpha=0.1,color = gray(.8),
			ambient="black",specular="black",emission="black",shininess=0)
	rgl.linestrips(xyztern$x[c(1,2)],xyztern$y[c(1,2)],xyztern$z[c(1,2)],color = gray(.5))
	rgl.linestrips(xyztern$x[c(2,3)],xyztern$y[c(2,3)],xyztern$z[c(2,3)],color = gray(.5))
	rgl.linestrips(xyztern$x[c(1,3)],xyztern$y[c(1,3)],xyztern$z[c(1,3)],color = gray(.5))
}
zs <- c(0,25)	
for (z in zs){
	xyztern <- xyz2ternxyz(data.frame(
					x = c(1800, 1800, 2100, 2100+omega-z),
					y = c(omega-z,0, 0, omega-z),
					z = c(z, z, z,z)))
	rgl.quads(xyztern$x,xyztern$y,xyztern$z,alpha=0.3,color = gray(.8),
			ambient="black",specular="white",emission="black")
	# left
	rgl.linestrips(xyztern$x[c(1,2)],xyztern$y[c(1,2)],xyztern$z[c(1,2)],color = gray(.5))
	# bottom
	rgl.linestrips(xyztern$x[c(2,3)],xyztern$y[c(2,3)],xyztern$z[c(2,3)],color = gray(.5))
	#right
	rgl.linestrips(xyztern$x[c(3,4)],xyztern$y[c(3,4)],xyztern$z[c(3,4)],color = gray(.5))
	# back
	rgl.linestrips(xyztern$x[c(4,1)],xyztern$y[c(4,1)],xyztern$z[c(4,1)],color = gray(.5))
}
# more APC planes in the back, cut off
zs <- c(75,100)	
for (z in zs){
	xyztern <- xyz2ternxyz(data.frame(
					x = c(1800, 1800, 1900, 1900+omega-z),
					y = c(omega-z,0, 0, omega-z),
					z = c(z, z, z,z)))
	rgl.quads(xyztern$x,xyztern$y,xyztern$z,alpha=0.3,color = gray(.5),
			ambient="black",specular="white",emission="black")
	# left
	rgl.linestrips(xyztern$x[c(1,2)],xyztern$y[c(1,2)],xyztern$z[c(1,2)],color = gray(.5))
	# bottom
	rgl.linestrips(xyztern$x[c(2,3)],xyztern$y[c(2,3)],xyztern$z[c(2,3)],color = gray(.5))
	#right
	rgl.linestrips(xyztern$x[c(3,4)],xyztern$y[c(3,4)],xyztern$z[c(3,4)],color = gray(.5))
	# back
	rgl.linestrips(xyztern$x[c(4,1)],xyztern$y[c(4,1)],xyztern$z[c(4,1)],color = gray(.5))
}

par3d(pp)
rgl.postscript("Figures/APCisorgl2.1.pdf",fmt="pdf")

writeOBJ("Figures/APCisorgl2.1.obj",withNormals=FALSE,linesAsLines=TRUE)

# need a test model to try to import layered.









x <- diff(par3d("bbox")[1:2])
y <- diff(par3d("bbox")[3:4])
z <- diff(par3d("bbox")[5:6])
decorate3d(xlim = c(startYr, endYr), ylim = c(0, omega-25), zlim = c(0, omega-25), 
		aspect = c(x, y, z), box = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "")
#decorate3d(aspect = c(x, y, z), box = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "")

rgl.postscript("Figures/APCisorgl2.2.pdf",fmt="pdf")
#
#open3d()
#shade3d(readOBJ(url(
#						"http://sci.esa.int/science-e/www/object/doc.cfm?fobjectid=54726"
#				), material = list(col = "gray")))


# lower left
# 619.764  516.692
# lower right
# 642.022  484.880

# base plate:
# xyz
zs <- c(0,25)	
for (z in zs){
	xyztern <- xyz2ternxyz(data.frame(
					x = c(1800, 1800, 2100, 2100+omega-z),
					y = c(omega-z,0, 0, omega-z),
					z = c(z, z, z,z)))
	rgl.quads(xyztern$x,xyztern$y,xyztern$z,alpha=0.3,color = gray(.8),
			ambient="black",specular="white",emission="black")
	# left
	rgl.linestrips(xyztern$x[c(1,2)],xyztern$y[c(1,2)],xyztern$z[c(1,2)],color = gray(.5))
	# bottom
	rgl.linestrips(xyztern$x[c(2,3)],xyztern$y[c(2,3)],xyztern$z[c(2,3)],color = gray(.5))
	#right
	rgl.linestrips(xyztern$x[c(3,4)],xyztern$y[c(3,4)],xyztern$z[c(3,4)],color = gray(.5))
	# back
	rgl.linestrips(xyztern$x[c(4,1)],xyztern$y[c(4,1)],xyztern$z[c(4,1)],color = gray(.5))
}