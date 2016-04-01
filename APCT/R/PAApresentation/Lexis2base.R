# Author: tim
###############################################################################
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

library(rgl,lib.loc="/usr/local/lib/R/site-library")
source("R/Functions.R")

#rgl.close()

# from Josh O'Brien on SO:
#http://stackoverflow.com/questions/16362381/save-the-orientation-of-a-rgl-plot3d-plot
## Save RGL parameters to a list object
#pp <- par3d(no.readonly=TRUE)
#pp <- par3d(no.readonly=TRUE)
## Save the list to a text file
#dput(pp, file="Data/TALrglview2.R", control = "all")
pp <- dget("Data/TALrglview2.R")
ppbird <- dget("Data/ppbird.R")
startYr <- 1800
endYr <- 2100
omega <- 125 #
#rgl.close()

# get parts for base APC plane
# add an apc plane on the bottom
# birth cohort lines
cohorts <- seq(startYr-omega, endYr, by = 25)
ages    <- seq(25, omega, by = 25)
periods <- seq(startYr + 25, endYr + omega, by = 25)
Cohi <- lapply(1:length(cohorts), function(i){
			cart <- data.frame(x = c(ifelse(cohorts[i]<startYr,startYr,cohorts[i]), cohorts[i] + omega), 
					y = c(ifelse(cohorts[i]<startYr,startYr-cohorts[i],0), omega), z = c(0, 0))
			tern <- xyz2ternxyz(cart)
			list(cart=cart,tern=tern)
		})
Peri <- lapply(1:length(periods),function(i){
			cart <- data.frame(x = c(periods[i], periods[i]), 
					y = c(ifelse(periods[i]<endYr,0,periods[i]-endYr), omega), 
					z = c(0, 0))
			tern <- xyz2ternxyz(cart)
			list(cart=cart,tern=tern)
		})
Agei <- lapply(1:length(ages),function(i){
			cart <- data.frame(x = c(startYr, endYr + ages[i]), y = ages[i], z = c(0, 0))
			tern <- xyz2ternxyz(cart)
			list(cart=cart,tern=tern)
		})
#############################
# axes lines
cart <- data.frame(x = c(startYr, endYr), y = c(0, 0), z = c(0, 0))
tern <- xyz2ternxyz(cart)
calaxis <- list(cart=cart,tern=tern)

cart <- data.frame(x = c(startYr, startYr), y = c(0, omega), z = c(0, 0))
tern <- xyz2ternxyz(cart)
leftageaxis <- list(cart=cart,tern=tern)

#############################
# axes labels
# age
cart        <- data.frame(x = seq(startYr, endYr, by = 50), y = c(0), z = c(0))
tern        <- xyz2ternxyz(cart)
calaxislabs <- list(cart=cart,tern=tern)

cart        <- data.frame(x = startYr, y = seq(0,omega,by=25), z = 0)
tern        <- xyz2ternxyz(cart)
leftageaxislabs <- list(cart=cart,tern=tern)

ecart <- data.frame(x = startYr, y = 0, z = omega)
tern        <- xyz2ternxyz(ecart)
extreme <- list(cart=ecart,tern=tern)

baseAPCplane <- list(Age = Agei, Period = Peri, Cohort = Cohi, 
		axes = list(calaxis = list(calaxis=calaxis,calaxislabs=calaxislabs),
				leftageaxis = list(leftageaxis=leftageaxis,leftageaxislabs=leftageaxislabs)),extreme=extreme)

plotbaseAPCplane <- function(baseAPCplane,tern = TRUE){
	
	
	meh <- lapply(baseAPCplane$Cohort, function(cohi,tern){
				rgl.linestrips(
						cohi$tern$x * tern + cohi$cart$x * (1 - tern), 
						cohi$tern$y * tern + cohi$cart$y * (1 - tern), 
						cohi$tern$z * tern + cohi$cart$z * (1 - tern), 
						color = AssignColour("C"))
			},tern=tern)
	meh <- lapply(baseAPCplane$Period, function(peri,tern){
				rgl.linestrips(
						peri$tern$x * tern + peri$cart$x * (1 - tern), 
						peri$tern$y * tern + peri$cart$y * (1 - tern), 
						peri$tern$z * tern + peri$cart$z * (1 - tern), 
						color = AssignColour("P"))
			},tern=tern)
	meh <- lapply(baseAPCplane$Age, function(agei,tern){
				rgl.linestrips(
						agei$tern$x * tern + agei$cart$x * (1 - tern), 
						agei$tern$y * tern + agei$cart$y * (1 - tern), 
						agei$tern$z * tern + agei$cart$z * (1 - tern), 
						color = AssignColour("A"))
			},tern=tern)
	
	#############################
# axes lines
	
	with(baseAPCplane$axes$calaxis,
			rgl.linestrips(
					calaxis$tern$x * tern + calaxis$cart$x * (1 - tern), 
					calaxis$tern$y * tern + calaxis$cart$y * (1 - tern), 
					calaxis$tern$z * tern + calaxis$cart$z * (1 - tern), 
					color = AssignColour("P"), lwd = 2))
	with(baseAPCplane$axes$calaxis,
			rgl.texts(
					calaxislabs$tern$x * tern + calaxislabs$cart$x * (1 - tern), 
					(calaxislabs$tern$y - 5) * tern + (calaxislabs$cart$y - 5) * (1 - tern),
					calaxislabs$tern$z * tern + calaxislabs$cart$z * (1 - tern), 
					calaxislabs$cart$x, adj = c(.5, 1),
					cex = 1.2, color="black",useFreeType=TRUE,family="sans"))
	with(baseAPCplane$axes$leftageaxis,
			rgl.linestrips(
					leftageaxis$tern$x * tern + leftageaxis$cart$x * (1 - tern), 
					leftageaxis$tern$y * tern + leftageaxis$cart$y * (1 - tern), 
					leftageaxis$tern$z * tern + leftageaxis$cart$z * (1 - tern), 
					color = AssignColour("A"), lwd = 2))
	with(baseAPCplane$axes$leftageaxis,
			rgl.texts(
					(leftageaxislabs$tern$x-5) * tern + (leftageaxislabs$cart$x - 5)* (1 - tern), 
					leftageaxislabs$tern$y * tern + leftageaxislabs$cart$y * (1 - tern),
					leftageaxislabs$tern$z * tern + leftageaxislabs$cart$z * (1 - tern), 
					leftageaxislabs$cart$y, 
					adj = c(1,.5),cex = 1.2, color="black",useFreeType=TRUE,family="sans"))
}

#############################################################
# step 1 move from right angle to constrained               #
# --------------------------------------------------------- #
i  <- 1
Steps <- 21
weights <- seq(0,1,length=Steps)
for (i in 1:Steps){
	open3d()
	plot3d(NULL,xlim = c(startYr, endYr), 
			ylim = c(0, omega), 
			zlim = c(0, omega), 
			box = FALSE, 
			aspect = c(3, 1, 1),
			axes = FALSE, 
			type ='n',
			xlab = "",
			ylab = "",
			zlab = "")
	par3d(ppbird)
	bg3d("white")
	plotbaseAPCplane(baseAPCplane, weights[i])
	par3d(ppbird)
	
	filename <- paste0("frame", i,".png")

	rgl.snapshot(filename = file.path("PAApresentation/Figures/APCcert2tern",filename), fmt = "png", top = TRUE)
	#rgl.postscript(filename = file.path("PAApresentation/Figures/APCcert2tern/PDF",filename),fmt = "pdf", drawText=TRUE)
	rgl.close()
}
getwd()
#?rgl.snapshot
#which(rownames(installed.packages())=="rgl")
#installed.packages()[which(rownames(installed.packages())=="rgl"),]
#?library
#############################################################
# this sequence moves from bird's eye to TAL viewing angle. #
# --------------------------------------------------------- #
#for the sake of dimensioning
# =====================================
# set up space, a box, a prelim aspect ratio (final adjustment later)
open3d()
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

plotbaseAPCplane(baseAPCplane,1)
rgl.points(baseAPCplane$extreme$tern$x,
		baseAPCplane$extreme$tern$y,
		baseAPCplane$extreme$tern$z)
par3d(ppbird)


#dput(ppbird, file="Data/ppbird.R", control = "all")
#ppbird <- par3d(no.readonly=TRUE)
# rgl.close()
# for each step, update $zoom and $userMatrix, and $FOV

f <- par3dinterp( 
		zoom = c(ppbird$zoom,pp$zoom), 
		userMatrix = list(ppbird$userMatrix, pp$userMatrix),
		FOV = c(0,30))
par3d()
#play3d(f,duration=1)
movie3d(f,duration=1,
		convert = FALSE,
		dir="Figures/APCbird2persp",
		frames="frame",
		movie=".png",
		fps = 20)
getwd()
bad.names <- file.path(list.files("Figures/APCbird2persp"))
file.rename(from = file.path("Figures/APCbird2persp",bad.names),
		to = file.path("Figures/APCbird2persp",
				paste0("frame",1:length(bad.names),".png")))
######################################################
# this sequence stays in the previous persp, and adds TAL planes
# in sequence.
# =====================================

# working here to get a good shot
plotTALcohort <- function(cohort = 1800, omega = 125){
	
	xyz <- xyz2ternxyz(data.frame(x=c(cohort,cohort+omega,cohort),y=c(0,omega,0),z=c(0,0,omega)))
	rgl.triangles(xyz, 
			color = gray(.7), 
			alpha = .8, 
			ambient = "black",
			specular     = "white", 
			emission     = "black", 
			shininess    = 30)
	# add lifespan lines to 2000 TAL
	ages <- seq(0,omega,by=25)
	for (i in ages){
		lsi <- xyz2ternxyz(data.frame(x=c(cohort,cohort+i),y=c(0,i),z=c(i,0)))
		rgl.linestrips(lsi$x,lsi$y,lsi$z,color = AssignColour("L"))
	}
# add thano lines to 2000 TAL
	for (i in ages){
		tsi <- xyz2ternxyz(data.frame(x=c(cohort,cohort+i),y=c(0,i),z=omega-c(i,i)))
		rgl.linestrips(tsi$x,tsi$y,tsi$z,color = AssignColour("T"))
	}
# add chrono lines to 2000 TAL
	for (i in ages){
		asi <- xyz2ternxyz(data.frame(x=c(cohort+i,cohort+i),y=c(i,i),z=c(0,omega-i)))
		rgl.linestrips(asi$x,asi$y,asi$z,color = AssignColour("A"))
	}
}
plotTALcohort(1800)
pp
par3d()




plot3d(xlim = c(startYr, endYr), ylim = c(0, omega), zlim = c(0, omega), 
		box = FALSE, 
		aspect = c(3, 1, 1),
		axes = FALSE, 
		type ='n',
		xlab = "",
		ylab = "",
		zlab = "")
par3d(pp)
bg3d("white")
plotbaseAPCplane(baseAPCplane,1)
par3d(pp)

#######################################

ns   <- seq(25,200,by=25)
# an example rgl TAL (2000 birth cohort)
for (n in ns){
	xyztern <- xyz2ternxyz(data.frame(
					x = c(2000, 2000+omega, 2000) - n,
					y = c(0, omega, 0),
					z = c(0, 0, omega)))
	rgl.linestrips(xyztern$x[c(1,2)],xyztern$y[c(1,2)],xyztern$z[c(1,2)],color = gray(.5))
	rgl.linestrips(xyztern$x[c(2,3)],xyztern$y[c(2,3)],xyztern$z[c(2,3)],color = gray(.5))
	rgl.linestrips(xyztern$x[c(1,3)],xyztern$y[c(1,3)],xyztern$z[c(1,3)],color = gray(.5))
}

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





# now 3 calendar lines
cal1         <- xyz2ternxyz(data.frame(x = c(startYr, endYr), y = c(0, 0), z = c(0, 0)))
leftagebound <- xyz2ternxyz(data.frame(x = c(startYr, startYr), y = c(0, omega), z = c(0, 0)))
thanoaxis    <- xyz2ternxyz(data.frame(x = c(startYr, startYr), y = c(0, 0), z = c(0, omega)))
#cal2 <- xyz2ternxyz(data.frame(x = c(startYr, endYr), y = c(omega, omega), z = c(0, 0)))
cal3 <- xyz2ternxyz(data.frame(x = c(startYr, endYr-100), y = c(0,0), z = c(omega, omega)))
rgl.linestrips(cal1$x, cal1$y, cal1$z, color = AssignColour("P"), lwd = 2)
rgl.linestrips(leftagebound$x, leftagebound$y, leftagebound$z, color = AssignColour("A"), lwd = 2)
#rgl.linestrips(cal2$x, cal2$y, cal2$z, color = AssignColour("P"), lwd = 2)
rgl.linestrips(cal3$x, cal3$y, cal3$z, color = AssignColour("P"), lwd = 2)
rgl.linestrips(thanoaxis$x, thanoaxis$y, thanoaxis$z, color = AssignColour("T"), lwd = 2)







