# This is still rather experimental, and possibly a waste of time.
# 
# Author: riffe
###############################################################################
#install.packages("rgl")

library(rgl)
#
#clear3d()

#rgl.close()
# graphics.off()
plot3d(x1,y1,z1,box=FALSE,aspect=c(3,1,1),axes=FALSE,type ='n',xlab="",ylab="",zlab="")
#decorate3d(xlim=c(1800,2100), ylim=c(0,100), zlim=c(0,100),aspect=TRUE,box = FALSE, axes = FALSE, xlab = "year",ylab = "chrono age", zlab = "thano age")

# cohort diagonals?
x1 <- seq(1800,2000,by=10)
x2 <- x1 + 100
#
z1 <- z2 <- y1 <- rep(0, length(seq(0,100,by=10)))
y2 <- y1 + 100

for (z in 0:(length(z1)-1)){
    for (i in 1:length(x1)){
		segments3d(c(x1[i],x2[i]-10*z),c(0,100-10*z),c(10*z,10*z),col=1)
	}
}

# age horizontals:
x1 <- seq(1800,1900,by=10)
x2 <- x1 + 200

y1 <- seq(0,100,by=10)
y2 <- seq(0,100,by=10)

z1 <- z2 <- rep(0,11)
#plot3d(x1,y1,z1,box=FALSE,aspect=TRUE,axes=FALSE,type ='n',xlab="",ylab="",zlab="")
#decorate3d(xlim=c(1800,2100), ylim=c(0,100), zlim=c(0,100),aspect=TRUE,box = FALSE, axes = FALSE, xlab = "year",ylab = "chrono age", zlab = "thano age")

for (z in 0:(length(z1)-1)){
	for (i in 1:(length(x1)-z)){
		segments3d(c(x1[i],x2[i]),c(y1[i],y2[i]),c(10*z,10*z),col=2)
	}
}

# year "verticals"

x1 <- seq(1800,2100,by=10)
x2 <- x1

y1 <- c(rep(0,21),seq(10,100,by=10))
y2 <- c(seq(0,90,by=10),rep(100,21))

zl <- 11#


for (z in 0:(zl-1)){
	y2[y2 > 100-z*10] <- 100-z*10
	for (i in 1:(length(x1)-z)){
		#y22 <- y2-10*z
		#y22 <- ifelse(sign(y22) < 0, 0, y22)
		segments3d(c(x1[i],x2[i]),c(y1[i],y2[i]),c(10*z,10*z),col=3)
	}
}

# =====================================
# vertical (z) struts:

# this needs to increment by 10 for each y
x1 <- seq(1800,2000,by=10)
x2 <- x1

zl <- 10
for (z in zl:0){
	for (y in 0:10){
		for (x in x1){
			#y22 <- y2-10*z
			#y22 <- ifelse(sign(y22) < 0, 0, y22)
			segments3d(c(x+y*10,x+y*10),c(y*10,y*10),c(0,max(10*z - 10*y,0)),col=4)
		}
	}
}

# =======================================
# now the cohort lifelines :-)
# cohort diagonals?
x1 <- seq(1800,2000,by=10)

inc <- seq(10,100,by=10)
for (iz in inc){
	for (i in 1:length(x1)){
		segments3d(c(x1[i],x2[i]+iz),c(0,iz),c(iz,0),col=5)
	}
}


# =========================================
# Add ATL semitransparent triangle for 1900 birth cohort:
rgl.triangles(x=c(1900,2000,1900),y=c(0,100,0),z=c(0,0,100),col="#00FF1130")

# =========================================
# no apply proper aspect ratio to the whole thing
decorate3d(xlim=c(1800,2100), ylim=c(0,100), zlim=c(0,100),aspect=c(3,1,1),box = FALSE, axes = FALSE, xlab = "year",ylab = "chrono age", zlab = "thano age")

# and now we have a basic unlabeled wireframe

writeWebGL(dir="RGL1")

