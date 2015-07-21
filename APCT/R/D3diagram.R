# This is still rather experimental, and possibly a waste of time.
# 
# Author: triffe
###############################################################################
#install.packages("rgl")

library(rgl)
#
#clear3d()

#rgl.close()
# =====================================
# set up space, a box, a prelim aspect ratio (final adjustment later)
plot3d(x1, y1, z1, 
		box = FALSE, 
		aspect = c(3, 1, 1),
		axes = FALSE, 
		type ='n',
		xlab = "",
		ylab = "",
		zlab = "")
par3d(family="sans",FOV=10)
bg3d("white")
par3d(mouseMode=c("trackball","none",  "zoom",  "pull" ))
par3d(skipRedraw=TRUE)
#decorate3d(xlim=c(1800,2100), ylim=c(0,100), zlim=c(0,100),aspect=c(3,1,1),box = FALSE, axes = FALSE, xlab = "year",ylab = "chrono age", zlab = "thano age")


# =====================================
# birth cohort diagonals
# (black)
x1 <- seq(1800, 2000, by = 10)
x2 <- x1 + 100
#
z1 <- z2 <- y1 <- rep(0, length(seq(0, 100, by = 10)))
y2 <- y1 + 100

for (z in 0:(length(z1) - 1)){
    for (i in 1:length(x1)){
		rgl.linestrips(
				c(x1[i], x2[i] - 10 * z),
				c(0, 100 - 10 * z),
				c(10 * z, 10 * z),
				col = "black", alpha = .7)
	}
}

# =====================================
# age horizontals 
# (red)
x1 <- seq(1800, 1900, by = 10)
x2 <- x1 + 200

y1 <- seq(0, 100, by = 10)
y2 <- seq(0, 100, by = 10)

z1 <- z2 <- rep(0, 11)
#plot3d(x1,y1,z1,box=FALSE,aspect=TRUE,axes=FALSE,type ='n',xlab="",ylab="",zlab="")
#decorate3d(xlim=c(1800,2100), ylim=c(0,100), zlim=c(0,100),aspect=TRUE,box = FALSE, axes = FALSE, xlab = "year",ylab = "chrono age", zlab = "thano age")

for (z in 0:(length(z1) - 1)){
	for (i in 1:(length(x1) - z)){
		rgl.linestrips(
				c(x1[i], x2[i]),
				c(y1[i], y2[i]),
				c(10 * z, 10 * z),
				col = "red",
				alpha = .7)
	}
}
# =====================================
# year "verticals" (they'd be vertical in x,y plot, but they are 'depth' in xyz space)
# (green)
x1 <- seq(1800, 2100, by = 10)
x2 <- x1

y1 <- c(rep(0, 21), seq(10, 100, by = 10))
y2 <- c(seq(0, 90, by = 10), rep(100, 21))

zl <- 10#

for (z in 0:zl){
	# I hate this line. Need to think this through another way...
	y2[y2 > 100 - z * 10] <- 100 - z * 10
	for (i in 1:(length(x1) - z)){
		rgl.linestrips(
				c(x1[i], x2[i]),
				c(y1[i], y2[i]),
				c(10 * z, 10 * z),
				col = "green",
				alpha = .7)
	}
}

# =====================================
# vertical (z) struts:
# (blue)
# this needs to increment by 10 for each y
x1 <- seq(1800, 2000, by = 10)
x2 <- x1

# this loop is super slow, sorry
zl <- 10
for (z in zl:0){
	for (y in 0:zl){
		for (x in x1){
			#y22 <- y2-10*z
			#y22 <- ifelse(sign(y22) < 0, 0, y22)
			rgl.linestrips(c(x + y * 10, x + y * 10),
					       c(y * 10, y * 10),
						   c(0, max(10 * z - 10 * y, 0)),
						   col = "blue",
						   alpha = .5)
		}
	}
}

# =======================================
# now the cohort lifelines :-)
# (magenta)     lifelines run parallel to these
x1 <- seq(1800, 2000, by = 10)

inc <- seq(10, 100, by = 10)
for (iz in inc){
	for (i in 1:length(x1)){
		rgl.linestrips(
				c(x1[i], x2[i] + iz),
				c(0, iz),
				c(iz, 0),
				col = "magenta", 
				alpha = .7)
	}
}


# =========================================
# Add ATL semitransparent triangle for 1900 birth cohort:
# (magenta)
rgl.triangles(
		x = c(1900, 2020, 1900),
		y = c(0, 120, 0), 
		z = c(0, 0, 120),
		col = "magenta",
		alpha = .3)
# a label pointer
rgl.linestrips(
		c(2000, 2025),
		c(100, 125),
		c(0, 0),
		color = "magenta",
		alpha = .7)
# label
text3d(2030, 130, 0, text = "1900 cohort ATL", col = "black", family="sans", cex = 1.1, alpha = 1) 
# Add ATL semitransparent triangle for 1900 period
# (green)
rgl.triangles(
		x = c(1920, 1920, 1920),
		y = c(0, 120, 0), 
		z = c(0, 0, 120), col = "green", alpha = .3)
text3d(1920, 125, 0, text = "1920 period ATL", col = "black", family="sans", cex = .9, alpha = 1) 

# =========================================
# add labelling:

# period
x <- seq(1800, 2100, by = 10)
texts3d(x ,rep(0, length(x)), rep(0, length(x)), texts = x, col = "black", adj = c(0, 1), family="sans", cex = .7, alpha = 1) 
text3d(1950, -10, -10, text = "Year", col = "black", family="sans", cex = .9, alpha = 1) 
# (green) depth lines
x1 <- x2 <- seq(2000, 2100, by = 10)
y1 <- 0; y2 <- seq(0, 100, by = 10)
for (i in 1:11){
	rgl.linestrips(
			c(x1[i], x2[i]),
			c(y1, y2[i]),
			c(0, 0),
			color = "green",
			alpha = 1)
}

# =========================================
# thano age (needs no guidelines, can just follow an edge)
text3d(1785, -5, 50, text = "thano age", col = "black", family="sans", cex = 1.1, alpha = 1) 
texts3d(1797, rep(0, 11), seq(0, 100, by = 10), texts = seq(0, 100, by = 10), col = "black", family="sans", cex = .7, alpha = 1) 

# =========================================
# chrono age 
# (red)
x1 <- 1800; x2 <- seq(1800, 1900, by = 10)
y1 <- y2 <- seq(0, 100, by = 10)
for (i in 1:11){
	rgl.linestrips(
			c(x1, x2[i]),
			c(y1[i], y2[i]),
			c(0, 0),
			color = "red",
			alpha = 1)
}
texts3d(1797, seq(0, 100, by = 10), rep(0, 11), texts = seq(0, 100, by = 10), col = "black", family="sans", cex = .7, alpha = 1) 
text3d(1790, 50, 0, text = "chrono age", col = "black", family="sans", cex = 1.1, alpha = 1) 

# ====================================
# add planes for start and end of observation (1900, 2000), indicating past and future
rgl.quads(x = c(1900, 1900, 1900, 1900), 
		y = c(-10, 110, 110, -10), 
		z = c(-10, -10, 110, 110), 
		col = gray(.5),
		alpha = .1 )
rgl.quads(
		x = c(2000, 2000, 2000, 2000), 
		y = c(-10, 110, 110, -10), 
		z = c(-10, -10, 110, 110), 
		col = gray(.5),
		alpha = .1 )
text3d(1900, 117, 117, text = "start of obs. 1900", col = "black", family="sans", cex = .9, alpha = 1) 
text3d(2000, 117, 117, text = "end of obs. 2000", col = "black", family="sans", cex = .9, alpha = 1) 
# title
text3d(1950, 0, 150, text = "Tim Riffe (2015) A unified model of demographic time", col = "black", family="sans", cex = 1.2, alpha = 1) 

# =========================================
# now apply proper aspect ratio to the whole thing
par3d(skipRedraw=FALSE)
x <- diff(par3d("bbox")[1:2])
y <- diff(par3d("bbox")[3:4])
z <- diff(par3d("bbox")[5:6])
decorate3d(xlim = c(1800, 2100), ylim = c(0, 100), zlim = c(0, 100), 
		aspect = c(x, y, z), box = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "")

# =========================================
# and now we have a wireframe
# can save to WebGL, for viewing in browser.
# just set a working directory if needed.
writeWebGL(dir = "RGL1")

