# Author: tim
###############################################################################
# this script produces a snapshot of the 3d model- TAL highlighted as sheets.

library(rgl)
#clear3d()

#rgl.close()
# =====================================
# set up space, a box, a prelim aspect ratio (final adjustment later)
plot3d(c(0,1), c(0,1), c(0,1), 
		box = FALSE, 
		aspect = c(3, 1, 1),
		axes = FALSE, 
		type ='n',
		xlab = "",
		ylab = "",
		zlab = "")
#par3d(family="sans",FOV=10)
bg3d("white")
#par3d(mouseMode=c("trackball","none",  "zoom",  "pull" ))
#par3d(skipRedraw=TRUE)
decorate3d(xlim=c(1800,2100), ylim=c(0,100), zlim=c(0,100),aspect=c(3,1,1),box = FALSE, axes = FALSE, xlab = "year",ylab = "chrono age", zlab = "thano age")

# make an APC layer for thano age 0.





# an example rgl TAL (1900 birth cohort)
rgl.triangles(
		x = c(1900, 2020, 1900),
		y = c(0, 120, 0), 
		z = c(0, 0, 120),
		col = "gray",
		alpha = .3)



args(rgl.triangles)


rgl.material(
		color        = c("white"),
		alpha        = c(1.0),
		lit          = TRUE, 
		ambient      = "black",
		specular     = "white", 
		emission     = "black", 
		shininess    = 50.0, 
		smooth       = TRUE,
		texture      = NULL, 
		textype      = "rgb", 
		texmipmap    = FALSE, 
		texminfilter = "linear", 
		texmagfilter = "linear",
		texenvmap    = FALSE,
		front        = "fill", 
		back         = "fill",
		size         = 3.0, 
		lwd          = 1.0,
		fog          = TRUE,
		point_antialias = FALSE,
		line_antialias = FALSE,
		depth_mask   = TRUE,
		depth_test   = "less",
		...
)
(x, y = NULL, z = NULL, normals=NULL, texcoords=NULL, ... )

