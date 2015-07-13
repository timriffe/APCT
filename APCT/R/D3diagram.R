# TODO: Add comment
# 
# Author: riffe
###############################################################################
#install.packages("rgl")

library(rgl)

x <- c(1,4,3,6,2,5)
y <- c(2,2,4,3,5,9)
z <- c(1,3,5,9,2,2)
plot3d(x,y,z,box=FALSE,aspect=TRUE,axes=FALSE)
segments3d(x[2:3],y[2:3],z[2:3],col=1,lwd=2)


# try this stuff:
#http://homepage.stat.uiowa.edu/~luke/R/misc3d/misc3d-pdf/misc3d-pdf.pdf