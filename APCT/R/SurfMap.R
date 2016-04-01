
# Surf <- A
SurfMap <- function (Surf, 
  thano = as.integer(rownames(Surf)), 
  chrono = as.integer(colnames(Surf)), 
  colramp = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "OrRd")), space = "Lab"), 
  napprox = 10, 
 xlab = "Years lived, a", 
 ylab = "Years left, y",
 contour=TRUE,
 ticks = NULL,
 legnd = TRUE,
 outline = TRUE,
 mai = c(.5,.5,.5,1.5),
 bg=FALSE,
 add = FALSE,
 xshift = 0,
 yshift = 0,
 cex=.8) 
{
    if (is.null(ticks)){
      ticks <- pretty(Surf,n=napprox)
    }
    zlim  <- range(ticks)
    n     <- length(ticks)-1
    col   <- rev(colramp(n))

    SurfCol <- as.character(cut(Surf,breaks=ticks,labels=col))
    dim(SurfCol) <- dim(Surf)
    
    x <- col(Surf) - 1 + min(chrono) + xshift
    y <- row(Surf) - 1 + min(thano)  + yshift
#	x <- chrono
#	y <- thano
	if (! add){
        par(xaxs="i",yaxs="i",xpd=TRUE,mai=mai)
        plot(NULL, type = "n", xlim = c(70,101),ylim=c(0,15),xlab="",ylab="",axes=FALSE,asp=1)
    }
	if (bg){
		# draw background in light gray
		rect(70+xshift,0+yshift,101+xshift,14+yshift,border=NA,col=gray(.9))
		segments(70+xshift,seq(0,15,by=5)+yshift,101+xshift,seq(0,15,by=5)+yshift,col="white")
		segments(seq(70,100,by=5)+xshift,0+yshift,seq(70,100,by=5)+xshift,15+yshift,col="white")
		segments(c(0,0,0,5,10,15,20,25,30)+70+xshift,c(5,10,15,15,15,15,15,15,15)+yshift,
				c(5,10,15,20,25,30,31,31,31)+70+xshift,c(0,0,0,0,0,0,4,9,14)+yshift,col="white")
	}
	
  # draw cells
  rect(x, y, x + 1, y + 1, border = NA, col = SurfCol)
  

  # outline area
  if (outline){
    ages  <- as.integer(colnames(Surf))
    thana <- as.integer(rownames(Surf))
    MinL  <- ages[colSums(Surf,na.rm=TRUE) > 0][1]
    MaxL  <- rev(ages[colSums(Surf,na.rm=TRUE) > 0])[1]
    Th    <- rev(thana[rowSums(Surf,na.rm=TRUE) > 0])[1]
    segments(MinL,0,MinL,Th+1)
    segments(MinL,0,MaxL+1,0)
    segments(MinL,Th+1,MaxL - Th + 1,Th+1)
    segments((MaxL - Th + 1):MaxL,Th:1,(MaxL - Th + 2):(MaxL+1),Th:1)
    segments((MaxL - Th + 1):(MaxL+1),(Th + 1):1,(MaxL-Th+1):(MaxL+1),Th:0)
  }
    
    # axes
    segments(70+xshift,seq(0,15,by=5)+yshift,69.6+xshift,seq(0,15,by=5)+yshift)
    segments(seq(70,100,by=5)+xshift,0+yshift,seq(70,100,by=5)+xshift,-.4+yshift)
    text(69.6+xshift,seq(0,15,by=5)+yshift,seq(0,15,by=5),pos=2,cex=cex, xpd=TRUE)
    text(seq(70,100,by=5)+xshift,-.4+yshift,seq(70,100,by=5),pos=1,cex=cex, xpd=TRUE)
    
    if (legnd){
      # legend
      ticksat <- (ticks - min(ticks))/diff(range(ticks)) * 12 + 2.5
      rect(102+xshift,ticksat[-length(ticks)]+yshift,103.5+xshift,ticksat[-1]+yshift,col=col, border = gray(.6))
      text(103.5+xshift, ticksat+yshift,ticks,cex=.8,pos=4)
      
      # labels
      text(85+xshift,-2+yshift,xlab)
      text(68+xshift,17+yshift,ylab,pos=4)
    }
    if (contour){
      contour(chrono+.5+xshift, thano+.5+yshift, t(Surf), add = TRUE, col = gray(.2), levels = ticks, labcex = .8)
    }
}



