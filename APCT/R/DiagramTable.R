
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
				"A","D"), byrow=TRUE,ncol=2)
N <- 1
T <- L <- A <- 0:2
D <- C <- P <- 0:2
Measures <- list(T=T,L=L,A=A,D=D,C=C,P=P)
i <- 1

for (i in 1:nrow(Combos)){
	a1 <- Combos[i,2]
	o1 <- Combos[i,1]
	pdf(paste0("/home/tim/workspace/DemoSurf/Figures/Test/",a1,o1,".pdf"))
	Diagram2(Abscissae=Measures[[a1]],Ordinate=Measures[[o1]],AbMeasure=a1,OrdMeasure=o1,N=N, isotropic=FALSE,add=FALSE)
	dev.off()
	Diagram2(Abscissae=Measures[[a1]],Ordinate=Measures[[o1]],AbMeasure=a1,OrdMeasure=o1,N=N, isotropic=TRUE)
	dev.off()
}



up_rt <- function(vcol, hcol, dcol){
	x <- seq(1/3, 1, length = 3)
	
	arrows(0,0,0,1,col=hcol,length=.05)
	arrows(0,0,1,0,col=vcol,length=.05)
	
	segments(x,0,x,1,col=vcol,lty=2,lwd=.5)
	segments(0,x,1,x,col=hcol,lty=2,lwd=.5)
	
	segments(0,x,x,0,col=dcol,lty=2,lwd=.5)
	segments(x[1:2],1,1,x[1:2],col=dcol,lty=2,lwd=.5)
	
}

dev.new(width=2,height=2,units="cm")
par(mai=c(.25,.25,.25,.25),xaxs="i",yaxs="i",xpd=TRUE)
plot(NULL,type="n",xlim=c(0,1),ylim=c(0,1), axes =FALSE, xlab="",ylab="")
up_rt(DefaultColors("P"),DefaultColors("A"),DefaultColors("C"))

down_rt <- function(vcol, hcol, dcol){}
tri_rt <- function(vcol, hcol, dcol){}

up_eq <- function(vcol, hcol, dcol){}
down_eq <- function(vcol, hcol, dcol){}
tri_eq <- function(vcol, hcol, dcol){}


