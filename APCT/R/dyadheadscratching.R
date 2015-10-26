
# Author: tim
###############################################################################
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
getwd()
# TPD
ttd       <- seq(0,100,by=25)
period    <- seq(1900,2050,by=25)
TPD       <- expand.grid(T=ttd,P=period)
TPD$D     <- rowSums(TPD)

pdf("Figures/DyadSanityCheck/ALL.pdf",width=10,height=10)
par(mfrow=c(3,3), mai = c(.4,.4,.4,.4))
#par(mfrow=c(1,3))
# TP(D)
plot(TPD$P,TPD$T, col = NA,asp=1,main="TP(D)")
text(TPD$P,TPD$T,TPD$D)
#dev.off()
# PD(T)
plot(TPD$D,TPD$P, col = NA,asp=1,main="PD(T)")
text(TPD$D,TPD$P,TPD$T)
# TD(P)
plot(TPD$D,TPD$T, col = NA,asp=1,main="TD(P)")
text(TPD$D,TPD$T,TPD$P)

# TAL
age <- seq(0,100,by=25)
ttd <- seq(0,100,by=25)

TAL    <- expand.grid(A=age,T=ttd)
TAL$L  <- rowSums(TAL)
TAL <- TAL[TAL$L <= 100, ]

#par(mfrow=c(1,3))
# TA(L)
plot(TAL$A,TAL$T, col = NA,asp=1,main="TA(L)")
text(TAL$A,TAL$T,TAL$L)

# TL(A)
plot(TAL$L,TAL$T, col = NA,asp=1,main="TL(A)")
text(TAL$L,TAL$T,TAL$A)

# AL(T)
plot(TAL$L,TAL$A, col = NA,asp=1,main="AL(T)")
text(TAL$L,TAL$A,TAL$T)

# LCD
LS  <- seq(0,100,by=25)
Coh <- seq(1900,2050,by=25)

LCD    <- expand.grid(L=LS,C=Coh)
LCD$D  <- rowSums(LCD)

#par(mfrow=c(1,3))
# LC(D)
plot(LCD$C,LCD$L, col = NA,asp=1,main="LC(D)")
text(LCD$C,LCD$L,LCD$D)
# CD(L)
plot(LCD$D,LCD$C, col = NA,asp=1,main="CD(L)")
text(LCD$D,LCD$C,LCD$L)

# LD(C)
plot(LCD$D,LCD$L, col = NA,asp=1,main="LD(C)")
text(LCD$D,LCD$L,LCD$C)

dev.off()


