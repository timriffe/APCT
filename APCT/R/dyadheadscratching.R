
# Author: tim
###############################################################################

# TPD
ttd       <- seq(0,100,by=10)
period    <- seq(1900,2000,by=10)
TPD       <- expand.grid(T=ttd,P=period)
TPD$D     <- rowSums(TPD)

par(mfrow=c(1,3))
# PD(T)
plot(TPD$D,TPD$P, col = NA)
text(TPD$D,TPD$P,TPD$T)

# TD(P)
plot(TPD$D,TPD$T, col = NA)
text(TPD$D,TPD$T,TPD$P)

# TP(D)
plot(TPD$P,TPD$T, col = NA)
text(TPD$P,TPD$T,TPD$D)



# TAL
age <- seq(0,100,by=10)
ttd <- seq(0,100,by=10)

TAL    <- expand.grid(A=age,T=ttd)
TAL$L  <- rowSums(TAL)
TAL <- TAL[TAL$L <= 100, ]

par(mfrow=c(1,3))
# TA(L)
plot(TAL$A,TAL$T, col = NA)
text(TAL$A,TAL$T,TAL$L)

# TL(A)
plot(TAL$L,TAL$T, col = NA)
text(TAL$L,TAL$T,TAL$A)

# AL(T)
plot(TAL$L,TAL$A, col = NA)
text(TAL$L,TAL$A,TAL$T)
