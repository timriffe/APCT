
# Author: riffe
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
this.wd <- getwd()
# need to grab loess results from the ThanoEmpirical repository
TEwd    <- gsub(pattern="APCT",replacement="ThanoEmpirical",this.wd )
Results <- local(get(load(file.path(
								TEwd,
								"Data",
								"LoessQuinquenal_imp.Rdata"))))
names(Results)

# make 2 arrays, a male and a female.
# think some, then send to Alyson around when she gets back from vacay. Maybe Anna O too.
ADL3 <- Results[["adl3__0.5"]]
names(ADL3)


# can't have negatives
imp0 <- function(x){
	x[x < 0]<- 0
	x
}
Males   <- imp0(ADL3$Male$Surf)
Females <- imp0(ADL3$Female$Surf)



# quick peek
# image(t(Females[,,3]))
# note that we're missing a piece of the upper triangle (bottom of matrix)
# too few data points there to get anything out of it.
# let's just impute it with something reasonable-seeming
# so we can move forward.



#image(t(Males[,,3]))
a <- 70:100
#plot(a,Males["12",,3],type='l',col="green",ylim=c(0,.06))
#lines(a,Males["11",,3],col="royalblue")
#lines(a,Males["10",,3],col="blue")

# let's just apply the same proportional change that we see btwn
# the last two rows to each extrapolated row. If it drops
# below zero, we impute zero. Easy, cheap.
extendOmatic <- function(M){
	# some columns may have all NAs, be sure to keep it that way
	# assume data are in this very particular shape. OK, because
	# this is a once-off hack (we hope)
	start <- M[nrow(M), ]
	ratio <- start /  M[nrow(M) - 1, ]
	
	Col <- ncol(M)
	Row <- nrow(M)
	
	# the part we need to fill in with a triangle of extrapolated data
	NewBlock <- matrix(nrow = Col - Row, ncol = Col)
	
	for (i in 1:nrow(NewBlock)){
		NewBlock[i, ] <- start * ratio ^ i # cumprod
	}
	
	# append matrix
	Mout <- rbind(M, NewBlock)
	
	# need to respect NA diagonal.
	Nums <- row(Mout) + col(Mout)
	
	# need a marker on the right side in the top row
	Ind  <- max((1:ncol(Mout))[!is.na(Mout[1,])])
	Mout[Nums > Nums[1,Ind]] <- NA
	
	# clean extrap, just in case
	Mout[Mout < 0] <- 0
	Mout[Mout > 1] <- 1
	
	# include rownames
	rownames(Mout) <- 1:nrow(Mout) - 1
	Mout
}
# cool, now we can do stuff!
M1915e <- extendOmatic(Males[,,3])
F1915e <- extendOmatic(Females[,,3])

# make sure NA placement matches, you never know...
all(colSums(!is.na(M1915e)) == colSums(!is.na(F1915e)))
all(rowSums(!is.na(M1915e)) == rowSums(!is.na(F1915e)))

# slice off unneeded columns
M1915e <- M1915e[, colSums(!is.na(M1915e)) > 0] 
F1915e <- F1915e[, colSums(!is.na(F1915e)) > 0] 
M1915e <- M1915e[rowSums(!is.na(M1915e)) > 0, ] 
F1915e <- F1915e[rowSums(!is.na(F1915e)) > 0, ] 
# chrono ages + thano ages, should be same lengths
(ca <- as.integer(colnames(M1915e)))
(ta <- as.integer(rownames(M1915e)))

# quick'n easy from HMD:

#image(t(M1915e-F1915e))
#
#par(mfrow=c(1,2))
#image(t(M1915e), zlim = c(0,.6))
#image(t(F1915e), zlim = c(0,.6))

# females have higher values than males in ALL cells, wow.
# I guess that's consistent with lit.
sum(M1915e > F1915e, na.rm = TRUE)
sum(F1915e > M1915e, na.rm = TRUE)

# cool! now we want cohort Mx data for the 1915-1919 cohort,
# ages 74 - 95


#library(HMDHFDplus)
#cMx <- readHMD("Data/USA_cMx_1x5.txt")
#cMx <- cMx[cMx$Year == 1915 & cMx$Age >= 74 &  cMx$Age <= 95 & !is.na( cMx$Age),]
#cMxm <- cMx$Male
#cMxf <- cMx$Female
#
#dput(cMxm)
cMxm <- c(0.049986, 0.052877, 0.057103, 0.061325, 0.066315, 0.07132, 
		0.081139, 0.088158, 0.096355, 0.104718, 0.113543, 0.122644, 0.133971, 
		0.144406, 0.15709, 0.169257, 0.183678, 0.199131, 0.217271, 0.239125, 
		0.263696, 0.285049)
#dput(cMxf)
cMxf <- c(0.028989, 0.031342, 0.034459, 0.037594, 0.041088, 0.045432, 
		0.05254, 0.057924, 0.064801, 0.071759, 0.079933, 0.088454, 0.09763, 
		0.107266, 0.118591, 0.129736, 0.142486, 0.157165, 0.173349, 0.191498, 
		0.212159, 0.237106)

mx2qx <- function(mx){
	mx / (1 + .5 * mx)
}
qxm <- mx2qx(cMxm)
qxf <- mx2qx(cMxf)

qx2lx <- function(qx){
	cumprod(1-c(0,qx))
}
lxm <- qx2lx(qxm)
lxf <- qx2lx(qxf)

# these could of course be rescaled such that the radix is the 1992 population of people from
# the 1915-1919 cohorts. But I think it's not necessary here.
#plot(74:96,lxm,type='l')
#lines(74:96,lxf,col = "blue")

lx2dx <- function(lx){
	-diff(c(lx,0))
}

# dx is the sugar for working with these data.
plot(lx2dx(lxm),type='l')
lines(lx2dx(lxf),col="red")

#####################################
# we learn that there is another direction in which we ought to extend
# the data. Since we only have up to age 95, there are people alive in l(x)
# that we don't have in this window of d(x). That last open age group of d(x)
# is therefore present in each preceding age of l(x), and if we want to see the
# chrono consequences of these morbidity patterns then we need to be able to 
# compose an l(x) out of it's various d(x) components, taking into account
# the thanatological trajectories of each d(x). So we need values for those 
# missing d(x), for the ages > 95. I'm inclined to just repeat the experience of those
# that died at age 95, shifting it right. Then the matrix will get bigger.
# another problem will be that we need to extend cMx until the cohort
# is mostly extinct. In that case, how about doing the HMD extendo-O-matic
# thing and borrowing from the preceding cohorts.


# OK, step 1, for fudging a working example:
# extend our morbidity surfaces to include people out to omega.
# let's say omega is 110. Because HMD.

openm       <- rev(diag(M1915e[nrow(M1915e):1,]))
openf       <- rev(diag(F1915e[nrow(F1915e):1,]))
# again, for the sake of closing out. More thought could go into this.
anew        <- 96:110
RightSide   <- matrix(nrow=nrow(M1915e),ncol=length(anew),dimnames=list(rownames(M1915e),anew))
M1915ee     <- cbind(M1915e, RightSide)
F1915ee     <- cbind(F1915e, RightSide)
N           <- ncol(M1915e)
for (i in 1:ncol(RightSide)){
	for (z in 1:length(openm)){
		M1915ee[z,N+1+i-z] <- openm[z]
		F1915ee[z,N+1+i-z] <- openf[z]
	}
}

M1915ee <- rbind(M1915ee,M1915ee[1:15,] * 0)
F1915ee <- rbind(F1915ee,F1915ee[1:15,] * 0)

M1915ee[upper.tri(M1915ee)[nrow(M1915ee):1,]] <- NA
F1915ee[upper.tri(F1915ee)[nrow(F1915ee):1,]] <- NA
# PS: this kind of extrap could maybe just use Lee Carter?
# or just spline extrap? Extrap over logit, then expit back?
# or force it out to an asymptote? For this particular case
# there is no substantive precedent I can think of, given 
# the dimensions. Note that the above double-loop extrapolation
# thingy only affects 5% of a given chrono age for males (since
# .05 were in 96+ of d96). 11% for females. So the leverage of
# decision making on this piece is at least bounded by those
# percentages. We could put in all 1s or all 0s, that is, and 
# see the unreasonable bounds to the consequences.

# now extend Mx
#####################################
plot(log(cMxm))
plot(log(cMxf))
# cohort Mx is really dang linear.
# so I'm going to extrapolate with a line...
# Jim wouldn't like that. Whatever in this case
# we just want to close out. In the end, we just 
# want to draw conclusions about ages 74-95...
# these decisions boil back to the tail of d(x) for us,
# which constributes to the pattern in each preceding age.

coefm <- lm(log(cMxm)~c(74:95))$coef
coeff <- lm(log(cMxf)~c(74:95))$coef

# predict ages 96-110...
cMxme <- exp(coefm[1] + coefm[2] * 96:110)
cMxfe <- exp(coeff[1] + coeff[2] * 96:110)

# looks good.
#plot(74:95, cMxm,log='y', type = 'l', col = "blue",xlim=c(74,110),ylim=c(.02,1))
#lines(74:95, cMxf,col="red")
#points(96:110,cMxme,pch=19,col="blue" )
#points(96:110,cMxfe,pch=19,col="red" )

# so we'll redo to get the final d(x)
cMxmE <- c(cMxm,cMxme)
cMxfE <- c(cMxf,cMxfe)

lxm <- qx2lx(mx2qx(cMxmE))
lxf <- qx2lx(mx2qx(cMxfE))

dxm <- lx2dx(lxm)
dxf <- lx2dx(lxf)

# groups the last two dx elements so everything both adds and is conformable
dxm <- c(dxm[1:(length(dxm)-2)],sum(rev(dxm)[1:2]))
dxf <- c(dxf[1:(length(dxf)-2)],sum(rev(dxf)[1:2]))
# cool, we're closed out now.
#plot(dxm)
#plot(dxf)

##################################
# now we can do demography!!!!!
##################################
# in the course of this exercise, I realized
# that in order to make good use of these data, you
# need to close out the lifetable by extending until
# something close to extinction, and also extending
# the morbidity patterns so far, hence the above nonesense.
###################################

# Just assume we have two valid ATL patterns, 
# one for males, the other for females.
# yellow = hotter than red in this ramp....
#par(mfrow=c(1,2))
#image(74:110, 0:37, t(M1915ee), zlim = range(c(M1915ee, F1915ee), na.rm = TRUE), main = "males")
#image(74:110, 0:37, t(F1915ee), zlim = range(c(M1915ee, F1915ee), na.rm = TRUE), main = "females")

dxM      <- dxm[row(M1915ee) + col(M1915ee) - 1]
dim(dxM) <- dim(M1915ee)
dxF      <- dxf[row(F1915ee) + col(F1915ee) - 1]
dim(dxF) <- dim(F1915ee)

# verify that the column sums mathc lx:
all(colSums(dxM,na.rm=TRUE) == lxm[-length(lxm)])
all(colSums(dxF,na.rm=TRUE) == lxf[-length(lxf)])

# so, we can multiply in neat ways:
Mtc <- dxM * M1915ee
Ftc <- dxF * F1915ee

# the column sums of this morbidity prevalence matrix give the chronological
# age pattern of prevalence within the cohort (Assuming this radix)

Mc <- colSums(Mtc, na.rm=TRUE) / lxm[-length(lxm)]
Fc <- colSums(Ftc, na.rm=TRUE) / lxf[-length(lxf)]

#plot(74:110, Mc, type = 'l', ylim = c(0, .6), main = "marginal chronological age pattern")
#lines(74:110, Fc, col = "red")

# now, part of the difference between males and females is due to the underlying
# morbidity pattern, and part is due to the difference in the distribution
# of lifelines. 

# Further (what I tried to demonstrate in the presentation),
# if one holds the morbidity pattern constant, but changes
# the underlying mortality, we will draw different conclusions about the expected
# change in prevalence than if we simply assume the marginal chronological age pattern
# and then perturb mortality. 

# leverage gained by this added insight into morbidity variation could help
# better predict convergence/divergence between males and females, for instance.




