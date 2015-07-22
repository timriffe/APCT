
# Author: TR
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

# load the loess model, which was fit with the following call:
# i.e., 5-year cohorts by remaining years by chrono age
#mod   <- loess('gross_mot  ~Coh5 * ta * ca' ,
#		data = Dat[Dat$sex == "f", ], 
#		weights = p_wt2, # this, plus point density both act as weights
#		span = .7,     # a variable passed in, or smoothness
#		# is similiar conceptually to a 1:1:1 aspect ratio. Everything is in years...
#		normalize = FALSE,
#		control = loess.control(trace.hat="approximate")
#)
#
mod <- local(get(load("Data/gross_mot.Rdata")))

print(object.size(mod), units="Mb")

# these are our bounds

# you might want to make Coh5 finer, for a continuous fill.
# it'd be tricky, since the 1925 cohort is composed of 1925-1929,
# but you can't actually extrapolate to 1929. Maybe I should have named the 
# cohorts by their midpoints?
# instead I only did the discrete slices, so the cohort named 1915 is actually
# 1915-1919, and as a squished cross-section that's OK, but not so much for
# single-year or finer interpolation/extrapolation. Will have to think on it
# maybe refit models without grouping cohorts??
Coh5  <- c(1905,1910,1915,1920,1925)
t.age <- 0:12
c.age <- 70:100

# multiplicative give the most freedom.

newdata        <- expand.grid(ta = t.age+.5, ca = c.age+.5, Coh5 = Coh5)
# easier to keep dimensions straight if we predict over rectangular grid, 
# then throw out values outside range
# Surf is actually an array
Surf           <- predict(mod, newdata)

dimnames(Surf) <- list(floor(t.age),floor(c.age), Coh5)

# HRS wave endpoints
LeftYear  <- 1992
RightYear <- 2011

# this reduces extrapolation outside of data points 
for (i in 1:dim(Surf)[3]){
	#maxL  <- 2011 - Coh5[i] - 1
	#maxt  <- tamax[as.character(Coh5[i])]
	#keept <- as.integer(rownames(Surf)) <= maxt
	A     <- Surf[,,i]
	MaxL <- RightYear - Coh5[i] - 1
	A[ col(A) - 1 + 70 + row(A) - 1 > MaxL] <- NA
# possibly need to trim lower left corner too: dimnames(A)
	MinL <- LeftYear - (Coh5[i] + 5)
	A[col(A) + 70 - 1 < MinL] <- NA
	#A[!keept, ] <- NA 
	Surf[,,i] <- A
}

Surf[Surf<0] <- 0
matplot(Surf[,,2], type = 'l',col=gray(.5))
image(Surf[,,5])
range(Surf,na.rm=TRUE)

par(mfrow=c(1,5))
for (i in 1:5){
	image(as.integer(colnames(Surf)),
			as.integer(rownames(Surf)),
			t(Surf[,,i]),zlim=range(Surf,na.rm=TRUE))
}

