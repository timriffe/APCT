###########################################################
# TR: this script is rough but gets the job done.         #
# it contains statistical sins. These are currently       #
# being worked on by an actual statistician. I think      #
# the effects of results (of all my sins) will be minor,  #
# and that the qualitative conclusions will hold.         #
###########################################################

# for TR computers...otherwise you need to set wd yourself
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm","tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/APCT/APCT")
} 
getwd()
# install.packages("lubridate")
library(lubridate)
library(data.table)


# cleaning/processing functions
# need to make some codes usable...


# this code imported from ThanoEmpirical, but it is in need of some serious overhaul
convertDates <- function(Dat){
	# can't be done with apply because we can't have Date class matrices...
	DateInd       <- grep(pattern="_dt",colnames(Dat))
	for (i in DateInd){
		Dat[,i]    <- as.Date(Dat[,i],origin="1960-1-1")
	}
	invisible(Dat)
}

getChronoAge <- function(Date, BirthDate){
	out <- rep(NA, length(Date))
	Ind <- !is.na(Date) & !is.na(BirthDate)
	out[Ind] <- decimal_date(Date[Ind]) - decimal_date(BirthDate[Ind])
	out
}
getThanoAge <- function(Date, DeathDate){
	out <- rep(NA, length(Date))
	Ind <- !is.na(Date) & !is.na(DeathDate)
	out[Ind] <- decimal_date(DeathDate[Ind]) - decimal_date(Date[Ind])
	out
}

# TR: This is sketchy. To be replaced one day
imputeWeights <- function(wt,intv_dt){
	if (all(wt == 0)){
		wt2 <- NA * wt
		return(wt2)
	}
	if (sum(wt>0) == 1){
		wt2 <- approx(x = intv_dt[wt>0],
				y = wt[wt>0],
				xout = intv_dt,
				rule = 1:2,
				method = "constant",
				f = .5)$y
	}
	if (sum(wt>0)>=2){
		wt2 <- approx(x = intv_dt[wt>0],
				y = wt[wt>0],
				xout = intv_dt,
				rule = 1:2,
				method = "linear")$y 
	}
	return(wt2)
}


Dat           <- local(get(load("Data/thanos_long_v3_1.RData")))
# dead only = full coordinates:
Dat           <- Dat[Dat$dead == 1, ]

# remove missed interviews
Dat           <- Dat[!is.na(Dat$intv_dt), ]
# make sex column easier to use:
Dat$sex       <- ifelse(Dat$sex == "1.male","m","f")

# make date R-friendly
Dat           <- convertDates(Dat)

# data.table for easier processing of some stuff
Dat           <- data.table(Dat)

# for some years there are separate weights for those in care homes vs
# society at large. In this case, they add, because the default weights
# are set to zero
Dat$nh_wt[is.na(Dat$nh_wt)] <- 0
Dat$p_wt      <- Dat$p_wt + Dat$nh_wt

# but for other years the care home pop gets zero weights
# but we don't want to lose them, ergo: sketchy imputation 
# For some reason HRS gives them zero weight, 
# but they are clearly in-universe...
Dat           <- Dat[, p_wt2 := imputeWeights(p_wt, intv_dt), by = list(id) ]
Dat           <- Dat[!is.na(Dat$p_wt2),]


# decimal values for chrono-thano age
Dat$age       <- getChronoAge(Dat$intv_dt, Dat$b_dt)
Dat$ttd       <- getThanoAge(Dat$intv_dt, Dat$d_dt)

# single age classes (not used)
Dat$ca        <- floor(Dat$age)
Dat$ta        <- floor(Dat$ttd)

# make 5 year cohorts
Dat$coh5      <- Dat$b_yr - Dat$b_yr %% 5

# Coh5keep inlcudes neighboring outside cohorts for help fitting
Coh5keep <- c(1900, 1905, 1910, 1915, 1920, 1925, 1930)

# these are the cohorts we predict for
Coh5     <- c(1905, 1910, 1915, 1920, 1925) 
Dat      <- Dat[Dat$coh5 %in% Coh5keep, ]

Dat$srhpoor         <- ifelse(Dat$srh == "5. poor",1,ifelse(Dat$srh == "NA",NA,0))


plot(jitter(Dat$srhpoor) , jitter(Dat$ttd))

#Dat[, srhpoor := imputeSkippedQuestions(srhpoor,intv_dt), by = list(id) ]


#rescale <- function(var,Dat,compelment = FALSE){
#	Dat[[var]] <- Dat[[var]] / max(Dat[[var]], na.rm = TRUE)
#	if (compelment){
#		Dat[[var]] <- 1 - Dat[[var]]
#	}
#	Dat
#}
#Dat     <- rescale("adl3_", Dat, FALSE)
#Dat     <- rescale("iadl3_", Dat, FALSE)
#Dat     <- rescale("adl5_", Dat, FALSE)
#Dat     <- rescale("iadl5_", Dat, FALSE)
#
#setnames(Dat,"adl5_","adl5")
#setnames(Dat,"iadl5_","iadl5")
#setnames(Dat,"adl3_","adl3")
#setnames(Dat,"iadl3_","iadl3")
## let's just smooth these in AP.

Dat$DateDec <- lubridate::decimal_date(Dat$intv_dt)

# we want to be clearly in Gompertzlandia
Dat <- Dat[Dat$age > 65, ]
varname <- "srhpoor"
# now we do a simplistic smooth of the cloud of points in the space.
FitLoess <- function(varname, 
		Dat, 
		sex,
		t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
		c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
		span = .5, # will vary
		.Coh5){
	# conservative here to cut tails
	maxL  <- 100
	minL  <- 70
	
	# multiplicative give the most freedom.
	mod   <- loess(paste0(varname,'~coh5 * ta * ca') ,
			data = Dat[Dat$sex == sex, ], 
			weights = p_wt2, # this, plus point density both act as weights
			span = span,     # a variable passed in, or smoothness
			# is similiar conceptually to a 1:1:1 aspect ratio. Everything is in years...
			normalize = FALSE,
			control = loess.control(trace.hat="approximate")
	)
	
	newdata        <- expand.grid(ta = t.age, ca = c.age, coh5 = .Coh5)
	# easier to keep dimensions straight if we predict over rectangular grid, 
	# then throw out values outside range
	#newdata        <- newdata + .5
	Surf           <- predict(mod, newdata)
	
	dimnames(Surf) <- list(floor(t.age),floor(c.age), .Coh5)
	
	# need to trim on the left side where applicable, since some questions didn't enter until
	# wave 2 or 3. There are many such cases, so check and make sure we dont' extrapolate.
	# sex <- "m"
	# varname <- "cesd"
	MissingWaves <- tapply(Dat[Dat$sex==sex,varname],Dat[Dat$sex==sex,"wave"], function(x){
				all(is.na(x))
			})
	RightYear <- 2011; LeftYear <- 1992
#	# this reduces extrapolation outside of data points 
	for (i in 1:dim(Surf)[3]){
		#maxL  <- 2011 - Coh5[i] - 1
		#maxt  <- tamax[as.character(Coh5[i])]
		#keept <- as.integer(rownames(Surf)) <= maxt
		A     <- Surf[,,i]
		MaxL <- RightYear - .Coh5[i] - 1
		A[ col(A) - 1 + 70 + row(A) - 1 > MaxL] <- NA
# possibly need to trim lower left corner too: dimnames(A)
		MinL <- LeftYear - (.Coh5[i] + 5)
		A[col(A) + 70 - 1 < MinL] <- NA
		#A[!keept, ] <- NA 
		Surf[,,i] <- A
	}
	list(Surf = Surf, span = span, sex = sex, varname = varname, Cohorts = Coh5)
}


Female <- FitLoess(varname = "srhpoor", 
				Dat = Dat, 
				sex = "f",
				t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
				c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
				span =  .5, 
				.Coh5 = Coh5)

Male <- FitLoess(varname = "srhpoor", 
				Dat = Dat, 
				sex = "m",
				t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
				c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
				span =  .5, 
				.Coh5 = Coh5)

SRHPOOR <- list(Male = Male, Female = Female)
save(SRHPOOR, file = "Data/srhpoor.Rdata")

SRHPOOR <- local(get(load("Data/srhpoor.Rdata")))
source("/home/tim/git/APCT/APCT/R/SurfMap.R")

# these are the plots to be arranged in a panel for the paper
for (i in 1:5){
	pdf(file.path("Figures/TALapplication",paste0("srhpoor",Coh5[i],".pdf")), width=9,height=5)
# dev.new(width=9,height=5)
	par(mai=c(.4,.4,.1,.1),xpd=TRUE)
	plot(NULL, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(70,100), ylim = c(0,15), asp = 1)
	SurfMap(SRHPOOR$Male$Surf[, , i], 
			ticks = seq(0, .5, by = .05), 
			outline = FALSE, 
			ylab = "",
			xlab = "", 
			bg = TRUE, 
			add = TRUE,
			legnd = FALSE,
			yshift = 0,
			cex=1.5)
	text(85,-2,"Chronological age",cex=1.5)
	text(67.75,7.6,"Thanatological age", srt = 90,cex=1.5)
	dev.off()
}

# make a legend in a pdf of the same dimension:
ticks <-seq(0, .5, by = .05)
colramp <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "OrRd")), space = "Lab")
col   <- rev(colramp(length(ticks)-1))
ticksat <- (ticks - min(ticks))/diff(range(ticks)) * 12 
pdf("Figures/TALapplication/Legend.pdf", width=9,height=5)
# dev.new(width=9,height=5)
par(mai=c(.4,.4,.1,.1),xpd=TRUE)
plot(NULL, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(70,100), ylim = c(0,15), asp = 1)
rect(70,ticksat[-length(ticks)],71.5,ticksat[-1],col=col, border = gray(.6))
text(71.5, ticksat,ticks,pos=4,cex=1.5)
text(71,14,"Prevalence",cex=1.5)
dev.off()


str(SRHPOOR)
# for PAA presentation:
Coh5 <- SRHPOOR[[1]]$Cohorts
for (i in 1:5){
	pdf(file.path("PAApresentation/Figures",paste0("srhpoor",Coh5[i],".pdf")), width=9,height=5)
# dev.new(width=9,height=5)
	par(mai=c(.6,.6,.1,.1),xpd=TRUE)
	plot(NULL, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(70,100), ylim = c(0,15), asp = 1)
	SurfMap(SRHPOOR$Male$Surf[, , i], 
			ticks = seq(0, .5, by = .05), 
			outline = FALSE, 
			ylab = "",
			xlab = "", 
			bg = TRUE, 
			add = TRUE,
			legnd = FALSE,
			yshift = 0,
			cex=1.5)
	text(85,-2.5,"Chronological age",cex=1.5)
	text(67,7,"Time to death", srt = 90,cex=1.5)
	dev.off()
}









#plot(NULL, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(70,100), ylim = c(0,100), asp = 1)
#for (i in 1:5){
#	SurfMap(SRHPOOR$Male$Surf[,,i], 
#			ticks = seq(0, .5, by = .05), 
#			outline = FALSE, 
#			ylab = "",
#			xlab = "", 
#			bg = TRUE, 
#			add = TRUE,
#			legnd = FALSE,
#			yshift = (5-i) * 22)
#}
#



# Now we take a look at a canned example!
#plot(0:12,rowMeans(SRHPOOR$Male$Surf[,,1],na.rm=TRUE), ylim = c(0,.4))
#lines(0:12,rowMeans(SRHPOOR$Male$Surf[,,2],na.rm=TRUE))
#lines(0:12,rowMeans(SRHPOOR$Male$Surf[,,3],na.rm=TRUE))
#lines(0:12,rowMeans(SRHPOOR$Male$Surf[,,4],na.rm=TRUE))
#lines(0:12,rowMeans(SRHPOOR$Male$Surf[,,5],na.rm=TRUE))
#lines(0:12,rowMeans(SRHPOOR$Male$Surf,na.rm=TRUE),col="red")
#
#matplot(0:12,SRHPOOR$Male$Surf[,,1], ylim = c(0,.5), pch=19,col="#00000020")
#matplot(0:12,SRHPOOR$Male$Surf[,,2], ylim = c(0,.5), pch=19,col="#00000020",add=TRUE)
#matplot(0:12,SRHPOOR$Male$Surf[,,3], ylim = c(0,.5), pch=19,col="#00000020",add=TRUE)
#matplot(0:12,SRHPOOR$Male$Surf[,,4], ylim = c(0,.5), pch=19,col="#00000020",add=TRUE)
#matplot(0:12,SRHPOOR$Male$Surf[,,5], ylim = c(0,.5), pch=19,col="#00000020",add=TRUE)
#lines(0:12,rowMeans(SRHPOOR$Male$Surf,na.rm=TRUE),col="red")

# produce generic g(y)
gy <- rowMeans(SRHPOOR$Male$Surf,na.rm=TRUE)
getwd()
library(HMDHFDplus)
source("R/Functions.R")
x  <- 60:110
y  <- 0:40
LT <- readHMDweb("USA","mltper_1x1",username=us,password=pw)
mx2 <- LT$mx[LT$Year == 2010 & LT$Age %in% x]
mx1 <- LT$mx[LT$Year == 1980 & LT$Age %in% x]

lx2 <- mx2lx(mx2)
lx1 <- mx2lx(mx1)
dx2 <- mx2dx(mx2)
dx1 <- mx2dx(mx1)
Lx2 <- mx2Lx(mx2)
Lx1 <- mx2Lx(mx1)
N   <- length(x)
gy  <- c(gy,rep(0,N-length(gy)))

Morb <- May(gy,dx2)

# remaining life expectancy at age 70
(ex2 <- mx2ex(mx2))
(ex1  <- mx2ex(mx1))
ex2-ex1
(eH2 <- geteH(mx2,Morb)  )
(eH1 <- geteH(mx1,Morb))
eH2 - eH1
(eH1 / ex1)
(eH2 / ex2) # higher proportion

# unhealthy, similar but also increased
(ex1 - eH1)
(ex2 - eH2)

################################################
# buy what if we had used the sullivan method based on 
# 1980 age pattern of morbidity fixed, rather than ttd pattern fixed?

ga1         <- mxgay2gaLx(mx1, gy)
ga2         <- mxgay2gaLx(mx2, gy)
eU2sullivan <- sum(ga1 * Lx2)
(eHsullivan  <- ex2 - eU2sullivan)
eU2sullivan - (ex1 - eH1)
# sullivan increase in unhealthy expectancy
100 * eU2sullivan / (ex1 - eH1) # sullivan predicts 56% increase
100 * (ex2 - eH2) / (ex1 - eH1) # real 0% increase