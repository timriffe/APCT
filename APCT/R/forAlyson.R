
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
.Platform$file.sep
gsub(pattern="APCT",replacement="ThanoEmpirical",this.wd )
Results <- local(get(load(file.path(
								gsub(pattern="APCT",replacement="ThanoEmpirical",this.wd ),
								"Data",
								"LoessQuinquenal_imp.Rdata"))))
names(Results)

# make 2 arrays, a male and a female.
# think some, then send to Alyson around when she gets back from vacay. Maybe Anna O too.
