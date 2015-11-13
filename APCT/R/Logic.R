
# Author: tim
###############################################################################

# I want to make an APCTDL logic thing. You give it measures in and it returns
# the set of knowable measures.

# identity functions: give 
APC <- function(set){
	set <- toupper(set)
	id  <- c("A","P","C")
	if (sum(set %in% id) >= 2){
		set <- union(set, id)
	}
	set
}
TPD <- function(set){
	set <- toupper(set)
	id  <- c("T","P","D")
	if (sum(set %in% id) >= 2){
		set <- union(set, id)
	}
	set
}
TAL <- function(set){
	set <- toupper(set)
	id  <- c("T","A","L")
	if (sum(set %in% id) >= 2){
		set <- union(set, id)
	}
	set
}
LCD <- function(set){
	set <- toupper(set)
	id  <- c("L","C","D")
	if (sum(set %in% id) >= 2){
		set <- union(set, id)
	}
	set
}

Identity <- function(set){
	require(magrittr)
	set   <- toupper(set)
	stopifnot(all(set %in% c("A","P","C","T","D","L")))
	NI    <- ni  <- length(set)
	for (i in 1:3){
		set <- APC(set) %>% TPD %>% LCD %>% TAL
		ni  <- length(set)
		if (ni == 6)  break
		if (ni == NI) break
		if (ni > NI)  NI <- ni
		
	}
	if (length(set) == 2) cat("uninformative dyad\n")
	if (length(set) == 3) cat("triad identity\n")
	if (length(set) == 6) cat("hexad identity\n")
	sort(set)
}

# examples:
# uninformative dyad
# Identity(c("A","D"))
# informative dyad
# Identity(c("A","T"))
# identity (uninformative triad)
# Identity(c("A","T","L"))
# hexad identity
# Identity(c("A","T","D"))
