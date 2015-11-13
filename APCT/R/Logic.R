
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

Identity <- function(set, verbose = TRUE){
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
	if (verbose){
		if (length(set) == 2) cat("uninformative dyad\n")
		if (length(set) == 3) cat("triad identity\n")
		if (length(set) == 6) cat("hexad identity\n")
	}
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

# now to figure out diagonal direction (upward or downward slope)

# A + C == P
# A + T == L
# P + T == D
# L + C == D

# a function to get the derived measure, if any
derived <- function(set){
	set   <- toupper(set)
	setdiff(Identity(set,FALSE),set)
}

# check set membership while maintaining dimensions of left side matrix
`%indim%` <- function(mat,x){
	hm <- mat %in% x
	dim(hm) <- dim(mat)
	hm
}

# return a list with x,y, derived (if any), and whether the derived measure is increasing or decreasing.
dyadaxes <- function(x = "P", y = "A", verbose = TRUE){
	dyad <- c(x, y)
	der  <- derived(dyad)
	if (length(der) == 0){
		if (verbose){
			cat("Diagram\n",
					y,"|           \n",
					" |           \n",
					" |           \n",
					" |           \n",
					" |___________\n",
					"       ", x, "\n"
					, sep = "")
		}
		return(invisible(list(x = x, y = y, derived = NA, increasing = NA)))
	}
	this.order <- c(x, y, der)
	# these are the orderings: always 
	# A + C == P
	# A + T == L
	# P + T == D
	# L + C == D
	ids.ordered <- matrix(c("A", "C", "P",
					"A", "T", "L",
					"P", "T", "D",
					"L", "C", "D"), nrow = 4, byrow = TRUE)
	id <- which(rowSums(ids.ordered %indim% this.order) == 3)
	base.order <- ids.ordered[id, ]
	xi <- which(base.order == x)
	yi <- which(base.order == y)
	# if xi & yi are < 3, then it's a descending diagonal
	# otherwise increasing upward to right.
	if (all(c(xi,yi) < 3)){
		# descending
		if (verbose){
			cat("Diagram\n",
					y,"|⋱          \n",
					" |  ⋱       \n",
					" |    ⋱  ", der, "\n",
					" |      ⋱ \n",
					" |________⋱\n",
					"       ", x, "\n"
					, sep = "")
		}
		return(invisible(list(x = x, y = y, derived = der, increasing = FALSE)))
	} else {
		# increasing
		if (verbose){
			cat("Diagram\n",
					y,"|        ⋰  \n",
					" |      ⋰   \n",
					" |    ⋰ ", der, "\n",
					" |  ⋰     \n",
					" |⋰__________\n",
					"       ", x, "\n"
					, sep = "")
		}
		return(invisible(list(x = x, y = y, derived = der, increasing = TRUE)))
	}
}


#dyadaxes("L","D")

