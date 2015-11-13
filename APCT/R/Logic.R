
# Author: tim
###############################################################################

# I want to make an APCTDL logic thing. You give it measures in and it returns
# the set of knowable measures.

# identity functions: give and if the identity in question can add any
# time measure to the set, then it does.

#' Triad Identity union functions
#'
#' @aliases triads
#' 
#' @description checks the given set of measures to see if any of the four triad identities can add another measure. Returns the union of the given set and whatever the identity in question can add to it. The identity union functions include: \code{APC()},\code{TPD()},\code{TAL()},\code{LCD()}. These are called by \code{Identity()} and \code{derived()}.
#'  
#' 
#' @param set a vector of any subset of \code{"A","P","C","T","D","L"}
#' @return an alphabetical vector of the union of the given set and any measure that the given identity can contribute to it.
#' @export
#' @name triads
#' 
#' @examples
#' \dontrun{
#' APC(c("A","C"))
#' TPD(c("D","A","T"))
#' ATL(c("D","A","T"))
#' TAL(APC(LCD(c("D","L","A"))))
#' Identity(c("D","L","A"))
#' }
NULL
#' 
#' @rdname triads
APC <- function(set){
	set <- toupper(set)
	id  <- c("A","P","C")
	if (sum(set %in% id) >= 2){
		set <- union(set, id)
	}
	set
}
#'
#' @rdname triads
TPD <- function(set){
	set <- toupper(set)
	id  <- c("T","P","D")
	if (sum(set %in% id) >= 2){
		set <- union(set, id)
	}
	set
}
#' @rdname triads
TAL <- function(set){
	set <- toupper(set)
	id  <- c("T","A","L")
	if (sum(set %in% id) >= 2){
		set <- union(set, id)
	}
	set
}
#' @rd name triads
LCD <- function(set){
	set <- toupper(set)
	id  <- c("L","C","D")
	if (sum(set %in% id) >= 2){
		set <- union(set, id)
	}
	set
}

#' return the union of a set and its derived measures, if any
#' 
#' @description given a vector of time indices, what are all the knowable time indices?
#' 
#' @param set a vector of any subset of \code{"A","P","C","T","D","L"} 
#' @param verbose do you want the function to id what kind of relationship we obtain (in console)?
#' @return an alphabetical vector of the union of the given set and any measures that can be derived from it.
#' 
#' @export 
#' @examples
#' \dontrun{
#' # uninformative dyad
#' Identity(c("A","D"))
#' # informative dyad
#' Identity(c("A","T"))
#' # identity (uninformative triad)
#' Identity(c("A","T","L"))
#'# hexad identity
#' Identity(c("A","T","D"))
#' }
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



# now to figure out diagonal direction (upward or downward slope)

# A + C == P
# A + T == L
# P + T == D
# L + C == D

#' return the union of a set and its derived measures, if any
#' 
#' @description given a vector of time indices, what are all the derivable time indices?
#' 
#' @param set a vector of any subset of \code{"A","P","C","T","D","L"}
#' @return an alphabetical vector of the set of time measures that can be derived from it.
#' 
#' @export 
#' @examples
#' \dontrun{
#' # uninformative dyad
#' derived(c("A","D"))
#' # informative dyad- gives L
#' derived(c("A","T"))
#' # identity (uninformative triad)- nothing to add
#' derived(c("A","T","L"))
#'# hexad identity- adds C,P,L
#' derived(c("A","T","D"))
#' }
derived <- function(set){
	set   <- toupper(set)
	setdiff(Identity(set,FALSE),set)
}


#' check set membership while maintaining dimensions of left side matrix
#' @description a utility function. Like \code{%in%}, but keeps dims on left
#' @param mat a matrix, or anything that can be given to \code{%in%}
#' @param the object against which membership is checked.
#' @return a logical, same dimensions as \code{mat}
#' 
#' @export 
#' 
#' @examples 
#' \dontrun{
#' matrix(c("A","D","L","T"),2) %indim% c("A","T","C")
#' }
`%indim%` <- function(mat,x){
	hm <- mat %in% x
	dim(hm) <- dim(mat)
	hm
}


#'
#' provides basic diagram setup for a given two measures
#' 
#' @description Does a given dyad have a derived measure? If so, where do we draw it? This function gives the metadata required to answer that question, and optionally draws a cheap example diagram in the console. The full set of measures is \code{"A","P","C","T","D","L"}. The function might be called for the list it returns or for the diagram drawn in the console.
#' 
#' @param x, which variable is in the x axis of the diagram? Default \code{"P"} per the standard Lexis diagram.
#' @param y, which variable is in the y axis of the diagram? Default \code{"A"} per the standard Lexis diagram.
#' @param verbose logical. Shall we draw an example in the console?
#' @return an (invisibly returned) list with four components: what is x, y, the derived measure, and whether the derived measure ascend or descends.
#' @export
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

#dyadaxes(x="P",y="A")
#dyadaxes(x="L",y="D")
#dyadaxes(x="C",y="T")
# APCTDL

