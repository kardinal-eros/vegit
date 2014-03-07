extractTaxon <- function (x, n, blank = "blank") {
	if (missing(n))
		stop ("need starting row and width of chars for taxa block")
	stopifnot(length(n) == 2)
	
	stopifnot(inherits(x, "character"))	

	x <- sapply(x, function (x) substring(x, 1, n[2]), USE.NAMES = FALSE)
	x <- x[n[1]:(length(x))]
	x <- str_trim(x)
	x[x == ""] <- blank
	if (any(is.na(x)))
		stop("NAs introduced")
	else	
	return(x)
}




