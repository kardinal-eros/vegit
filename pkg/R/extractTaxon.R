extractTaxon <- function (x, col, row, blank = "blank") {
	if (missing(col) | missing(row))
		stop ("need starting row and width of chars for taxa block")
	
	stopifnot(inherits(x, "character"))	

	x <- sapply(x, function (x) substring(x, 1, col), USE.NAMES = FALSE)
	x <- x[row:(length(x))]
	x <- str_trim(x)
	x[x == ""] <- blank

	#	skip keywords, if present
	i <- c("BEGIN HEADER", "END HEADER", "BEGIN TABLE", "END TABLE")
	i <- unlist(sapply(i, grep, x))
	
	if (length(i) > 0)	x <- x[-i[length(i) > 0]]
	if (any(is.na(x)))
		stop("NAs introduced")
	else	
	return(x)
}




