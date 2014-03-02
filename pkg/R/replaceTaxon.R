replaceTaxon <- function (x, y, z, schema = c("taxon", "abbr"), where) {
	if (inherits(y, "data.frame") & inherits(z, "data.frame")) {
		stopifnot(ncol(y) >= 2)
		if (length(match(schema, names(z))) < 2)
			stop("need to find schema columns")				
	}
	else {
		stop("x must be a data.frame")
	}
				
	stopifnot(inherits(x, "character"))
	
	if (missing(where))
		stop("need width (number of chars) for taxa block")
	
	y$abbr <- z$abbr[match(y[,2], z$taxon)]
	#	break
	x <- splitTranscript(x, where = where)
	r <- str_trim(x[[1]])
	for (i in seq_along(y$abbr)) {
		r <- gsub(y$taxon[i], y$abbr[i], r, fixed = TRUE)
	}
	
	r <- str_pad(r, width = max(sapply(r, nchar)), side = "right")
	r <- paste(r, x[[2]])
	#	set empty lines
	r <- gsub("[[:space:]]*$", "", r)
	
	return(r)
}




