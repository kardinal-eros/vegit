splitTranscript <- function (x, where, blank = "blank") {
	if (missing(where))
		stop ("need width of chars (where) for splitting")
	stopifnot(inherits(x, "character"))
	
	n <- max(sapply(x, nchar, USE.NAMES = FALSE))
	
	s <- sapply(x, function (x) substring(x, 1, where), USE.NAMES = FALSE)
	t <- sapply(x, function (x) substring(x, where + 1, n), USE.NAMES = FALSE)
	return(list(s, t))
}


