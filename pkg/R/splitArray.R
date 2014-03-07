splitArray <- function (x, col, row = NULL, blank = "blank") {
	if (missing(col))
		stop ("need width of chars (col) for splitting")
	else
		if (!is.null(row))
			message("split header and taxa block")	
	stopifnot(inherits(x, "character"))
	
	n <- max(sapply(x, nchar, USE.NAMES = FALSE))
	
	s <- sapply(x, function (x) substring(x, 1, col), USE.NAMES = FALSE)
	t <- sapply(x, function (x) substring(x, col + 1, n), USE.NAMES = FALSE)
	
	if (!is.null(row)) {
		s <- list(s[ 1:(row - 1) ], s[ row:length(s)])
		t <- list(t[ 1:(row - 1) ], t[ row:length(t)])		
	}
	return(list(s, t))
}


