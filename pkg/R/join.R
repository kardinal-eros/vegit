join <- function (x, y, make.names = TRUE) {
	j <- match(c("taxon", "matched.taxon"), names(x))
	stopifnot(length(j) == 2)
	x <- x[, j]
	stopifnot(nrow(unique(x)) == nrow(x))
	
	j <- match(c("abbr", "taxon"), names(y))
	stopifnot(length(j) == 2)
	y <- y[, j]
	
	x$abbr <- y$abbr[match(x$matched.taxon, y$taxon)]
	
	if (make.names) {
		x$abbr <- make.names(x$abbr)
		x$matched.taxon <- make.names(x$matched.taxon)
		x$taxon <- make.names(x$taxon)
	}
	
	return(x)
}