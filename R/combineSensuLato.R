combineSensuLato <- function (x) {
	stopifnot(inherits(x, "Vegsoup"))
	l <- linktaxa::groupSensuLato(vegsoup::taxon(x))
	r <- x
	cat("start loop ")
	#	rather very slow
	for (i in seq_along(l)) {
		cat(".")
		y <- list(from = l[[ i ]], to = names(l)[ i ])
		r <- combine(r, y)		
	}
	return(r)
}
