combineSensuLato <- function (x) {
	stopifnot(inherits(x, "Vegsoup"))
	l <- linktaxa::groupSensuLato(vegsoup::taxon(x))
	r <- x
	ii <- seq_along(l)
	#	rather very slow
	pb <- txtProgressBar(min = 0, max = length(ii),
	char = '.', width = 45, style = 3)
	for (i in ii) {
		setTxtProgressBar(pb, i)
		y <- list(from = l[[ i ]], to = names(l)[ i ])
		r <- combine(r, y)		
	}
	close (pb)
	return(r)
}
