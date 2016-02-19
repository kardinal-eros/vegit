join <- function (x, y, schema = list(x = c("taxon", "matched.taxon"), y = c("abbr", "taxon")), make.names = TRUE) {
	# x$taxon equals x[[schema[[1]][1]]]
	# x$matched.taxon equals x[[schema[[1]][2]]]
	# y$abbr equals y[[schema[[2]][1]]]
	# y$taxon equals y[[schema[[2]][2]]]
	
	j <- match(schema[[1]], names(x))
	stopifnot(length(j) == 2)
	x <- x[, j]
	
	j <- match(schema[[2]], names(y))
	stopifnot(length(j) == 2)
	y <- y[, j]
	
	test <- is.na(x)
	if (any(test)) {
		error <- x[which(apply(test, 1, any)), ]
		error <- apply(error, 1, paste, collapse = ": ")		
		stop("NA found in argument x\n",
			ifelse(length(error) > 1, paste(error, collapse = ", "), error),
			call. = FALSE)
	}
	
	if (nrow(unique(x)) != nrow(x)) {
		error <- x[duplicated(x), ]
		stop("duplicates found\n",
			apply(error, 1, paste, collapse = ": "), call. = FALSE)
	}	
	
	i <- match(x[[schema[[1]][2]]], y[[schema[[2]][2]]])
	if (any(is.na(i))) {
		error <- "unmatched elements"
		 x[[schema[[1]][2]]][is.na(i)]
		stop()		
	}
	x$abbr <- y[[schema[[2]][1]]][match(x[[schema[[1]][2]]], y[[schema[[2]][2]]])]
	
	if (make.names) {
		x$abbr <- make.names(x$abbr)
		x[[schema[[1]][2]]] <- make.names(x[[schema[[1]][2]]])
		x[[schema[[1]][1]]] <- make.names(x[[schema[[1]][1]]])
	}
	
	return(x)
}