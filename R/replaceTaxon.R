#	keep keywords in sync with read.verbatim() in vegsoup


replaceTaxon <- function (x, y, z, schema = c("taxon", "abbr"), col, row = NULL, file, keywords = FALSE, overwrite = FALSE) {
	
	stopifnot(inherits(x, "character"))
	
	if (inherits(y, "data.frame") & inherits(z, "data.frame")) {
		stopifnot(ncol(y) >= 2)
		if (length(match(schema, names(z))) != 2)
			stop("need to find schema columns")				
	}
				
	if (missing(col))
		stop("need width (number of chars) for taxa block")

	if (!missing(file)) {
		if (file.exists(file) & !overwrite)
			stop("output file exits:\n", file,
				"\nuse overwrite = TRUE", call. = FALSE)
	}

	#	read.verbatim keywords
	k <- c("BEGIN HEAD", "END HEAD", "BEGIN TABLE", "END TABLE")
		
	#	assign abbreviations	
	y$abbr <- z$abbr[ match(y[, 2], z$taxon) ]

	#	break apart
	x <- splitArray(x, col = col, row = row)
		
	#	taxa block
	if (!is.null(row)) {
		h <- x[[1]][[1]]           # header variables
		t <- str_trim(x[[1]][[2]]) # taxa
		v <- x[[2]][[1]]           # header values
		a <- x[[2]][[2]]           # abundances		
	} else {
		t <- str_trim(x[[1]])
		v <- x[[2]]
	}	

	#	replace
	for (i in seq_along(y$abbr))
		t <- gsub(y$taxon[i], y$abbr[i], t, fixed = TRUE)
	
	#	minimal indent of taxa (and header) block
	if (!is.null(row)) {		
		w <- max(c(nchar(t), nchar(str_trim(h))))
		t <- str_pad(t, width = w, side = "right")
		h <- str_pad(str_trim(h), width = w, side = "right")
	} else {
		w <- max(nchar(t))
		t <- str_pad(t, width = w, side = "right")
	}		
	
	#	assemble
	if (!is.null(row)) {
		if (keywords) {
			r <- c(paste(c(k[1], h, k[2]), c("", v, "")),
				   paste(c(k[3], t, k[4]), c("", a, ""))) 
		} else {
			r <- paste(c(h, t), c(v,a))
		}
	} else {
		r <- paste(t, v)		
	}
	#	set empty lines
	r <- gsub("[[:space:]]*$", "", r)

	#	write file
	if (!missing(file)) {
		con <- file(file)
			writeLines(r, con)
		close(con)
	}
	return(r)
}




