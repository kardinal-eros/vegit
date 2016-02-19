trimTaxon <- function (x, pad = "@hl", sep = ";") { # , ...
	if (inherits(x, "character")) {
		x <- read.csv(x, sep = sep)[, 1, drop = FALSE]
		x <- c(names(x), as.character(x[, 1]))
	}	
	else {
		stop()
	}
	
	x <- str_trim(x)
	x <- str_pad(x, width = max(sapply(x, nchar)), side = "right")
	x <- paste(x, pad, sep = "")
	return(x)
}