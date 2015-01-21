#	avoid non-ASCII character, degree sign is "\u00B0"

char2dd <- function (x, chd = "\u00B0", chm = "\"", chs = "\"\"", NS = TRUE) {
	#	convert to default of char2dms in package sp
	#	gsub will instantly convert to character if it is not already
	x <- gsub(chd, "d", x, fixed = TRUE)
	x <- gsub(chm, "'", x, fixed = TRUE)
	
	#	second character is duplicated minute character
	n <- nchar(chs)
	nn <- unlist(strsplit(chs, "*"))
	if (n > 1 & unique(nn) == chm)	
		x <- gsub(paste0(rep(chm, n), collapse = ""), "\"", x, fixed = TRUE)
	else
		x <- gsub(chs, "\"", x, fixed = TRUE)	
		
	i <- sapply(sapply(x, type.convert, simplify = FALSE), is.numeric)
	x1 <- unlist(x)[i]
	x2 <- unlist(x)[!i]
	x2 <- paste0(x2, ifelse(NS, "N", "E"))
	x2 <- char2dms(x2)
	
	r <- vector("numeric", length(x))
	r[i] <- as.numeric(x1)
	r[!i] <- as.numeric(x2)
	return(r)
}