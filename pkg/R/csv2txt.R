csv2txt <- function (x, header.rows, sep = ";", overwrite = FALSE, ...) {
	if (missing(header.rows))
		stop("please define the number of header rows", call. = FALSE)
	else
		n <- as.integer(header.rows)

	#	output filename
	f <- gsub(".csv", ".txt", x, fixed = TRUE)
	if (file.exists(f) & !overwrite)
		stop("output file with txt extension exits:\n", f,
			"\nuse overwrite = TRUE", call. = FALSE)
	
	#	read file
	x <- read.csv(x, sep = sep, colClasses = "character", header = FALSE, ...)

	#	transform header
	#	format to equal width, add leading zeros (numbers)
	#	or trailing dots (character strings)
	l <- as.matrix(x)[1:n, 1]  # labels	
	v <- as.matrix(x)[1:n, -1] # values
	
	#	width of strings
	w <- apply(apply(v, 2, function (x) sapply(x, nchar)), 1, max)
	
	#	type of value
	t <- apply(v, 1, function (x) class(type.convert(x)))
	
	for (i in seq_along(l)) {
		if (t[i] == "integer")
			v[i, ] <- sprintf(paste0("%0", w[i], "d"), as.integer(v[i, ]))	
		else
			v[i, ] <- gsub("[[:space:]]", ".", sprintf(paste0("%-", w[i], "s"), v[i,]))
	}
	
	#	for ease with use rbind
	m <- c()
	for (i in 1:nrow(v)) {
		ii <- unlist(sapply(v[i, ], strsplit, ""))
		m <- rbind(m, matrix(ii, nrow = w[i], ncol = ncol(v)))
	}
	
	#	assign labels
	m <- cbind(rep.int(l, times = w), m)
	m[duplicated(m[,1]),1] <- ""
	
	#	taxa block
	#	there may be empty rows seperating blocks?
	v <- as.matrix(x[-c(1:n), ])
	b <- v[,1] == ""	
	v[v == ""] <- "."
	v[b, ] <- ""

	#	combine with header, result (m) to be returned by function
	m <- rbind(m, v)
	m <- t <- as.data.frame(m, stringsAsFactors = FALSE, row.names = 1:nrow(m))
	
	#	transform to text
	
	#	format width of first column
	t[, 1] <- sprintf(paste0("%-", max(sapply(x, nchar)) + 1, "s"), t[, 1])
	#	as text
	t <- apply(t, 1, function (x) paste0(unlist(strsplit(x, ""))))
	t <- sapply(t, paste0, collapse = "")
	con <- file(f)
	writeLines(t, con)
	close(con)

	return(m)
}