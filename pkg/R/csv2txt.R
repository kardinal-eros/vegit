csv2txt <- function (x, header.rows, merge.cols, sep = ";", width = 1, vertical = TRUE, collapse = " ", overwrite = FALSE, ...) {
	if (missing(header.rows))
		stop("please define the number of header rows", call. = FALSE)
	else
		n <- as.integer(header.rows)
	
	if (!missing(merge.cols))
		j <- as.vector(merge.cols)
	else
		j <- 1
	
	if (!vertical & missing(width))
		stop("must define with for verital = FALSE")
		
	#	output filename
	f <- gsub(".csv", ".txt", x, fixed = TRUE)
	if (file.exists(f) & !overwrite)
		stop("output file with txt extension exits:\n", f,
			"\nuse overwrite = TRUE", call. = FALSE)
	
	#	read file
	x <- read.csv(x, sep = sep, colClasses = "character", header = FALSE)#, ...)
	
	if (length(j) > 1) {
		x1 <- apply(x[, j], 1, function (x) paste(x, collapse = " "))
		x2 <- x[ ,-j]
		x <- as.matrix(cbind(x1, x2))
	}
	
	#	transform header
	#	format to equal width, add leading zeros (numbers)
	#	or trailing dots (character strings)
	l <- as.matrix(x)[1:n,  1, drop = FALSE ] # labels	
	v <- as.matrix(x)[1:n, -1, drop = FALSE ] # values
		
	if (vertical) {
		#	width of strings (w) and type of value (t)
		if (nrow(v) > 1) {
			w <- apply(apply(v, 2, function (x) sapply(x, nchar)), 1, max)
			t <- apply(v, 1, function (x) class(type.convert(x)))
		} else {
			w <- apply(v, 2, nchar)
			t <- sapply(v, function (x) class(type.convert(x)))		
		}
		for (i in seq_along(l)) {
			if (t[i] == "integer")
				v[i, ] <- sprintf(paste0("%0", w[i], "d"), as.integer(v[i, ]))	
			else
				v[i, ] <- gsub("[[:space:]]", ".", sprintf(paste0("%", w[i], "s"), v[i,]))
		}

		#	rbind
		m <- c()
		for (i in 1:nrow(v)) {
			ii <- unlist(sapply(v[i, ], strsplit, ""))
			m <- rbind(m, matrix(ii, nrow = w[i], ncol = ncol(v)))
		}
		
		#	assign labels
		if (n > 1) {
			m <- cbind(rep.int(l, times = w), m)
			m[duplicated(m[,1]), 1] <- ""
		} else {
			m <- cbind(l, m)
		}
	}
	
	if (!vertical) {	
		for (i in 1:ncol(v)) {
			#	%s right justyfied
			#	%-s left justyfied
			v[, i] <- sprintf(paste0("%", width, "s"), v[, i])
			m <- v
			dimnames(m) <- NULL
		}
		#	assign labels		
		m <- cbind(l, m)
	}
				
	
	#	taxa block
	#	there may be empty rows seperating blocks?
	v <- as.matrix(x[-c(1:n), ])
	b <- v[,1] == ""	
	v[v == ""] <- "."
	v[b, ] <- ""

	#	combine with header, result (r) to be returned by function
	r <- rbind(m, v)
	r <- t <- as.data.frame(r, stringsAsFactors = FALSE, row.names = 1:nrow(m))
	
	#	transform to text
	
	#	format width of first column
	t[, 1] <- sprintf(paste0("%-", max(sapply(x, nchar)) + 1, "s"), t[, 1])

	#	format width of remaining columns
	for (i in 2:ncol(t)) {
		t[, i] <- sprintf(paste0("%", width, "s"), t[, i])
	}
		
	#	save matrix as text
	t <- apply(as.matrix(t), 1, paste0, collapse = "") # arggument collapse 
	con <- file(f)
	writeLines(t, con)
	close(con)

	#	return matrix
	return(r)
}