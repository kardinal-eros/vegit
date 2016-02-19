#	example data
#	x <- t(data.frame(plot = 9:12,
#		value1 = round(abs(rnorm(4)), 1),
#		value2 = round(rnorm(4), 1),
#		value3 = ceiling(abs(rnorm(4) * 10)),
#		value4 = sample(1:100, 4),
#		value5 = c("abcd", "b", "bc", "acd")))
#	x <- cbind(rownames(x), x)
#	dimnames(x) <- NULL
#	x <- gsub(" ", "", x)

#	format horizontal, expand to width
.horizontal <-
function (x, width) {
	stopifnot(is.matrix(x))
	for (i in 1:ncol(x)) {
		#	%s right justyfied
		#	%-s left justyfied
		x[, i] <- sprintf(paste0("%", width, "s"), x[, i])
		dimnames(x) <- NULL
	}
	return(x)
}

#	format vertical, shrink to width 1
.vertical <-
function (x) {
	#	format to equal width, add leading zeros (numbers)
	#	or trailing dots (character strings)

	stopifnot(mode(x) == "character")
	l <- x[, 1]
	w <- apply(t(apply(x[, -1, drop = FALSE], 1, nchar)), 1, max)
	v <- x[, -1, drop = FALSE]
	
	#	width of strings (w) and type of value (t)
	if (nrow(v) > 1) {
		w <- apply(apply(v, 2, function (x) sapply(x, nchar)), 1, max)
	} else {
		w <- max(apply(v, 2, nchar))
	}
	t <- apply(v, 1, function (x) class(type.convert(x)))
	
	for (i in seq_along(l)) {
		if (t[i] == "integer") {
			v[i, ] <- sprintf(paste0("%0", w[i], "d"), as.integer(v[i, ]))
		} else {
			if (t[i] == "numeric") {
				v[i, ] <- gsub("[[:space:]]", ".", sprintf(paste0("%", w[i], "s"), v[i,]))
			} else {
				v[i, ] <- gsub("[[:space:]]", ".", sprintf(paste0("%", w[i], "-s"), v[i,]))
			}
		}
	}
	#	rbind
	m <- c()
	for (i in 1:nrow(v)) {
		ii <- unlist(sapply(v[i, ], strsplit, ""))
		m <- rbind(m, matrix(ii, nrow = w[i], ncol = ncol(v)))
	}
	
	#	assign labels
	m <- cbind(rep.int(l, times = w), m)
	m[duplicated(m[,1]), 1] <- ""
	
	return(m)
}

csv2txt <-
function (x, header.rows, merge.cols, sep = ";", width = 1, vertical = TRUE, collapse = " ", overwrite = FALSE, ...) {
	if (missing(header.rows))
		stop("please define the number of header rows", call. = FALSE)
	else
		n <- as.integer(header.rows)
	
	if (!missing(merge.cols))
		j <- as.vector(merge.cols)
	else
		j <- 1
	
	if (!vertical & missing(width))
		stop("must define with if verital = FALSE")
	
	#	output filename
	f <- gsub(".csv", ".txt", x, fixed = TRUE)
	if (file.exists(f) & !overwrite)
		stop("output file with txt extension exits:\n", f,
			"\nuse overwrite = TRUE", call. = FALSE)
	
	#	read file
	x <- read.csv(x, sep = sep, colClasses = "character", header = FALSE)
	x <- as.matrix(x)
	
	if (j > 1) {
		x1 <- apply(x[ , 1:j ], 1, function (x) paste(x, collapse = "@"))
		x2 <- x[ ,-(1:j) ]
		x <- cbind(x1, x2)
	}
	
	#	taxa block
	#	with or without header
	if (n > 0) {
		h <- as.matrix(x[  c(1:n), ])
		v <- as.matrix(x[ -c(1:n), ])
	} else {
		v <- as.matrix(x[ -1, ]) # read.csv(..., header = FALSE)
	}
	
	#	transform header or plot identifier if present
	if (vertical) {
		h <- .vertical(x[ n, , drop = FALSE])
	}
	if (!vertical) {
		h <- .horizontal(h, width)
		v <- .horizontal(v, width)
	}
	
	#	there may be empty rows seperating blocks?
	b <- v[,1] == ""
	v[v == ""] <- "."
	v[b, ] <- ""
	
	#	combine with header if present, result (r) to be returned by function
	if (n > 0) {
	r <- rbind(h, v)
	r <- t <- as.data.frame(r, stringsAsFactors = FALSE, row.names = 1:nrow(m))
	}
	
	#	format width of first column
	t[, 1] <- sprintf(paste0("%-", max(sapply(x, nchar)) + 1, "s"), t[, 1])

	#	format width of remaining columns
	for (i in 2:ncol(t)) {
		t[, i] <- sprintf(paste0("%", width, "s"), t[, i])
	}
	
	#	save matrix as text
	t <- apply(as.matrix(t), 1, paste0, collapse = "") # argument collapse?
	con <- file(f)
	writeLines(t, con)
	close(con)
	
	#	return matrix
	return(r)
}