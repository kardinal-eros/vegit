#	undocumented and unexported experimatal fucntions
.replaceTaxonRepository <-
function (x, y, dir) {
	if (missing(x) | missing(y)) stop
	xx <- as.character(x)
	yy <- as.character(y)
	
	dirs <- list.files("~/Documents/vegsoup-data", full.names = TRUE)
	
	if (missing(dir)) {
		#	cat("apply changes to all folders")
	} else {
		#	cat("apply changes to all folders")
		dirs <- dir
	}
	
	for (i in dirs) {
		#	i = dirs[1]
		f <- paste(i, "/species.csv", sep = "")
		if (file.exists(f)) {
			ii <- read.csv2(f, colClasses = "character")
			if (length(grep("history", names(ii))) < 1) {
				stop(i, " has no history column")	
			}
			if (length(grep("abbr", names(ii))) < 1) {
				stop(i, " has no abbr column")	
			}
			if (!is.na(match(xx, ii$abbr))) {
				ii$history[ii$abbr %in% xx] <-
					paste(ii$history[ii$abbr %in% xx],
						"| Changed", xx, "To", yy, Sys.time())
				ii$abbr[ii$abbr %in% xx] <- yy
				cat("\n", x, "found in ", i)
				write.csv2(ii, paste(i, "/species.csv", sep = ""), row.names = FALSE)
			} else {
				#	cat("\n", x, "not found in ", i)
			}
		}
	}
}
	
.replace <-
function (path, rpl) {
	#	full data set replacement
	path <- list.files(path, full.names = TRUE)
	rpl <- matrix(rpl, ncol = length(rpl)) # c("dryo vill", "dryo vill vill")
	
	for (j in path) {
		for (i in 1:nrow(rpl)) {
			x <- rpl[i, 1]
			y <- rpl[i, 2]
			replaceTaxonRepository(x, y, dir = path)
		}
	}
	
	#	folder wise replacement
	#	the folder
	path <- "~/Documents/vegsoup-data/weißkirchen dta"
	#	the day
	day <- "2013-05-29"
	#	file with instucions
	file <- paste(path, "/recode ", day, ".txt", sep = "")	
	#	recode2013-05-29.txt
	
	rpl <- as.matrix(read.delim(file, colClasses = "character", header = FALSE))
	
	#	check spaces!
	rpl
}