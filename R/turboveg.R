turbovegLayers <- function (x, no.layer = "0l") {	
	l1 <- 0:9
	l2 <- c(no.layer,
		"tl1", "tl2", "tl3",
		"sl1", "sl2",
		"hl", "jl", "sl",
		"ml")
	
	ll <- data.frame(code = l1, value = l2,
		stringsAsFactors = FALSE)
	
	l <- layers(x)
	
	l <- ll[match(l, ll$code), ]
	r <- layers(x, collapse = l$value)
	
	return(r)
}
