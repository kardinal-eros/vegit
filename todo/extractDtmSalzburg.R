extractDtmSalzburg <- function (x) {
	stopifnot(inherits(x, "Vegsoup"))
	r <- raster("~/Google Drive/sagis/dtm10m/dtm10m.asc")
	v <- spTransform(x, CRS("+init=epsg:31258"))
	x$elevation <- round(extract(r, v), 0)
	return(x)
}


