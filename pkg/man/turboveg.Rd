\name{ Turboveg }

\alias{turbovegLayers}

\title{ Utility Functions Related to Turboveg }

\description{
  The function converts numeric layer codes to their respective abbreviations.
}

\usage{
turbovegLayers(x, no.layer = "0l")
}

\arguments{
  \item{ x }{
    \code{Vegsoup}, object of class \code{"Vegsoup"}
  }
  \item{ no.layer }{
    \code{character}, the value to assign for code 0.
  }
}

\details{
The notation to encode layers in \emph{Turboveg} and their translation is as follows.
  \tabular{lll}{
    code  \tab description \tab translation \cr	  
	0 \tab No layer \tab 0l \cr
	1 \tab Tree layer (high) (t1) \tab tl1 \cr
	2 \tab Tree layer -middle- (t2) \tab tl2 \cr
	3 \tab Tree layer -low- (t3) \tab tl3 \cr
	4 \tab Shrub layer (high) (s1) \tab sl1 \cr
	5 \tab Shrub layer -low- (s2) \tab sl2 \cr
	6 \tab Herb layer (hl) \tab hl \cr
	7 \tab juvenile (jl) \tab jl \cr
	8 \tab seedling (< 1 year) (sl) \tab hl \cr
	9 \tab Moos layer (ml) \tab ml \cr
  }
}

\value{
Vegsoup object
}

\author{ Roland Kaiser }

\examples{
# Nothing here yet
}

\keyword{ manip }
