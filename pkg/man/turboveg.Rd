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
	\code{0} \tab No layer \tab \code{0l} \cr
	\code{1} \tab Tree layer (high) (t1) \tab \code{tl1} \cr
	\code{2} \tab Tree layer -middle- (t2) \tab \code{tl2} \cr
	\code{3} \tab Tree layer -low- (t3) \tab \code{tl3} \cr
	\code{4} \tab Shrub layer (high) (s1) \tab \code{sl1} \cr
	\code{5} \tab Shrub layer -low- (s2) \tab \code{sl2} \cr
	\code{6} \tab Herb layer (hl) \tab \code{hl} \cr
	\code{7} \tab juvenile (jl) \tab \code{jl} \cr
	\code{8} \tab seedling (< 1 year) (sl) \tab \code{hl} \cr
	\code{9} \tab Moos layer (ml) \tab \code{ml} \cr
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
