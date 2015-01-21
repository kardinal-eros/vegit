\name{ coordinates }

\alias{char2dd}

\title{ Clean Coordiantes Formats }

\description{
  The function converts coordinates stored as degree, minute, decimal second values (\emph{dms}) to decimal degrees (\emph{dd}).
}

\usage{
char2dd(x, chd = "°", chm = "\"", chs = "\"\"", NS = TRUE)
}

\arguments{
  \item{ x }{
    \code{character}, vector of coordinates
  }
  \item{ chd }{
    \code{character}, degree character terminator
  }
  \item{ chm }{
    \code{character}, minute character terminator
  }
  \item{ chs }{
    \code{character}, second character terminator
  }
  \item{ NS }{
    \code{logical}, \code{TRUE} for north/south, \code{FALSE} for east/west decimal degrees
  }  
}

\details{
Argument \code{x} may be composed of mixed \emph{dms} and \emph{dd} values. Quotation marks need to be escaped (\emph{"} \code{"\""}).
}

\value{
numeric
}

\author{ Roland Kaiser }

\examples{
# Nothing here yet
}

\keyword{ manip }
