\name{ combine }

\alias{combineSensuLato}

\title{ Pool Taxa in Vegsoup Object }

\description{
  Pool Taxa in Vegsoup Object.
}

\usage{
combineSensuLato(x)
}

\arguments{
  \item{ x }{
    vegsoup object.
  }
}

\details{
  This function can be used to automatically collapse taxa in the \emph{s.lat.} case. The functions internally calls \code{\link{groupSensuLato}} from \pkg{linktaxa}. Run this function alone to see which taxa are going to be merged.
}

\value{
vegsoup object.
}

\author{ Roland Kaiser }

\examples{
require(vegsoup)
require(linktaxa)

data(windsfeld)
x <- windsfeld

# see candidates
groupSensuLato(taxon(x))

# perform replacement
x <- combineSensuLato(x)

#	now list is empty
groupSensuLato(taxon(x))
}

\keyword{ manip }
