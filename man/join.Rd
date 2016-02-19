\name{ join }

\alias{join}

\title{ Join data }

\description{
  Join abbreviations form a taxonomic reference list.
}

\usage{
join(x, y, schema = list(x = c("taxon", "matched.taxon"), y = c("abbr", "taxon")), make.names = TRUE)
}

\arguments{
  \item{ x }{
    \code{data.frame}. Table to be matched against (argument \code{y}).
  }
  \item{ y }{
    \code{data.frame}. Table were matches of \code{x} are sought.
  }
  \item{ schema }{
    \code{list}. List with column names to be searched for.
  }
  \item{ make.names }{
    \code{logical}. See \code{\link{make.names}}.
  }

}

\details{    
  \tabular{ll}{
  taxon \tab abbr \cr
  matched.taxon \tab taxon \cr
  }
}

\value{
data.frame
}

\author{ Roland Kaiser }

\examples{
# Nothing here yet
}

\keyword{ manip }
