\name{ tables }

\alias{csv2txt}
\alias{extractTaxon}
\alias{replaceTaxon}
\alias{splitTranscript}
\alias{trimTaxon}

\title{ Data as Text Arrays }

\description{
A set of function that help in developing \code{Vegsoup} objects from scanned vegetation tables. The functions operate on csv files, most likely output from
OCR software, or on text arrays, themselves possibly developed from OCR
transcripts. Text arrays are plain text files as could also be produced
using a type writer (monospaced layout). Rows (lines) give species. Positions
along each line, the columns, give the plots. Each plot aligns vertically.
As a consequence, in a file corresponding to this type of text array each species
with it's abundances in plots consist of a single line. There might also be a
header part containing information on the sampling units, not the species. At least
the header includes the plot names.
}

\usage{
csv2txt(x, header.rows, sep = ";", overwrite = FALSE, ...)
extractTaxon(x, n, blank = "blank")
replaceTaxon(x, y, z, schema = c("taxon", "abbr"), where)
splitTranscript(x, where, blank = "blank")
trimTaxon(x, pad = "@hl", sep = ";")
}

\arguments{
  \item{ x }{
    character. Vector of strings where each species with its abundance along plots
    (releves) as vector elements. (See \sQuote{Details}).
  }
  \item{ y }{
    data.frame. Two column matrix of taxon matches.
    See \code{\link[linktaxa:linktaxa]{linktaxa}}.
    See \code{extractTaxon}. First column is giving the source, second column the
    matched taxon.
  }
  \item{ z }{
    data.frame. Taxonomic reference list, must hold literal column \code{"abbr"}.
  }
  \item{ n }{
    integer. Rows (lines) and/or columns (position along a line) of text array
    (\emph{cursor coordinates}).
  }
  \item{ where }{
    integer. Defines the width of characters (\code{\link{nchar}}) for splitting
    the text array into species (left) and abundances (right) part.
  }
  \item{ header.rows }{
    character. Like above, the number of rows the header part claims in the table.
  }
  \item{ schema }{
    character. Column names to queried from argument \code{z}.
  }
  \item{ blank }{
    character. Value to be filled in instead of NAs.
  }
  \item{ sep }{
    character. Separator for \code{\link{read.csv}}.
  }
  \item{ pad }{
    character. Pad to species names.
  }
  \item{ overwrite }{
    logical. Overwrite existing files.
  }
  \item{ \dots }{
    passed to \code{\link{read.csv}}.
  }
}

\details{

Give summary.

\describe{

\item{ \code{splitTranscript} }{
splits taxa and abundance part of text arrays and returns them as lists. The
functions demands an argument \code{where}.
(see also argument \code{n} to functions extractTaxon read csv2txt.
}

\item{ \code{trimTaxon} }{
trims and pads strings of species names as part of a csv file. The
function selects by default the first column.
}

\item{ \code{csv2txt} }{
takes a csv file and transform it to a text array.
Note, file encoding might be an issue. At least it is for german umlaut.
Lines with non ASCII characters will likely have wrong indentation.
There is no handle to cope with decimals in the header for now.
Fortunately, they are rarely present in printed sources.
}

\item{ \code{replaceTaxon} }{
replaces species name of source with an abbr(eviation) queried
against a reference list. In order to accomplish this tasks three objects need
to be supplied (see \sQuote{Arguments}).
}

\item{ \code{extractTaxon} }{
extracts species from text array.
}

}
}

\value{
Depending on the function.
}

\author{ Roland Kaiser }

\examples{
# Nothing here yet
}

\keyword{ manip }
