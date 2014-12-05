\name{ tables }

\alias{csv2txt}
\alias{extractTaxon}
\alias{replaceTaxon}
\alias{splitArray}
\alias{trimTaxon}

\title{ Data as Text Arrays }

\description{
  This is a set of functions that help in the process of developing \code{Vegsoup} objects
  as defined in the \pkg{vegsoup} package from scanned vegetation tables.
  
  The functions operate on CSV files, most likely output
  from OCR software, or on text arrays, themselves possibly developed from OCR
  transcripts. Text arrays, in this context, are tabular respectively matrix like plain text
  files. Rows (lines) give species. Positions along each line, the columns, give the plots
  respectively the abundance/presence of a species in a plot. As a consequence, in a file
  corresponding to this type of text array each species with it's abundances in plots
  consist of a single line. For a particular plot all the abundances for the therein
  occurring species have width of one letter and each plot aligns vertically.
  There might also be a header part containing information on the sampling
  units, not the species. At least the header includes the plot names.
}

\usage{
csv2txt(x, header.rows, merge.cols, sep = ";", width = 1, vertical = TRUE, collapse = " ", overwrite = FALSE, ...)

extractTaxon(x, col, row, blank = "blank")

replaceTaxon(x, y, z, schema = c("taxon", "abbr"), col, row = NULL,
             file, keywords = FALSE, overwrite = FALSE)

splitArray(x, col, row = NULL, blank = "blank")

trimTaxon(x, pad = "@hl", sep = ";")
}

\arguments{
  \item{ x }{
    \code{character}. Text array with species abundances. (See \sQuote{Details}).
  }
  \item{ y }{
    \code{data.frame}. Two column matrix of taxon matches.
    See \code{\link[linktaxa:linktaxa]{linktaxa}}. First column is giving the source,
    second column the matched taxon.
  }
  \item{ z }{
    \code{data.frame}. Taxonomic reference list, must hold literal column \code{"abbr"}.
  }
  \item{ row, col }{
    \code{integer}. \code{integer}. Row (lines) and column (position along a line) positions
    of a text array (\emph{cursor coordinates}). \code{col} defines the width of characters
    (\code{\link{nchar}}) for splitting horizontally, \code{row} sets the vertical split.
  }
  \item{ header.rows }{
    \code{integer}. Like above, the number of rows the header part claims
    in the table.
  }
  \item{ merge.cols }{
    \code{integer}. Merge two or more columns into one (experimental!).
  }
  \item{ width }{
    \code{integer}. Additional space to adjust column width.
  }
  \item{ vertical }{
    \code{logical}. Format header horizontal, then must define width.
  }
  \item{ collapse }{
    \code{character}. Currently not used.
  }
  \item{ schema }{
    \code{character}. Column names to queried from argument \code{z}.
  }
  \item{ keywords }{
    \code{logical}. If \code{file} is given and \code{keywords = TRUE} wrap header
    and taxa block into paired keywords (BEGIN HEAD, END HEAD, BEGIN TABLE, END TABLE).
  }
  \item{ blank }{
    \code{character}. Value to be filled in instead of NAs.
  }
  \item{ sep }{
    \code{character}. Separator for \code{\link{read.csv}}.
  }
  \item{ pad }{
    \code{character}. Padding string to species names.
  }
  \item{ file }{
    \code{logical}. Output file name.
  }
  \item{ overwrite }{
    \code{logical}. Overwrite existing files.
  }
  \item{ \dots }{
    arguments passed to \code{\link{read.csv}}.
  }
}

\details{

Give summary.

\describe{

\item{ \code{splitArray} }{
splits taxa and abundance part of text arrays and returns them as lists. The
functions demands an argument \code{col}. If also the \code{row} argument is given,
\code{splitArray} will return a list of lists. First the header part, then the
taxa block. If only \code{col} is supply, and it must, the functions splits
the text array into first species and second abundances part, left and right
blocks of columns in the array respectively.
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
against a reference list. In order to accomplish this, three objects need
to be supplied (see \sQuote{Arguments}). If \code{keywords = TRUE} the functions
places the keywords searched by \code{read.verbatim} of package \pkg{vegsoup}.
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
