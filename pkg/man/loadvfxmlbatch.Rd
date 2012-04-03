\name{loadvfxmlbatch}
\alias{loadvfxmlbatch}
\title{loads visual fields from a set of XML files}
\description{loads visual fields from a set of XML files}
\usage{loadvfxmlbatch( filename, patternMap, typeData = "vf" ) }
\arguments{
  \item{filename}{filename}
  \item{patternMap}{pattern of stimulus locations. Default is \code{saplocmap$p24d2}}
  \item{typeData}{Type of data to load; visual field (\code{vf}), total deviations (\code{td}), pattern deviations (\code{pd}), global indices (\code{gi}), visual-field index (\code{vfi}), total-deviation p-values (\code{tdp}), pattern-deviation p-values (\code{pdp}), global indices probability maps (\code{gip}), visual-field-index probability map (\code{vfip}). Default is \code{vf}}
}
\details{
loads visual fields from a set of XML files. The CSV file must consist of two columns: col 1 with suitable paths for loading XML file and col 2 with the type of subject \code{"pwg"} or \code{"ctr"}
}

\value{returns a data frame in which each row is a visual-field object}

\references{
References?
}

\seealso{ \code{\link{loadvfcsv}}, \code{\link{loadvfxml}} }

\examples{
#loadvfxmlbatch(  )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}