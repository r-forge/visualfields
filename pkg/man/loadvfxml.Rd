\name{loadvfxml}
\alias{loadvfxml}
\title{loads visual fields from a XML file}
\description{loads visual fields from a XML file}
\usage{loadvfxml( filename, patternMap, typeData = "vf", typeSubject = "pwg", extractionType = c( "average" ) ) }
\arguments{
  \item{filename}{filename}
  \item{patternMap}{pattern of stimulus locations. Default is \code{saplocmap$p24d2}}
  \item{typeData}{Type of data to load; visual field (\code{vf}), total deviations (\code{td}), pattern deviations (\code{pd}), global indices (\code{gi}), visual-field index (\code{vfi}), total-deviation p-values (\code{tdp}), pattern-deviation p-values (\code{pdp}), global indices probability maps (\code{gip}), visual-field-index probability map (\code{vfip}). Default is \code{vf}}
  \item{typeSubject}{Type of subject, control (\code{ctr}) or patient (\code{pwg}). Default is (\code{pwg})}
  \item{extractionType}{When typeData is (\code{vf}) what type of extraction we want: all re-tested sensitivities \code{"all"} or just the mean \code{"average"} over re-tested values? Default is \code{"average"}}
}
\details{
GIVE HERE HOW THE XLM FILE SHOULD BE FORMATTED
}

\value{returns a data frame in which each row is a visual-field object}

\references{
References?
}

\seealso{ \code{\link{loadvfcsv}}, \code{\link{loadvfxmlbatch}} }

\examples{
#loadvfxml(  )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}