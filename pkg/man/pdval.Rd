\name{ pdval }
\alias{ pdval }
\title{pattern deviation }
\description{
get pattern-deviation values
}
\usage{
pdval( td )
}
\arguments{
  \item{td}{total-deviation values}
}
\details{
calculates pattern-deviation values using the (around) 85-th percentile. All in dBs.
}

\value{
returns the subjects's pattern-deviation values.
}

\references{
References?
}

\seealso{ \code{\link{pdpmap}}, \code{\link{tdval}}, \code{\link{tdpmap}} }

\examples{
# pdval( c( 1:54 ) )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}