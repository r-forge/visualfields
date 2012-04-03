\name{tdrankadjperc}
\alias{tdrankadjperc}
\title{percentiles for adjusted TD rank curve}
\description{gets percentiles for adjusted TD rank curve}
\usage{tdrankadjperc( td, percentiles = c( 0.5, 1, 2, 5, 95 ), type = c( "quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n" ) )}
\arguments{
  \item{td}{table with total-deviation values}
  \item{percentiles}{percentiles at which to calculate cutoff values}
  \item{type}{see \code{\link{wtd.quantile}} for a list of different options}
}
\details{details?}

\value{
percentiles for TD rank curves
}

\references{
References?
}

\seealso{ \code{\link{tdrank}},\code{\link{tdrankperc}} }

\examples{
#tdrankperc()
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}