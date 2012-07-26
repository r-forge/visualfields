\name{tdrankperc}
\alias{tdrankperc}
\title{percentiles for TD rank curve}
\description{gets percentiles for TD rank curve}
\usage{tdrankperc( td, percentiles = c( 0.5, 1, 2, 5, 95 ), type = c( "quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n" ) )}
\arguments{
  \item{td}{\code{\link{vf}}-object with total-deviation values}
  \item{percentiles}{percentiles at which to calculate cutoff values}
  \item{type}{see \code{\link{wtd.quantile}} for a list of different options}
}

\value{percentiles for TD rank curves}

\seealso{ \code{\link{tdrank}}, \code{\link{tdrankadjperc}} }

\examples{
# DO NOT EXECUTE
#tdrankperc( td )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}