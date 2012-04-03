\name{vfiperc}
\alias{vfiperc}
\title{percentiles for visual field index}
\description{gets the percentiles for visual field index}
\usage{ vfiperc( vals, percentiles = c( 0.5, 1, 2, 5, 95 ), type = c( "quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n" ) )}
\arguments{
  \item{vals}{table with global indices for control subjects}
  \item{percentiles}{percentiles at which to calculate cutoff values}
  \item{type}{see \code{\link{wtd.quantile}} for a list of different options}
}
\details{
gets the percentiles for visual field index
}

\value{
percentiles for visual field index
}

\references{
References?
}

\seealso{ \code{\link{vfindex}}, \code{\link{vfstats}}, \code{\link{gloperc}}, \code{\link{locperc}} }

\examples{
#vfindex(  )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}