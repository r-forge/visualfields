\name{locperc}
\alias{locperc}
\title{percentiles for each location}
\description{gets the percentiles for each location}
\usage{locperc( vals, stds, percentiles = c( 0.5, 1, 2, 5, 95 ), type = c( "quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n" ), poolLocations = FALSE )}
\arguments{
  \item{vals}{table with sensitivity values, total-deviation values, or pattern-deviation values}
  \item{stds}{standard deviations per location for sensitivity values, total-deviation values, or pattern-deviation values}
  \item{percentiles}{percentiles at which to calculate cutoff values}
  \item{type}{see \code{\link{wtd.quantile}} for a list of different options}
  \item{poolLocations}{assume that the shape of the empirical distributions at each location is the same and pool sensitivity, TD, or PD values per location. This is useful when the number of controls available is small. Sample size is increased at the expense of posible bias due to the fact that empirical distributions are not necessarily the same in each location}
}
\details{details?}

\value{
percentiles for each location
}

\references{
References?
}

\seealso{ \code{\link{gloperc}} }

\examples{
#locper()
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}