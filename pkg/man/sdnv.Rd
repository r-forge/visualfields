\name{ sdnv }
\alias{ sdnv }
\title{ standard deviations of normative values }
\description{
get the standard deviations at each location for normative values: sensitivities, total-deviation, and pattern-deviation values
}
\usage{
sdnv( vf, smooth = TRUE, smoothFunction = quad2Dfit )
}
\arguments{
  \item{vf}{ stimulus sensitivities in dBs }
  \item{smooth}{whether to use a function to smooth the results or not. Default is TRUE}
  \item{smoothFunction}{If \code{smooth} is true, the function to use for smoothing. Default is quad2Dsmothing, a 2D quadratic fit to resulting data. This function is not really a smoothing procedure, but a parametric fit}
}
\details{
calculates total-deviation and pattern-deviation values and, from them, their standard deviation at each location
}

\value{
returns a 3-column data frame with SDs per location. Each row is a location. Column 1 is for sensitivities, column 2 for total deviation, and column 3 for pattern deiation.
}

\references{
References?
}

\seealso{ \code{\link{ageLinearModel}}, \code{\link{locperc}} }

\examples{
#sdnv(  )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}