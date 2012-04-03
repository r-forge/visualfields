\name{ageLinearModel}
\alias{ageLinearModel}
\title{linear model for age effect on visual sensitivity}
\description{creates the linear model to account for mean age effect on visual sensitivity at each location}

\usage{ageLinearModel( vf, smooth = TRUE, smoothFunction = quad2Dfit )}
\arguments{
  \item{vf}{data frame with visual fields from which to extract the linear relationship between visual sensitiviy and age}
  \item{smooth}{whether to use a function to smooth the results or not. Default is TRUE}
  \item{smoothFunction}{If \code{smooth} is \code{TRUE}, the function to use for smoothing. Default is \code{\link{quad2Dfit}}, a 2D quadratic fit to resulting data. This function is not really a smoothing procedure, but a parametric fit}
}
\details{
Ahhhhhh!!!!!!!!
}

\value{
returns an object with intercepts and slopes modelling age-related mean sensitivity change.
}

\references{
References?
}

\seealso{ \code{\link{sdnv}}, \code{\link{locperc}} }

\examples{
#ageLinearModel()
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}