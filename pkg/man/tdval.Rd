\name{ tdval }
\alias{ tdval }
\title{ total deviation }
\description{
get total-deviation values
}
\usage{
tdval( vf )
}
\arguments{
  \item{vf}{ stimulus sensitivities in dBs }
}
\details{
calculates the normative reference sensitivities for healthy subjects of the same age as the subject and obtains differences between the visual field sensitivities and mean normal sensitivities. All in dBs.
}

\value{
returns the subjects's total deviations from the age-matched normative reference
}

\references{
References?
}

\seealso{ \code{\link{tdpmap}}, \code{\link{pdval}}, \code{\link{pdpmap}} }

\examples{
#tdval(  )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}