\name{ ghpostd }
\alias{ ghpostd }
\title{general height estimated as the pos-th ranked TD location}
\description{
gets the general height estimated as the pos-th ranked TD location
}
\usage{
ghpostd( td, rankRef = c( "default" ) )
}
\arguments{
  \item{td}{total-deviation values}
  \item{rankRef}{numeric or char. The rank position to take as reference for general height. Default is "default". For "default" it looks at the type of visual field, "p24d2" or "p30d2", "fullt" or "sitas", to get which is the position that corresponds, roughly with the 85th percentile}
}
\details{
calculates the general height estimated as the pos-th ranked TD location
}

\value{
returns the subject's estimated general height
}

\references{
References?
}

\seealso{\code{\link{pdval}}}

\examples{
# ghpostd(  )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}