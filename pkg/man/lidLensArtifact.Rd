\name{lidLensArtifact}
\alias{lidLensArtifact}
\title{identify in a very coarse way visual fields that may have lid or lens artifacts. TO BE USED ONLY WITH CONTROL DATA}
\description{identify in a very coarse way visual fields that may have lid or lens artifacts. TO BE USED ONLY WITH CONTROL DATA}
\usage{lidLensArtifact( vf, min_dB = c( 12 ) )}
\arguments{
  \item{vf}{visual-field object}
  \item{min_dB}{lowest dB value considered normal}
}
\details{
I'll tell you when I know
}

\value{
ideally, it would return a list of visual fields that are "suspect" of been affected by lid or lens artifact
}

\references{
References?
}

\seealso{ \code{\link{filterReliability}} }

\examples{
#lidLensArtifact()
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}