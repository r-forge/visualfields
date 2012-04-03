\name{vfstats}
\alias{vfstats}
\title{visual field stats}
\description{calculates the visual field stats}
\usage{ vfstats( vf ) }
\arguments{
  \item{vf}{ stimulus sensitivities in dBs }
}
\details{
calculates the visual field stats: mean sensitivity (msens), std of sensitivities (ssens), mean total deviation (mtdev), std of total deviation (stdev), mean pattern deviation (mpdev), std of pattern deviation (spdev). All are weighted means and stds. IMPORTANT FOR ADDING MORE GLOBAL INDICES. In \code{\link{vfstats}} all global indices refering to mean must start with \code{m} and all refereing to standard deviation must start with \code{s}, otherwise \code{\link{gloperc}} won't calculate the cutoff values correctly
}

\value{
returns the subjects's total deviations from the age-matched normative reference
}

\references{
References?
}

\seealso{ \code{\link{vfstats}} }

\examples{
#vfi(  )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}