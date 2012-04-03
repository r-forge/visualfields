\name{vfindex}
\alias{vfindex}
\title{visual field index}
\description{calculates the visual field index. It can be parametrized by re-defining which locations go to which weighting regions and the actual weights per region.}
\usage{ vfindex( vf, td2pdcutoff = -20, perc = 5, vfiset = vfidefault ) }
\arguments{
  \item{vf}{stimulus sensitivities in dBs}
  \item{td2pdcutoff}{cutof value for mean deviation. See \code{perc} below for an explanation of how both criteria work in conjunction. Default is -20}
  \item{perc}{percentile at which to check whether the 85th TD percentile is within normal limits or not. If it is not and mean deviation is lower than td2pdcutoff, then TD probability maps are to be used instead of PD probability maps. Default is 5th percentile}
  \item{vfiset}{settings to be applied for the calculation of VFI}
}
\details{
calculates the visual field index. It can be parametrized by re-defining which locations go to which weighting regions and the actual weights per region.
}

\value{
returns the subjects's total deviations from the age-matched normative reference
}

\references{
References?
}

\seealso{ \code{\link{vfstats}} }

\examples{
#vfindex(  )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}