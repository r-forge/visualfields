\name{vfidefault}
\alias{vfidefault}
\docType{data}
\title{Settings of visual field index }
\description{Specifies the weighting to be applied at each location for averaging.}
\usage{ data( vfidefault ) }
\format{
vfisettings contains structures for different patterns of location testing. For instance, p24d2 or p30d2. Each structure contains 2 items:
  \describe{
    \item{\code{locregions}}{mapping between locations in the visual-field testing pattern and regions with the same weight}
    \item{\code{locregions}}{the actual weights to be applied to each region}
  }
  In addition to data for each pattern of locations, it has a variable \code{locini} specifying the starting column in the VF-object in which locations sensitivities are listed
}

\keyword{datasets}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}