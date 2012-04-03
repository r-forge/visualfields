\name{vfsettings}
\alias{vfsettings}
\docType{data}
\title{Settings of visual-field object }
\description{Specifies the column where the visual-field sensitivities at each location are starting to be listed, the number of locations, and the locations that correspond to the anatomical region of the eye's blind spot (those BS locations are remove from the statistical analyses).}
\usage{ data( vfsettings ) }
\format{
vfsettings contains structures for different patterns of location testing. For instance, p24d2 or p30d2. Each structure contains 2 items:
\describe{
\item{\code{locnum}}{total number of locations of the testing pattern in which stimuli are presented. For 24/2, that is 54}
\item{\code{bs}}{locations that correspond to the ananomically region where the blind spot is. For 24/2, those are locations 26 and 35}
\item{\code{locrPD}}{specifies the rank TD value to be used for the derivation of TD. For 24/2, that location would be 7, corresponding approximately (but not very) to the 85th percentile}
  }
  In addition to data for each pattern of locations, it has a variable \code{locini} specifying the starting column in the VF-object in which locations sensitivities are listed
}

\keyword{datasets}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}