\name{saplocmap}
\alias{saplocmap}
\docType{data}
\title{xy-position mapping between SAP device convention and visualFields convention}
\description{
A table with x and y positions of stimulus presentation, the corresponding location for SAP device. The convention for visualFields is to use always a right-eye format. That is, a left eye would be "flipped" left-right and location number are counted row-wise from top-left to bottom-right. Information about the size of the stimulus and the corresponding angle of incidence and slope with Jansonious map (ADD REFERENCE) are added.
}
\usage{ data( saplocmap ) }
\format{
Table data frame SAPlocmap has 6 columns
\describe{
\item{\code{xod}}{stimulus x position}
\item{\code{yod}}{stimulus y position}
\item{\code{loc}}{sequential location number in the original device}
\item{\code{size}}{size of the stimulus presentation}
\item{\code{jmangle}}{Angle of incidence in blind spot from Jansonious map}
\item{\code{jmslope}}{Orientation of an average bundle at that position of the visual field as calculated from the Jansonious map}
\item{\code{region}}{Region of the visual fields in comparison with ONH sector. Garway-Heath map}
  }
}

\references{
JANSONIOUS
}

\keyword{datasets}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}