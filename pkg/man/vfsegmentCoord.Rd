\name{vfsegmentCoord}
\alias{vfsegmentCoord}
\title{Line segments}
\description{This function geneartes coordinates for the line segments to be plotted}
\usage{vfsegmentCoord( lineMap, length = 3.75 )}
\arguments{
  \item{lineMap}{patternMap having x,y and slope values}
  \item{length}{length of the line segment in inches. Default is \code{3.75}}
}
\details{This function geneartes coordinates for the line segments to be plotted. Based on the location (x,y) and slope, this function claculates the coordinate set {(x1,y1),(x2,y2)} which is used to plot the line segments}

\value{
something
}

\references{
something
}

\seealso{\code{\link{vfsegmentCoord}}}

\examples{
#vfsegmentCoord( lineMap,length )
}

\author{Chaitanya Khadilkar <chaikhad@indiana.edu>, Ivan Marin-Franch <imarinfr@indiana.edu>}