\name{quad2Dfit}
\alias{quad2Dfit}
\title{Two-dimensional quadratic fitting for visual-field results}
\description{fits a 2D quadratic function using values in val as "observations" for the x and y coordinates in patternMap}

\usage{quad2Dfit( val, patternMap = saplocmap$p24d2, bspos = vfsettings$p24d2$bs )}
\arguments{
  \item{val}{values at each location}
  \item{patternMap}{pattern of stimulus locations. It uses the x and y coordinates in conjunction with val to get the fitted values with the 2D model}
  \item{bspos}{position of the blind spot}
}
\details{
Ahhhhhh!!!!!!!!
}

\value{
fitted values by a 2D quadratic function
}

\references{
References?
}

\examples{
#quad2Dfit()
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}