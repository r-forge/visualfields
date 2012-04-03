\name{colorMapGraph}
\alias{colorMapGraph}
\title{color legend for p-values in td and pd probability plots}
\description{It geneartes color legend for p-values in td and pd probability plots}
\usage{colorMapGraph( ncol = 3, notSeenAsBlack = TRUE, txtfont = "mono", pointsize = 7, outerSymbol = "circles", innerSymbol = "circles", outerSize = 1, innerSize = 1, outerInch = 0.2, innerInch = 0.1 )}
\arguments{
  \item{ncol}{number of columns in where to show the color symbols. Default is \code{3}}
  \item{notSeenAsBlack}{Add a black color-code representing non-seen to the color scheme of the probability maps for td and pd}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{mono}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{7}}
  \item{outerSymbol}{The  outer symbol at all locations. Can be any of circles, squares, rectangles, stars. Default is \code{circle}}
  \item{innerSymbol}{The  inner symbol at all locations. Can be any of circles, squares, rectangles, stars. Default is \code{circle}}
  \item{outerSize}{Size of the outer symbol. Default is \code{1}}
  \item{innerSize}{Size of the inner symbol. Default is \code{1}}
  \item{outerInch}{Maximum size of the outer symbol in inches. Default is \code{0.2}}
  \item{innerInch}{Maximum size of the inner symbol in inches. Default is \code{0.1}}
}
\details{It geneartes color legend for p-values in td and pd probability plots}

\value{
something
}

\references{
something
}

\seealso{\code{\link{vfColorMap}}}

\examples{
#colorMapGraph( nrow, ncol, txtfont, pointsize )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}