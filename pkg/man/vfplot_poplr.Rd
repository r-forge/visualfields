\name{vfplot_poplr}
\alias{vfplot_poplr}
\title{Visual Fileds Plot}
\description{This package provides a function: vfplot which can be used to plot visual fields data.}
\usage{vfplot_poplr( sl, pval, vfinfo, newWindow = FALSE, txtfont = "mono", pointsize = 7, width = 6, xminmax = 29, yminmax = 29, outerSymbol = "circles", innerSymbol = "circles", outerSize = 1, innerSize = 1, outerInch = 0.24, innerInch = 0.12, lengthLines = 3.0, thicknessLines = 2 )}
\arguments{
  \item{sl}{slopes}
  \item{pval}{pvalues calculated for the slope}
  \item{vfinfo}{information about the visualfield}
  \item{newWindow}{boolean value- TRUE will generate a new window for the plot. Default value is TRUE}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{mono}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{7}}
  \item{width}{width of the window. height is calucated using the width, xminmax and yminmax}
  \item{xminmax}{minimum and maximum limits on the x axis}
  \item{yminmax}{minimum and maximum limits on the y axis}
  \item{outerSymbol}{The outer symbol at all locations. Can be any of circles, squares, rectangles, stars. Default is \code{circle}}
  \item{innerSymbol}{The inner symbol at all locations. Can be any of circles, squares, rectangles, stars. Default is \code{circle}}
  \item{outerSize}{Relative size of the outer symbol. Default is \code{1}}
  \item{innerSize}{Relative size of the inner symbol. Default is \code{1}}
  \item{outerInch}{Maximum size of the outer symbol in inches. Default is \code{0.2}}
  \item{innerInch}{Maximum size of the inner symbol in inches. Default is \code{0.1}}
  \item{lengthLines}{length of the lines denoting the orientation of the RNF bundle}
  \item{thicknessLines}{thickness of the lines denoting the orientation of the RNF bundle}
}
\details{The vfplot function operates on a single row of visual fileds (vf). Depending on the plot type it will generate a plot representing the sensitivity at each location. The  color scheme elucidates the degree of sensitivity at that location. Locations with zero sensitivity are shown in black. Blind spots have been excluded fronm the plot}

\value{
something
}

\references{
something
}

\seealso{\code{\link{vfColorMap}}}

\examples{
#vfplot_poplr()
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}