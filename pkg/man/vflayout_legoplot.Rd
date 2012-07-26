\name{vflayout_legoplot}
\alias{vflayout_legoplot}
\title{legoplot visualization for change in sensitivities over time of visual fields}
\description{
displays a legoplot visualization for change in sensitivities over time of visual fields
}
\usage{
vflayout_legoplot( vf, grp = 3, pwidth = 8.27, pheight = 11.69, margin = 0.25, filename = NULL )
}
\arguments{
  \item{vf}{\code{\link{vf}}-object with sensitivity thresholds}
  \item{grp}{how many visual fields to group. Default is \code{3}}
  \item{pwidth}{width of the page (in inches). Default is \code{8.27}}
  \item{pheight}{height of the page (in inches). Default is \code{11.69}}
  \item{margin}{margins of the page (in inches). Default is \code{0.25}}
  \item{filename}{file name to save the printout as pdf. If it is saved to pdf, it won't be displayed in the screen. Default is \code{NULL}.}
}

\seealso{\code{\link{vflayout}}, \code{\link{vflayout_poplr}}}

\examples{
vflayout_legoplot( vf91016right )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>, }