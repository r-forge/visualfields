\name{ vflayout_poplr }
\alias{ vflayout_poplr }
\title{printout with results for the visual field}
\description{
creates a printout with results for the visual field
}
\usage{
vflayout_poplr( vf, grp = 3, nperm = 5000, pwidth = 8.27, pheight = 11.69, margin = 0.25, filename = NULL )
}
\arguments{
  \item{vf}{visual-field data. It can be td or pd as well}
  \item{grp}{how many visual fields to group. Default is \code{3}}
  \item{nperm}{number of permutations. Default is \code{5000}}
  \item{pwidth}{width of the page (in inches). Default is \code{8.27}}
  \item{pheight}{height of the page (in inches). Default is \code{11.69}}
  \item{margin}{margins of the page (in inches). Default is \code{0.25}}
  \item{filename}{file name to save the printout as pdf. If it is saved to pdf, it won't be displayed in the screen. Default is \code{NULL}.}
}
\details{
creates a printout with results for the visual field
}

\value{
plots the printout or saves it as pdf
}

\seealso{ \code{\link{createviewport}} }

\examples{
#vflayout_poplr()
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}