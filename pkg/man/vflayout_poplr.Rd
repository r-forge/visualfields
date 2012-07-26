\name{ vflayout_poplr }
\alias{ vflayout_poplr }
\title{layout displaying the PoPLR analysis}
\description{the function performs the PoPLR analysis (see \code{\link{poplr}}) with default values and presents the results in form of a A4 printout}
\usage{
vflayout_poplr( vf, grp = 3, nperm = 5000, pwidth = 8.27, pheight = 11.69, margin = 0.25, filename = NULL )
}
\arguments{
  \item{vf}{\code{\link{vf}} object with threshold sensitivities, td, or pd values}
  \item{grp}{how many visual fields to group. Default is \code{3}}
  \item{nperm}{number of permutations. Default is \code{5000}}
  \item{pwidth}{width of the page (in inches). Default is \code{8.27}}
  \item{pheight}{height of the page (in inches). Default is \code{11.69}}
  \item{margin}{margins of the page (in inches). Default is \code{0.25}}
  \item{filename}{file name to save the printout as pdf. If it is saved to pdf, it won't be displayed in the screen. Default is \code{NULL}.}
}

\seealso{\code{\link{vflayout}}, \code{\link{vflayout_legoplot}}, \code{\link{poplr}}, \code{\link{hist_poplr}}, \code{\link{poplr_cstat}}, \code{\link{poplr_pstat}}, \code{\link{vfplot_poplr}}}

\references{ADD REFERENCES HERE}

\examples{
vflayout_poplr( vf91016right )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}