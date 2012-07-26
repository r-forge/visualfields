\name{bebie}
\alias{bebie}
\title{Bebie curve}
\description{Plots Bebie rank TD curve}
\usage{bebie( td, diff = TRUE, percentiles = TRUE, correction = TRUE, txtfont = "mono", pointsize = 7, cex = 1 )}
\arguments{
  \item{td}{a \code{vf} object with total-deviation values}
  \item{diff}{add absolute td values or differences from mean normal. Default is \code{TRUE}}
  \item{percentiles}{add percentile lines. Default is \code{TRUE}}
  \item{correction}{add ranked TD line after correcting for general height difference. Default is \code{TRUE}}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{mono}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{7}}
  \item{cex}{a numerical value giving the amount by which plotting text and symbols should be magnified relative to the default, which is \code{1}}
}

\value{returns a graph with the Bebie rank TD curve (also known as the cumulative defect curve [1]) if the option \code{diff} is set to \code{FALSE}. If the option \code{diff} is set to true, then it returns the difference between the subject's rank curve and the mean normal curve. To use this option (and the options with \code{percentile = TRUE}), the tables \code{nvtdrank}, \code{perctdrank}, and \code{perctdrankadj} must exist in the \code{\link{nv}}-object, for the perimeter \code{tperimetry}, the pattern of locations \code{talgorithm}, and thepresentation algorithm \code{tpattern}. See \code{\link{sapsunyiunv}} to see the structure the tables must have}

\references{
[1] H. Bebie, J. Flammer, and T. Bebie. \emph{The cumulative defect curve: separation of local and diffuse components of visual field damage}. Graefe's Archive Clinical Experimental Ophthalmology, 227, 1989
}

\examples{
td <- tdval( vf91016right[15,] )
bebie( td )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}