\name{bebie}
\alias{bebie}
\title{Bebie curve}
\description{Plots the Bebie curve}
\usage{bebie( td, diff = TRUE, percentiles = TRUE, correction = TRUE, txtfont = "mono", pointsize = 7, cex = 1 )}
\arguments{
  \item{td}{total-deviation values}
  \item{diff}{add absolute td values or differences from mean normal. Default is \code{TRUE}}
  \item{percentiles}{add percentile lines. Default is \code{TRUE}}
  \item{correction}{add ranked TD line after correcting for general height difference. Default is \code{TRUE}}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{mono}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{7}}
  \item{cex}{a numerical value giving the amount by which plotting text and symbols should be magnified relative to the default.. Default is \code{1}}
}
\details{Plots Bebie curve}

\value{
something
}

\references{
something
}

\examples{
#bebie( map )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}