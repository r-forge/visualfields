\name{progols}
\alias{progols}
\title{progression by linear ordinary least squares}
\description{makes a linear regression with projection in the future by projyears years}
\usage{progols( age, index, projyears = 5, xlab = "age", ylab = "md", txtfont = "mono", pointsize = 12, cex = 1 )}
\arguments{
  \item{age}{age}
  \item{index}{index measured at the corresponding age}
  \item{xlab}{label for x-axis. Default is \code{age}}
  \item{ylab}{label for y-axis. Default is \code{md}}
  \item{projyears}{Years for projection. Default is \code{5}}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{mono}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{12}}
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