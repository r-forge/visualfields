\name{hist_poplr}
\alias{hist_poplr}
\title{histogram fro PoPLR analysis}
\description{Plots an histogram of Fisher S statistic combining all location p-values}
\usage{hist_poplr( scomb_obs, pcomb_obs, scomb, txtfont = "mono", pointsize = 7 )}
\arguments{
  \item{scomb_obs}{observed S-statistic for the observed visual fields}
  \item{pcomb_obs}{p-value associated with the observed S-statistic for the observed visual fields}
  \item{scomb}{S-statistics calculated for the permutation of visual fields}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{mono}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{7}}
}
\details{something, something dark side}

\value{
something
}

\references{
something
}

\seealso{\code{\link{vfColorMap}}}

\examples{
#vfplot()
}

\author{Chaitanya Khadilkar <chaikhad@indiana.edu>, Ivan Marin-Franch <imarinfr@indiana.edu>}