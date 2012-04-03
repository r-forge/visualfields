\name{poplr}
\alias{poplr}
\title{Permutation of Pointwise Linear Regression (PoPLR)}
\description{Performs PoPLR analysis}
\usage{poplr( vf, nperm = 5000, type = "slr", sl_test = NULL, typecomb = "fisher", details = FALSE )}
\arguments{
  \item{vf}{visual-field data. It can be td or pd as well}
  \item{nperm}{number of permutations. Default is \code{5000}}
  \item{type}{Type of regression statistic: \code{slr} for simple linear regression and \code{rank} for Spearman correlation coefficient. Default is \code{slr}}
  \item{sl_test}{values for the 1-tailed hypothesis test for all locations. The reference value is not restricted, but it should be either zero (was there any progression?) or negative (was the progression greater than test value?). Default is \code{0}}
  \item{typecomb}{Combination type for p-values. Default is \code{fisher}}
  \item{details}{Whether to return all details of the permuation analysis or just final results. Default is \code{FALSE}}
}
\details{Performs PoPLR analysis}

\value{
something
}

\references{
something
}

\seealso{ \code{\link{poplr_pstat}}, \code{\link{poplr_cstat}} }

\examples{
#poplr( vf )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>, Neil O'Leary <oleary.neil@gmail.com>}