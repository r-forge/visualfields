\name{poplr_pstat}
\alias{poplr_pstat}
\title{Permutation of Pointwise Linear Regression (PoPLR)}
\description{Performs PoPLR analysis}
\usage{poplr_pstat( vf, porder, type = "slr", sl_test = NULL )}
\arguments{
  \item{vf}{visual-field data. It can be td or pd as well}
  \item{porder}{order of permunations of visual-fields sensitivities. Each row contains a permutation of tests from vf}
  \item{type}{Type of regression statistic: \code{slr} for simple linear regression and \code{rank} for Spearman correlation coefficient. Default is \code{slr}}
  \item{sl_test}{values for the 1-tailed hypothesis test for each location. The reference values are not restricted, but they should be either zero (was there any progression?) or negative (was the progression greater than test value?). Default is \code{NULL}}
}
\details{Performs PoPLR analysis}

\value{
the function retunts two different structures depending on whehter the analysis is linear regression (\code{type = "slr"} or Spearman rank correlation \code{type = "rank"}). For \code{slr} analysis, the sturcture consists of four matrices with data: \code{pval} (p-value at each permutation and location of the significance of the 1-tailed hypothesis test specified by \code{sl_test} for each location), \code{se} (standard error), \code{sl} (slope), and \code{int} (intercept). For \code{rank} analysis, the sturcture consists of two matrices: \code{pval} (p-value at each permutation and location of the significance of the 1-tailed hypothesis test specified by \code{sl_test} for each location) and \code{rho} (the Spearman rank correlation)}

\seealso{ \code{\link{poplr}}, \code{\link{poplr_cstat}} }

\references{
something
}

\examples{
#poplr_pstat( vf )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>, Neil O'Leary <oleary.neil@gmail.com>}