\name{poplr}
\alias{poplr}
\title{permutation of Pointwise Linear Regression (PoPLR)}
\description{performs the PoPLR analysis from a series of visual-field threshold sensitivities, or TD or PD values over time}
\usage{poplr( vf, nperm = 5000, type = "slr", truncVal = 1,
       sl_test = NULL, typecomb = "fisher", details = FALSE )}
\arguments{
  \item{vf}{\code{\link{vf}} object with threshold sensitivities, td, or pd values}
  \item{nperm}{number of permutations. Default is \code{5000}}
  \item{type}{Type of regression statistic: \code{slr} for simple linear regression and \code{rank} for Spearman correlation coefficient. Default is \code{slr}}
  \item{truncVal}{p-value cut-off for truncation. Default is \code{1}}
  \item{sl_test}{values for the 1-tailed hypothesis test for all locations. The reference value is not restricted, but it should be either zero (was there any progression?) or negative (was the progression greater than test value?). Default is \code{0}}
  \item{typecomb}{Combination type for p-values. Default is \code{fisher}}
  \item{details}{Whether to return all details of the permuation analysis or just final results. Default is \code{FALSE}}
}

\seealso{\code{\link{hist_poplr}}, \code{\link{poplr_cstat}}, \code{\link{poplr_pstat}}, \code{\link{vflayout_poplr}}, \code{\link{vfplot_poplr}}}

\references{ADD REFERENCES HERE}

\examples{
res <- poplr( vf91016right )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}