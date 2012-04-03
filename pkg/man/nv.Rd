\name{ nv }
\alias{ nv }
\docType{data}
\title{current version of normative values}
\description{
variable containing the current version of normative values to be used with visualFields. CARE!!! NEEDS TO BE UPDATED EVERY TIME THE DEFAULT \code{sapsunyiunv.rda} IS UPDATED
}
\usage{data( nv )}
\format{It has two elements with the text for variables to use as normative values and location map.
  \describe{
    \item{\code{nv$nvname}}{char. Name of the variable with the normative values to use. Default is "sapsunyiunv"}
    \item{\code{...}}{{same variables as in the corresponding normative values version used. See default value of \code{nv$nvname}}}
  }
}

\keyword{datasets}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}