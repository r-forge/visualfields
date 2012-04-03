\name{sapsunyiunv}
\alias{sapsunyiunv}
\docType{data}
\title{SUNY-IU normative reference values for Standard Automatic Perimetry}
\description{
Normative reference values for Standard Automatic Perimetry obtained from SUNY (Mitchel W Dul) and IU (William H Swanson) databases
}
\usage{ data( sapsunyiunv ) }
\format{
SAPHalifaxNorms includes several structures. Age linear models for different location patterns and different threshold-estimation algorithms:
\describe{
\item{\code{SAPsunyiuNV$pmapsettings}}{information about which percentiles this set of normative reference values is using and corresponding color coding for display}
\item{\code{SAPsunyiuNV$p24d2_fullt}}{for 24/2 grid with full threshold}
Each object contains a linear age model and normative limits for the percentiles specified in pmapsettings for both TD and PD.
  }
}

\references{
References?
}

\keyword{datasets}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>, William H Swanson, Victor Malinovski, Mitchell Dul}