\name{vfcolormap}
\alias{vfcolormap}
\title{Color code for TD or PD probability maps}
\description{returns the RGB values specifying the color code for the TD or PD probability maps}
\usage{vfcolormap( map, mapval = vfenv$nv$pmapsettings )}
\arguments{
  \item{map}{array with the percentile for TD or PD probability map}
  \item{mapval}{mapping values to use. Default is \code{nv$pmapsettings}}
}

\seealso{\code{\link{vfplot}}, \code{\link{vfgrayscale}}}

\examples{
# DO NOT EXECUTE
#vfcolormap( map )
}

\author{Chaitanya Khadilkar <chaikhad@indiana.edu>, Ivan Marin-Franch <imarinfr@indiana.edu>}