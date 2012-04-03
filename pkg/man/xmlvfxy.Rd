\name{xmlvfxy}
\alias{xmlvfxy}
\title{Extraction of (x,y)-coordinates of tested locations}
\description{extracts the (x,y)-coordinates of tested locations}
\usage{xmlvfxy( xmllines ) }
\arguments{
  \item{xmllines}{lines from loaded XML files}
}
\details{extracts the (x,y)-coordinates of tested locations. To be used only for custom locations, not when we know that the testing pattern is 24-2, or 30-2, etc}

\value{(x,y-coordinates of tested locations)}

\references{
References?
}

\seealso{ \code{\link{loadvfxml}}, \code{\link{xmlvfval}} }

\examples{
#xmldevval(  )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}