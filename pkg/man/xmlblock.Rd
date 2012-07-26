\name{xmlblock}
\alias{xmlblock}
\title{extracts a block from the XML file}
\description{extracts a block from the XML file}
\usage{xmlblock( tag, xmllines )}
\arguments{
  \item{tag}{tag to look at}
  \item{xmllines}{lines from loaded XML files}
}
\value{returns an array of characters with all that is inside a block corresponding to a particular tag}

\seealso{\code{\link{loadvfxml}}, \code{\link{loadvfcsv}}, \code{\link{xmlitem}}}

\examples{
# DO NOT RUN
#xmlblock( tag, xmllines )
}

\author{Ivan Marin-Franch <imarinfr@indiana.edu>}