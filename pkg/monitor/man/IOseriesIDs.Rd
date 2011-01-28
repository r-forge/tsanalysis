\name{IOseriesIDs}
\alias{IOseriesIDs}
\alias{is.IOseriesIDs}

\title{Constructor for IOseriesIDs object}
\description{Make a IOseriesIDs object from identifier information}
\usage{
    IOseriesIDs(output=NULL,           input=NULL,
                        output.transforms=NULL,  input.transforms=NULL, 
                        output.names=NULL,     input.names=NULL,
                         start=NA, end=NA, frequency=NA, 
                         pad=FALSE, pad.start=pad, pad.end=pad)
  
    is.IOseriesIDs(obj)
}
\arguments{
    \item{output}{see seriesIDs description of \code{series}.}
    \item{output.transforms}{see seriesIDs descriptions.}
    \item{output.names}{see seriesIDs descriptions.}
    \item{input}{see seriesIDs description of \code{series}.}
    \item{input.transforms}{see seriesIDs descriptions.}
    \item{input.names}{see seriesIDs descriptions.}

    \item{start}{see seriesIDs descriptions.}
    \item{end}{see seriesIDs descriptions.}
    \item{frequency}{see seriesIDs descriptions.}
    \item{pad}{see seriesIDs descriptions.}
    \item{pad.start}{see seriesIDs descriptions.}
    \item{pad.end}{see seriesIDs descriptions.}
    \item{obj}{any object.}
}

\value{An object of class "IOseriesIDs" "TSdata".}
\details{
This is the way to produce an object which can be used for database access.
Input and output values are passed to TSget so the object return is 
TSdata with input and output (but either of these can be NULL).
}
\seealso{
\code{\link[TSdbi]{TSget}}
\code{\link{seriesIDs}}
\code{\link{IOseriesIDs2}}
}
\examples{
    z <- IOseriesIDs(output=c("seriesA", "seriesB"))
    is.IOseriesIDs(z)
}
%\keyword{DSE}
\keyword{ts}

