\name{IOseriesIDs2}
\alias{IOseriesIDs2}
\title{Constructor for IOseriesIDs object}
\description{
Make a IOseriesIDs object from identifier information}
\usage{
    IOseriesIDs2(input=NULL, output=NULL,
      start = NA, end = NA, frequency = NA,
      pad.start = FALSE, pad.end = FALSE)
}
\arguments{
    \item{input}{Lists of character vectors indicating data to be used as input.
        Each vector has 5 elements indicating the 
        server, the database, the series identifier, any tranformation, and 
	a name.}
    \item{output}{List (as for input) indicating outputs. At 
        least one of input or output should be specified.}
    \item{start, end, frequency, pad.start, pad.end}{
    Information about data retrieval. See TSget for more details.}
}
\value{An object of class "IOseriesIDs".}
\details{
This is an alternate way to produce an object which can be used for database access.
}
\seealso{
\code{\link[TSdbi]{TSget}}
\code{\link{seriesIDs}}
\code{\link{IOseriesIDs}}
}
\examples{
inflation.sa.names <- IOseriesIDs2(
    output = list(
         c("ippia401",  "ytoypc", "industrial product price index"),
         c("B820600",  "ytoypc", "cpi"),      
         c("B820655",  "ytoypc", "cpixfe"),
         c("B820672",  "ytoypc", "price goods excl food & energy"),
         c("B820678",  "ytoypc", "price services"),
         c("B800938",  "ytoypc", "total unit labour costs")
           ))

}
%\keyword{DSE}
\keyword{ts}

