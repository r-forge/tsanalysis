\name{TSgetMethod}
\alias{TSget,IOseriesIDs,ANY-method}
\alias{TSget,IOseriesIDs,missing-method}
\alias{TSget,seriesIDs,missing-method}

\title{Get fixed data snapshot using TSPADI}
\description{
Get a TSdata structure from TSPADI Database Interface}
\usage{
    \S4method{TSget}{IOseriesIDs,ANY}(serIDs, con=options()$TSconnection,  ...)
    \S4method{TSget}{IOseriesIDs,missing}(serIDs, con=options()$TSconnection,  ...)
    \S4method{TSget}{seriesIDs,missing}(serIDs, con=options()$TSconnection,  ...)
}
\arguments{
    \item{serIDs}{An object of class IOseriesIDs identifying series.}
    \item{con}{A database connection.}
    \item{...}{Arguments passed to other methods.}
}
\value{A TSdata object taken from the database.
}
\details{
This function extracts data using the TSdbi interface
to a database. See IOseriesIDs, seriesIDs, and TSget for more details.

New \code{names} can be passed in \dots . 
}
\seealso{
  
\code{\link[TSdbi]{TSget}}
\code{\link[dse]{TSdata}}
\code{\link{IOseriesIDs}}
}
\examples{
   buscrd.names <- IOseriesIDs2(
    output = list(
         c("V36419",  "ytoypc", "long term business credit"),
         c("V122646",  "ytoypc", "short term business credit")
           ))
   con <- TSfinddb(c("padi","MySQL"), dbname="ets")
   if(!is.null(con)){
     options(TSconnection=con)
     z <- TSget(buscrd.names)
   }
}
%\keyword{DSE}
\keyword{ts}

