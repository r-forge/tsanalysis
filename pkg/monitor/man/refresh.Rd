\name{refresh}
\alias{refresh}
\title{Retrieve new data from a database}
\description{
Extact source information and retreive an updated version of 
the data from a database.
}
\usage{
    refresh(data, serIDs = TSseriesIDs(data), con=options()$TSconnection)
}
\arguments{
    \item{data}{An object which has previously been obtained with 
       \code{TSget}, so it contains source information which can 
       be extracted and TSget re-applied.}
    \item{serIDs}{Source series identifiers to retrieve the data.} 
    \item{con}{A database connection.} 
}
\value{A time series object with class depending on the source information.}
\examples{
   con <- TSfinddb(c("padi","MySQL"), dbname="ets")
   if(!is.null(con)) {
     options(TSconnection=con)
     d <- seriesIDs("V1")
     d <- TSget(d)
     d <- refresh(d) }
}
%\keyword{tframe}
\keyword{ts}

