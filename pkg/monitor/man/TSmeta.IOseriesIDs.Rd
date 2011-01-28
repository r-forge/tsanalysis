\name{TSmeta.IOseriesIDs}
\alias{TSmeta,IOseriesIDs,missing-method}
\alias{TSdates,IOseriesIDs,ANY-method}

\title{IOseriesIDs Specific Methods}
\description{See the generic function description.}
\usage{
    \S4method{TSmeta}{IOseriesIDs,missing}(x, con, ...)
    \S4method{TSdates}{IOseriesIDs,ANY}(serIDs, con=getOption("TSconnection"),  
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"), ...)
 }
\arguments{
    \item{x}{a IOseriesIDs object.}
    \item{serIDs}{a IOseriesIDs object.}
    \item{con}{a database connection.}
    \item{vintage}{character string indicating vintage of the series on the 
        database (if supported by the database).}
    \item{panel}{character string indicating panel of the series on the 
        database (if supported by the database).}
    \item{...}{(further arguments, currently disregarded).}

}
\seealso{
    \code{\link[TSdbi]{TSmeta}} 
    \code{\link[TSdbi]{TSget}} 
}
\keyword{ts}


