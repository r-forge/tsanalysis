\name{seriesIDs}
\alias{seriesIDs}
\alias{is.seriesIDs}
\alias{[.seriesIDs}
\alias{print.seriesIDs}
\alias{tframe.seriesIDs}
\alias{tfperiods.seriesIDs}
\alias{tfstart.seriesIDs}
\alias{tfend.seriesIDs}
\alias{tffrequency.seriesIDs}
\alias{seriesNames.seriesIDs}

\title{seriesIDs Objects}
\description{Make a seriesIDs object from a vector of identifiers}
\usage{
    seriesIDs(serIDs,  transforms= "",  
           start=NA, end=NA, frequency=NA, names=NULL, 
           pad=FALSE, pad.start=pad, pad.end=pad)
    is.seriesIDs(obj)
    \method{print}{seriesIDs}(x, ...)
    \method{tframe}{seriesIDs}(x)
    \method{tfperiods}{seriesIDs}(x)
    \method{tfstart}{seriesIDs}(x, ...)
    \method{tfend}{seriesIDs}(x, ...)
    \method{tffrequency}{seriesIDs}(x, ...)
    \method{seriesNames}{seriesIDs}(x)
}
\arguments{
    \item{serIDs}{vectors of strings indicating series identifiers.}
    \item{transforms}{see details.}
    \item{start}{a two element vector of starting year and period which 
       is used when requesting data.}
    \item{end}{a two element vector of end year and end period which 
       is used when requesting data.}
    \item{frequency}{a scalar indicating frequency. This is not 
       used in requesting data, but if supplied it is used as a check of 
       returned data. A warning is issued if returned data does not have this 
       frequency.}
    \item{names}{names to replace series identifiers.}
    \item{pad}{TRUE or FALSE indicating if data should be padded with NAs on both ends
       to the length of the longest available series. If padding is not done 
       then series are truncated to the intersection of available data so there 
       are no NAs in the result. pad.start and pad.end are the arguments which are 
       actually used. pad is just a convenient way to specify both.}
    \item{pad.start}{logical indicating if NAs should be padded to beginning of data.}
    \item{pad.end}{logical indicating if NAs should be padded to end of data.}
    \item{obj}{Any object.}
    \item{x}{a seriesIDs object.}
    \item{...}{arguments to be passed to other methods.}
}

\value{depends}
\details{
The constructor returns an object of class "seriesIDs" which can be used to 
retrieve a matrix time series object. This is the way to produce an object which can be used for database access
and will also apply some treatment to the data when it is retrieved. 
The transforms a vector of strings, one for each series, giving any 
transformations which should be applied when the data is retrieved 
(with eval(call()), e.g. "log",  or "" if no transform is to be applied).

Other functions provide methods for this class of object.
}
\seealso{
    \code{\link[TSdbi]{TSget}},
    \code{\link[base]{print}}
    \code{\link[tframe]{tframe}}, 
    \code{\link[tframe]{tfperiods}}, 
    \code{\link[tframe]{tfstart}},
    \code{\link[tframe]{tfend}},
    \code{\link[tframe]{tffrequency}},
    \code{\link[tframe]{seriesNames}}
}
\examples{
##if(require("TSpadi")) {
##    z <- seriesIDs( c("seriesA", "seriesB"), server="myserver")
##    is.seriesIDs(z)
##    if(checkPADIserver("ets")) 
##       print(seriesIDs(c("series1", "series2"), server="ets"))
##    }
}
\keyword{ts}



