\name{combineAndForecast}
\alias{combineAndForecast}
\alias{reconstruct.combinedForecast}
\alias{construct.data.to.override.horizon}
\alias{get.overriding.data}
\alias{restrict.overriding.data}
 
\title{Simple Monitoring Utility Functions}
\description{Utility functions for simple monitoring.}
\usage{
combineAndForecast(model, new.data,  
      overlapping.period.forecast.tag="g", forecast.tag="f") 
reconstruct.combinedForecast(combinedForecast) 
construct.data.to.override.horizon(new.data, model, plot=TRUE, forecast.tag="f")
get.overriding.data(file="overriding.data", 
    first.input="",first.output="", second.output="", m=1, p=10)
restrict.overriding.data(data, overriding.horizon=0)  
}
\value{depends}
\details{Internal functions, not to be called by user.}
\seealso{
    \code{\link{combinationMonitoring}}
}

%\keyword{DSE}
\keyword{internal}

