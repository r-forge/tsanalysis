\name{expandWeekly}
\alias{tfOne}

\title{Generate Weekly Time Series}
\description{Generate weekly time series by expanding or extracting from a
series of a different frequency.}

\usage{
expandMtoW(x, fromStart=start(x), notreleased=NA, na=NA)
expandQtoW(x, fromStart, notreleased=NA, na=NA)
extractWeekly.daily(x, fromStart, day=5, notreleased=NA, na=NA)
}

\arguments{
    \item{x}{a time series object.}
    \item{fromStart}{start date for the new series.}
    \item{notreleased}{value to be used for not yet released data.}
    \item{na}{value to be used for data points that will never be available.}
    \item{day}{extraction day to use converting daily to weekly data.}
}
\value{a weekly time series object.}
\details{
\code{expandMtoW} assigns monthly data into the last Friday of a month to 
 give a weekly Friday series with \code{na} inserted where there is no data.

\code{expandQtoW} assigns quarterly data into the last Friday of a quarter to 
 give a weekly Friday series with \code{na} inserted where there is no data.

\code{extractWeekly.daily} extracts data from day of the week, 1=Monday, 
 default 5=Friday to 
 give a weekly series with \code{na} inserted where there is no data.

\code{notreleased} is for values which will eventually be available.
Values of \code{notreleased = -7000}, \code{na = -99999} are used in some programs
}

\seealso{
    \code{\link[tframePlus]{as.quarterly}}
}
\keyword{ts}
