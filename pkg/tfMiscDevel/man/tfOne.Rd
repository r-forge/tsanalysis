\name{tfOne}
\alias{tfOne}

\title{Integrate Multivariate Time Series}
\description{Integrate multivariate time series using cumsum}

\usage{
    tfOne(x, tf=tfspan(x), start=tfstart(tf), end=tfend(tf),
        Title=NULL, lty=1:5, lwd=1,
        pch=NULL, col=1:6, cex=NULL, xlab=NULL, ylab=NULL,
        xlim=NULL, ylim=NULL, par=NULL, lastObs=TRUE, footnote=NULL, ...)
}

\arguments{
    \item{x}{a multivariate time series object.}
    \item{tf}{a time frame.}
    \item{t0}{a vector of length equal the number of series in \code{x} 
       indicating initial (time zero) valuess, defaults to zero.}
}
\value{a plot.}
\details{
  May replace tfOnePlot. Adds last obs, foot note, legend?
}

\seealso{
    \code{\link[tframe]{tfOnePlot}}
}
\keyword{internal}
