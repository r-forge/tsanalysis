\name{tframe.its}
\alias{tfstart.its}
\alias{tfstart.itstframe}
\alias{tfend.its}
\alias{tfend.itstframe}
\alias{periods.itstframe}
\alias{time.its}

\alias{tframe.its}
\alias{tfSet.itstframe}
\alias{tfwindow.its}

\title{its Methods for tframe}
\description{Methods to extend tframe of its series}

\usage{
    \method{tfstart}{its}(x)
    \method{tfstart}{itstframe}(x)
    \method{tfend}{its}(x)
    \method{tfend}{itstframe}(x)
    \method{time}{its}(x, ...)
    \method{tframe}{its}(x)
    \method{tfSet}{itstframe}(value, x) 
    \method{tfwindow}{its}(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
}

\arguments{
    \item{x}{a tframe or a tframed object.}
    \item{value}{a tframe.}
    \item{start}{A start date of a format compatible with the time series}
    \item{end}{An end date of a format compatible with the time series}
    \item{tf}{A tframe or tframed object}
    \item{warn}{A logical indicating if warning should be produced}
    \item{...}{arguments to be passed to other methods.}
}
\value{Depends}
\details{
    These methods extend tframe generics.
}

\seealso{
    \code{\link[tframe]{tframe}}, 
    \code{\link[tframe]{tfwindow}},
    \code{\link[tframe]{tfstart}}, 
    \code{\link[tframe]{tfend}},
    \code{\link[tframe]{tftime}},
    \code{\link[stats]{time}},
    \code{\link[tframe]{time.tframed}}
    }
\keyword{internal}
