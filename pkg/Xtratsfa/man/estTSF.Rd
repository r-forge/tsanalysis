\name{estTSF.R2M}
\alias{estTSF.R2M}
\alias{estTSF.MCV}

\title{Estimate Time Series Factor Model}
\usage{
    estTSF.R2M(y, p, diff.=TRUE, 
                rotation=if(p==1) "none" else "quartimin", 
		rotationArgs=NULL, 
		normalize=TRUE, eps=1e-5, maxit=1000, Tmat=diag(p),
		BpermuteTarget=NULL,
                factorNames=paste("factor", seq(p)))
    estTSF.MCV(y, p, diff.=TRUE,
                rotation=if(p==1) "none" else "oblimin", 
		rotationArgs=NULL,  
		normalize=TRUE, eps=1e-5, maxit=1000, Tmat=diag(p),
		BpermuteTarget=NULL,
                factorNames=paste("factor", seq(p)))
}
\arguments{
    \item{y}{a time series matrix.}
    \item{p}{integer indication number of factors to estimate.}
    \item{diff.}{logical indicating if model should be estimated with 
           differenced data.}
    \item{rotation}{character vector indicating the factor 
         rotation method (see GPArotation for options).}
    \item{rotationArgs}{list passed to GPFoblq, and then to the rotation method, 
          specifying arguments for the rotation criteria. See GPFoblq.}
    \item{normalize}{Passed to GPFoblq. TRUE means do Kaiser normalization before
          rotation and then undo it after completing rotatation. FALSE means do 
	  no normalization. See GPFoblq for other possibilities.}
    \item{eps}{passed to GPFoblq} 
    \item{maxit}{passed to GPFoblq} 
    \item{Tmat}{passed to GPFoblq} 
    \item{BpermuteTarget}{matrix of loadings. If supplied, this is used to permute the
                 order of estimated factors and change signs in order to 
		 compare properly.}
    \item{factorNames}{vector of strings indicating names to be given to factor
       series.}
}

\description{
   Estimate a TSFmodel . 
}

\value{A \code{TSFestModel} object which is a list containing \code{TSFmodel},
    the data, and some information about the estimation.}

\details{
  The function \code{estTSF.R2M} estimates parameters using raw second moments. 
  THIS ALL NEEDS TO BE CHECKED.
  The function \code{factanal} with no rotation is used to find the initial
  (orthogonal) solution. Rotation, if specified, is then done 
  with \code{GPFoblq}.
  \code{factanal} always uses the correlation matrix, so standardizing does 
  not affect the solution. 
  
  If \code{diff.} is \code{TRUE} (the default) the indicator data is differenced
  before it is passed to \code{factanal}. This is necessary if the data is not
  stationary. The resulting Bartlett predictor (rotated)
  is applied to the undifferenced data. See Gilbert and Meijer (2005) for a 
  discussion of this approach.

  If \code{rotation} is \code{"none"} the result of the \code{factanal} 
  estimation is not rotated. In this case, to avoid confusion with a rotated
  solution, the factor covariance matrix \code{Phi} is returned as \code{NULL}.
  Another possibility for its value would be the identity matrix, but this is
  not calculated so \code{NULL} avoids confusion.
  
  The arguments \code{rotation}, \code{methodArgs}, \code{normalize},
  \code{eps}, \code{maxit}, and \code{Tmat} are passed to
  \code{\link[GPArotation]{GPFoblq}}. 

  The estimated loadings, Bartlett predictor and predicted factor scores 
  are put in a \code{\link[tsfa]{TSFmodel}} which is part of the returned object.
  The Bartlett predictor can be calculated as 

  \deqn{(B' \Omega^{-1} B)^{-1} B' Omega^{-1} x}{%
        (B' Omega exp(-1) B) exp(-1) B' Omega exp(-1) x},

  or equivalently as

   \deqn{(B' Sigma^{-1} B)^{-1} B' Sigma^{-1} x,}{%
         (B' Sigma exp(-1) B) exp(-1) B' Sigma exp(-1) x},

  The first is simpler because \eqn{\Omega}{Omega} is diagonal, but breaks down 
  with a Heywood case, because  \eqn{\Omega}{Omega} is then singular (one or 
  more of its diagonal elements are zero). The second only requires 
  nonsingularity of \eqn{\Sigma}{Sigma}. Typically, \eqn{\Sigma}{Sigma} is not singular 
  even if \eqn{\Omega}{Omega} is singular.
  \eqn{\Sigma}{Sigma} is calculated  from \eqn{B  \Phi B' + \Omega}{B  Phi B' + Omega}, 
  where \eqn{B, \Phi,}{B, Phi,} and \eqn{\Omega}{Omega} are the
  estimated values returned from factanal and rotated. The data covariance could
  also be used for \eqn{\Sigma}{Sigma}. (It returns the same result with this estimation 
  method.)
  
  The returned \code{TSFestModel} object is a list containing
  \describe{
     \item{model}{the estimated \code{TSFmodel}.} 
     \item{data}{the indicator data used in the estimation.} 
     \item{estimates}{a list of}
        \item{estimation}{a character string indicating the name of the
	         estimation function.} 
        \item{diff.}{the setting of the argument \code{diff.}.} 
	\item{rotation}{the setting of the argument \code{rotation}.}
	\item{uniquenesses}{the estimated \code{uniquenesses}.}
	\item{BpermuteTarget}{the setting of the argument \code{BpermuteTarget}.}
  }  }
\references{
  Gilbert, Paul D. and Meijer, Erik (2005)
  Time Series Factor Analaysis with an Application to Measuring Money.
  Research Report 05F10, University of Groningen, SOM Research School.
  Available from  \url{http://som.eldoc.ub.rug.nl/reports/themeF/2005/05F10/}.

  Gilbert, Paul D. and Meijer, Erik (2006) Money and Credit Factors.
  Bank of Canada Working Paper 2006-3, available at 
  \url{http://www.bankofcanada.ca/2006/03/publications/research/working-paper-2006-3/}.

  Tom Wansbeek and Erik Meijer (2000) \emph{Measurement Error and 
   Latent Variables in Econometrics}, Amsterdam: North-Holland.

  }

\examples{
data("CanadianMoneyData.asof.28Jan2005", package="CDNmoney")
data("CanadianCreditData.asof.28Jan2005", package="CDNmoney")

cpi <- 100 * M1total / M1real
seriesNames(cpi) <- "CPI"
popm <- M1total / M1PerCapita
seriesNames(popm) <- "Population of Canada"

z <- tframed(tbind(
    MB2001,
    MB486 + MB452 + MB453 ,
    NonbankCheq,
    MB472 + MB473 + MB487p,
    MB475,
    NonbankNonCheq + MB454 + NonbankTerm + MB2046 + MB2047 + MB2048 +
    MB2057 + MB2058 + MB482),
    names=c("currency", "personal cheq.", "NonbankCheq",
    "N-P demand & notice", "N-P term", "Investment" )
  )


TotalMoney <- tframed(rowSums(z), tframe(z))

z <- tbind (z, ConsumerCredit, ResidentialMortgage,
    ShortTermBusinessCredit, OtherBusinessCredit)

z <-tfwindow(z, start=c(1981,11), end=c(2004,11))
scale <- tfwindow(1e8 /(popm * cpi), tf=tframe(z))

MBandCredit <- sweep(z, 1, scale, "*")
c4withR2M  <- estTSF.R2M(MBandCredit, 4)
tfplot(ytoypc(factors(c4withR2M)),
       Title="Factors from 4 factor model (year-to-year growth rate)")
tfplot(c4withR2M, graphs.per.page=3)
summary(c4withR2M)
}

\seealso{
   \code{\link[tsfa]{TSFmodel}},
   \code{\link[tsfa]{estTSF.ML}},
   \code{\link[GPArotation]{GPFoblq}},
   \code{\link[GPArotation]{rotations}},
   \code{\link{factanal}}
}
\author{Paul Gilbert and Erik Meijer}
\concept{TSFA}
\keyword{ts}

