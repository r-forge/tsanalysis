
\name{dse1-package}
\alias{dse1-package}

\docType{package}

\title{Transition Package from dse Bundle to dse Package.}

\description{Package to aid transition from dse Bundle to dse package.
}

\section{Usage}{
   library("dse1") 
}

\details{
The only purpose of this package is to prevent breaking older code that loads 
package dse1 from the old dse bundle. This package dse1 simply depends on
package dse, so dse is loaded.
}
