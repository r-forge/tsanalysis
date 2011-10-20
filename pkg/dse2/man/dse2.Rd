
\name{dse2-package}
\alias{dse2-package}

\docType{package}

\title{Transition Package from dse Bundle to EvalEst Package.}

\description{Package to aid transition from dse Bundle to EvalEst package.
}

\section{Usage}{
   library("dse2") 
}

\details{
The only purpose of this package is to prevent breaking older code that loads 
package dse2 from the old dse bundle. This package dse2 simple depends on
package EvalEst, so dse and  EvalEst are loaded.
}
