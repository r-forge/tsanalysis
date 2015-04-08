
estTSF.R2M <- function(y, p, diff.=TRUE,
                    rotation=if(p==1) "none" else "quartimin", 
		    rotationArgs=NULL,  
		    normalize=TRUE, eps=1e-5, maxit=1000, Tmat=diag(p),
                    BpermuteTarget=NULL,
		    factorNames=paste("factor", seq(p))) {
      # estimate from raw 2nd moments

      if (p < 1) stop("number of factors must be a positive integer.")
      
      Sigma  <- if (diff.) (crossprod(diff(y)))/(Tobs(y) - 1)
                      else (crossprod(y))/(Tobs(y))
      z <- factanal(covmat = Sigma, factors=p, scores="none",
        	           rotation="none", n.obs=(Tobs(y) - diff.))

      estConverged <- z$converged

      stds      <- sqrt(diag(Sigma))     # "standard deviations"
      hatOmega  <- stds * z$uniquenesses * stds
      uniquenesses <- z$uniquenesses
      rownorms  <- sqrt(rowSums(z$loadings^2))
      if (rotation == "none") {
         hatB <- diag(stds) %*% z$loadings              
	 Phi  <- NULL
	 rotationConverged <- TRUE
	 orthogonal <- TRUE
	 }
      else {	           
   	 rotB	   <- GPFoblq(diag(1/rownorms) %*% z$loadings, # KBorth
   			  Tmat=Tmat, 
			  normalize=normalize, eps=1e-5, maxit=1000,
			  method=rotation, methodArgs=rotationArgs )
   	 hatB	   <- diag(stds * rownorms) %*% loadings(rotB)
	 rotationConverged <- rotB$convergence
   	 orthogonal <- rotB$orthogonal

   	 # for debugging compare:  hatOmega - Omega, hatOmega, Omega
   	 
   	 # Make sure columns are ordered properly and have the correct signs.
   	 Phi <- rotB$Phi
   	 if (! is.null(BpermuteTarget)) {
   	    zz  <- permusign(hatB, BpermuteTarget, Phi=Phi)
   	    hatB <- zz$loadings
   	    Phi  <- zz$Phi
   	    }
	 }
      dimnames(hatB) <- list(seriesNames(y), factorNames)
      # for debugging compare: hatB - Btrue, hatB, Btrue
       
      ### Compute Bartlett-predicted factor scores     
      #BO <- t(hatB) %*% diag(1/hatOmega) or
      #BO <- crossprod(hatB, diag(1/hatOmega) )
      #LB   <- solve(BO %*% hatB, BO)
      Sigma <- if(is.null(Phi)) hatB %*% t(hatB) + diag(hatOmega) else
                        hatB %*% Phi %*% t(hatB) + diag(hatOmega)
      BO <- crossprod(hatB, solve(Sigma) )
      LB   <- solve(BO %*% hatB, BO)
      dimnames(LB) <- list(factorNames, seriesNames(y))

      z <- FAmodel(hatB, Omega=diag(hatOmega), Phi=Phi, LB=LB, 
	    stats=list(est="R2M",
	      estConverged=estConverged, rotationConverged=rotationConverged,
	      orthogonal=orthogonal, uniquenesses=uniquenesses, call=match.call()) )
      model <- TSFmodel(z, 
                        f=tframed(y %*% t(LB), tframe(y), names=factorNames), #hatf
			positive.data=all(0<y))
      model$data <- y 
      classed(model, c("TSFmodel", "fFAmodel", "FAmodel"))
 }

######################################
#######################################

# this is the second method used in the Monte Carlo experiment (100 iterations)

estTSF.MCV <- function(y, p, diff.=TRUE, 
                      rotation=if(p==1) "none" else "oblimin", 
		      rotationArgs=NULL, 
		      normalize=TRUE, eps=1e-5, maxit=1000, Tmat=diag(p),
		      BpermuteTarget=NULL,
                      factorNames=paste("factor", seq(p))) {
  
      if (p < 1) stop("number of factors must be a positive integer.")
      if(! is.null(rotationArgs))
         stop("rotation rotationArgs not yet supported by estTSF.MCV")
      #zz <- if (diff.) diff(y) else y
      #zz <- sweep(zz,2,colMeans(zz), "-")
      #Sigma <- cov(zz) which is the same as
      #Sigma  <- crossprod(zz)/(Tobs(zz) - 1)
      #Sigma  <- crossprod(zz)/(Tobs(zz))
      #Sigma  <- crossprod(if (diff.) diff(y) else y)/(Tobs(y)-diff.)
 
      # estimate from mean + covariance structure 
      z <- EFA.ML.mcs(if(diff.) diff(y) else y, p)

      estConverged <- z$converged  #PROBABLY NULL STILL
      
      stds	<- apply((if (diff.) diff(y) else y), 2, sd) 
      uniquenesses <- z$uniquenesses
      hatOmega  <- stds * uniquenesses * stds 
      #hatOmega  <- uniquenesses # standardized used in early versions
       
      # for debugging compare:  hatOmega - Omega, hatOmega, Omega

      # z$loadings is orth solution
      if (rotation == "none") {
         hatB <- diag(stds) %*% z$loadings              
         #hatB <- z$loadings   # standardized loadings used in early versions           
	 Phi  <- NULL
	 rotationConverged <- TRUE
	 orthogonal <- TRUE
  	 }
      else {	 
    	 rotB <- GPFoblq(z$loadings, Tmat=Tmat, 
			 normalize=normalize, eps=1e-5, maxit=1000,
    	                 method=rotation, methodArgs=rotationArgs)
    	 hatB <- diag(stds) %*% loadings(rotB)  
    	 #hatB <-  loadings(rotB) # standardized loadings used in early versions 
	 rotationConverged <- rotB$convergence
	 orthogonal <- rotB$orthogonal
      
    	 # Make sure columns are ordered properly and have the correct signs.
    	 Phi <- rotB$Phi
    	 if (! is.null(BpermuteTarget)) {
    	    zz  <- permusign(hatB, BpermuteTarget, Phi=Phi)
    	    hatB <- zz$loadings
    	    Phi  <- zz$Phi
    	    }
	 }

      dimnames(hatB) <- list(seriesNames(y), factorNames)
      # for debugging compare: hatB - Btrue, hatB, Btrue
  
      ### Compute Bartlett-predicted factor scores
      ##BO <- t(hatB) %*% diag(1/hatOmega) 
      #BO <- crossprod(hatB, diag(1/hatOmega) )
      #LB   <- solve(BO %*% hatB, BO)
      #  Use theoretically calculated  Sigma = B Phi B' + Omega 
      #  instead of data cov.
      Sigma <- if(is.null(Phi)) hatB %*% t(hatB) + diag(hatOmega) else
                        hatB %*% Phi %*% t(hatB) + diag(hatOmega)
      BO <- crossprod(hatB, solve(Sigma) )
      LB   <- solve(BO %*% hatB, BO)

      dimnames(LB) <- list(factorNames, seriesNames(y))

      z <- FAmodel(hatB, Omega=diag(hatOmega), Phi=Phi, LB=LB, 
	    stats=list(est="MCV",
	      estConverged=estConverged, rotationConverged=rotationConverged,
	      orthogonal=orthogonal, uniquenesses=uniquenesses, call=match.call()) )
      model <- TSFmodel(z, 
                        f=tframed(y %*% t(LB), tframe(y), names=factorNames), #hatf
			positive.data=all(0<y))
      model$data <- y 
      classed(model, c("TSFmodel", "fFAmodel", "FAmodel"))
     }

######################################################################
# Currently CFA below is used only in estTSF.MCV (above) and there are some
#  utility functions from erikbase.R defined locally in CFA.
# It is possible some of this can be generalized for other use, and some 
#  cleaned up or removed.
#
# cfaml.R: confirmatory factor analysis by ML.
# Erik Meijer, October 2004
#
# The model is the model used in the TSFA paper:
#   x = B xi + eps,
# with E(xi) = kappa, Cov(xi) = Phi, E(eps) = 0, Cov(eps) = Omega (diagonal),
# so that
#   mu    = E(x)   = B kappa
#   Sigma = Cov(x) = B Phi B' + Omega
#
######################################################################

#     From Erik, Sept 22, 2005
#> I have also been trying to convert estTSF.MCV, which estimates from
#> mean + covariance structure. It does fails with a singularity
#> problem in the larger number of replications.  In the end I don't
#> think we used the results from this estimator in the paper,
#
#We haven't and I suggest you remove this estimator from the code. In
#principle, this estimator would be (slightly) more efficient if
#the data are iid and normally distributed, but this assumption was
#violated anyway and some brief experiments did not give better
#estimators I think. Maybe I even didn't get it working.
#
#> However, I am having a bit of trouble understanding the big picture. I
#> don't think you start in a confirmatory setting, but you recast the
#> estimation as a confirmatory problem. What is the general logic here?
#
#I had to devise an algorithm that computes estimators for this fit
#function. I used the ML fit function that includes the means. I tried
#to use a standard algorithm that is used for CFA models. The problem
#with EFA is its non-identification, which leads to problems with
#singular matrices in this standard algorithm. So I used a specification
#that is a particular rotation method, but is explicitly imposed during
#the iterations, so that the model is identified. This particular
#rotation is defined by the restriction that all elements B(i,j) of B
#with j>i are zero. It does not affect the class of possible solutions
#after oblimin rotation, so this is a convenient way to obtain
#estimators.

  EFA.ML.mcs <- function(data, Nfact, tol=1.0e-8, itmax=250) {
  # Erik Meijer, October 2004, modified by PG Sept 2005

    # Exploratory factor analysis with ML estimation
    # using mean and covariance structure
    # and an iterative scoring algorithm.

    # First estimate parameters of an identified CFA model
    # NOTE!!! Uniquenesses is a diagomal matrix, not a vector
    # as in factanal. Maybe change this later.
  
    CFAresults <- CFA(data, Nfact, tol, itmax)
    #Omega   <- CFAresults$uniquenesses
  
 # Transform to orthogonal solution
     Phi    <- t(chol(CFAresults$Phi))	# Phi = cPhi cPhi'
  
    list(loadings = CFAresults$loadings %*% Phi, 
         uniquenesses = diag(CFAresults$uniquenesses), 
  	 kappa = solve(Phi, CFAresults$kappa), 
	 Phi = Phi)
  }  


CFA <- function(data, Nfact, tol=1.0e-8, itmax=250) {

# several utility functions are defined locally in CFA
vec <- function(A) {  matrix(c(A),ncol=1)}
# vec of a matrix: stack its columns in a vector


unit.vec <- function(i, n) {

  # i-th unit vector of order n,
  # i.e., i-th column of eye(n)

#  e.in <- zeros(n,1)
  e.in <- matrix(0,n,1)
  e.in[i,1] <- 1

  return(e.in)
}


diagonalization.matrix <- function(n) {

  # Diagonalization matrix of order n

  D <- NULL
  for (j in 1:n) {
    vv <- unit.vec(j,n) %x%  unit.vec(j,n)  # Kronecker product
    D <- cbind(D, vv)
  }

  return(D)
}

duplic <- function(n) {

  # Duplication matrix of order n

  D <- NULL
  for (j in 1:n) {
    vv <- unit.vec(j,n) %x%  unit.vec(j,n)  # Kronecker product
    D <- cbind(D, vv)
    if (j < n) {
      for (i in (j+1):n) {
        vv <- unit.vec(i,n) %x%  unit.vec(j,n) + unit.vec(j,n) %x%  unit.vec(i,n)
        D  <- cbind(D, vv)
      }
    }
  }

  return(D)
}

i.duplic <- function(n) {

  # Moore-Penrose inverse of duplication matrix of order n

  D   <- duplic(n)                          # duplication matrix
  i.D <- diag(1/diag(t(D) %*% D)) %*% t(D)  # (D'D)^{-1} D', where D'D is diagonal

  return(i.D)
}


parameter.matrices <- function(theta, Nvar, Nfact){

  # Extract parameters from parameter vector theta and put them
  # in the relevant parameter matrices.
  # unvecs is a local function
  unvecs <- function(v) {

      # Compute symmetric matrix A from its nonduplicated elements
      # v = "symmetric" vec of a matrix A: only the nonduplicated elements

      v <- as.matrix(v) # Column vector = matrix with 1 column
      p <- nrow(v)	# Number of elements

      # Determine n = number of rows (and columns) of A.
      # p = n(n+1)/2  <=>  n = sqrt(2p + 0.25) - 0.5

      n = floor(sqrt(2 * p + 0.25)) # To be sure it's exactly the desired integer

      # Now process the vector

      m <- 1
      A <- matrix(nrow=n, ncol=n)
      for (j in 1:n) {
  	A[j:n,j] <- v[m:(m+n-j),1,drop=FALSE]
  	A[j,j:n] <- t(A[j:n,j,drop=FALSE])
  	m <- m + n - j + 1
      }

      return(A)
      }

  bv <- theta[1 : ( (Nvar-Nfact)*Nfact ), 1]
  B2 <- matrix(c(bv),  nrow=Nvar-Nfact, ncol=Nfact) # lower part of B
  B  <- rbind(diag(1,Nfact), B2) # combine submatrices in loadings matrix

  m      <- (Nvar-Nfact)*Nfact
  omegav <- theta[(m+1):(m+Nvar), 1]
  Omega  <- diag(c(omegav))

  m      <- m + Nvar
  kappav <- theta[(m+1):(m+Nfact), 1]
  kappa  <- matrix(c(kappav), ncol=1)

  m      <- m + Nfact
  phiv   <- theta[(m+1) : (m+(Nfact*(Nfact+1))/2), 1]
  Phi    <- unvecs(phiv)

  return(list(loadings=B,uniquenesses=Omega,kappa=kappa,Phi=Phi))
}

######################################################################

parameter.vector <- function(loadings, uniquenesses, kappa, Phi){

  # Extract parameters from parameter matrices and put them in vector theta
  # (i.e., a 1-column matrix).

   matrix(c(loadings[-(1:ncol(loadings)),] , uniquenesses, kappa,
                       Phi[outer(1:ncol(Phi),1:ncol(Phi), ">=")]), ncol=1)
}

######################################################################

minlogL <- function(theta, Nfact, S, xbar) {

  # Fit function (- log likelihood / nobs).

  # theta = parameter vector
  # Nfact = number of factors
  # S     = sample covariance matrix (with sample size in denominator,
  #           not sample size minus 1)
  # xbar  = sample mean

  # Extract parameter matrices from parameter vector.

  Nvar   <- nrow(S)
  parmat <- parameter.matrices(theta, Nvar, Nfact)
  B      <- parmat$loadings
  Omega  <- parmat$uniquenesses
  kappa  <- parmat$kappa
  Phi    <- parmat$Phi

  # Compute mean + cov. matrix.

  mu    <- B %*% kappa
  Sigma <- B %*% Phi %*% t(B) + Omega

  # Return fit function.

  D <- det(Sigma)
  if (D <= 0.0) {
    L <- Inf
  } else {
#    L <- (log(D) + sum(diag(solve(Sigma) %*% S)) +
#            t(xbar - mu) %*% solve(Sigma) %*% (xbar - mu))/2
    L <- (log(D) + sum(diag(solve(Sigma, S))) +
            crossprod(xbar - mu, solve(Sigma, (xbar - mu))))/2
  }

  return( L )
}

######################################################################

score <- function(theta, Nfact, S, xbar) {

  # Derivative of fit function (- log likelihood / nobs).

  # theta = parameter vector
  # Nfact = number of factors
  # S     = sample covariance matrix (with sample size in denominator,
  #           not sample size minus 1)
  # xbar  = sample mean

  # Extract parameter matrices from parameter vector.

  Nvar   <- nrow(S)
  parmat <- parameter.matrices(theta, Nvar, Nfact)
  B      <- parmat$loadings
  Omega  <- parmat$uniquenesses
  kappa  <- parmat$kappa
  Phi    <- parmat$Phi

  # Compute mean + cov. matrix

  mu      <- B %*% kappa
  Sigma   <- B %*% Phi %*% t(B) + Omega
  i.Sigma <- solve(Sigma)    # Sigma^{-1}

  # Discrepancies

  err.mean <- xbar - mu
  err.Cov  <- S - Sigma
  V        <- err.Cov + err.mean %*% t(err.mean)
  W        <- i.Sigma %*% V %*% i.Sigma

  # Derivative of vec(B) w.r.t. its free parameters

  Delta     <- rbind(matrix(0,Nfact, Nvar - Nfact),
                     diag(1,Nvar - Nfact))
  d.vecB.db <- diag(1,Nfact) %x% Delta

  # Derivative of fit function w.r.t. parameters

#  dL.db     <-  - t(d.vecB.db) %*% ( kappa %x% (i.Sigma %*% err.mean)
#                   + vec(W %*% B %*% Phi) )
  dL.db     <-  - crossprod(d.vecB.db, ( kappa %x% (i.Sigma %*% err.mean)
                   + vec(W %*% B %*% Phi) ))
  dL.domega <-  - matrix(diag(W),ncol=1)/2
  #dL.dkappa <-  - t(B) %*% i.Sigma %*% err.mean
  dL.dkappa <-  - crossprod(B, i.Sigma) %*% err.mean
  #dL.dPhi   <-  - t(duplic(Nfact)) %*% vec(t(B) %*% W %*% B)/2
  dL.dPhi   <-  - crossprod(duplic(Nfact), vec(t(B) %*% W %*% B))/2

  # Stack derivatives in column vector

  return(rbind(dL.db, dL.domega, dL.dkappa, dL.dPhi))
}

######################################################################

info <- function(theta, Nfact, S, xbar) {

  # Expectations of second derivative of fit function
  # (- log likelihood / nobs): information matrix.            

  # theta = parameter vector
  # Nfact = number of factors
  # S     = sample covariance matrix (with sample size in denominator,
  #           not sample size minus 1)
  # xbar  = sample mean

  # Extract parameter matrices from parameter vector.

  Nvar   <- nrow(S)
  parmat <- parameter.matrices(theta, Nvar, Nfact)
  B      <- parmat$loadings
  Omega  <- parmat$uniquenesses
  kappa  <- parmat$kappa
  Phi    <- parmat$Phi

  # Compute mean + cov. matrix

  mu      <- B %*% kappa
  Sigma   <- B %*% Phi %*% t(B) + Omega
  i.Sigma <- solve(Sigma)    # Sigma^{-1}

  # Derivative of vec(B) w.r.t. its free parameters

  Delta     <- rbind(matrix(0,Nfact, Nvar - Nfact),
                     diag(1,Nvar - Nfact))
  d.vecB.db <- diag(1,Nfact) %x% Delta

  # Diagonalization and duplication

  HM     <- diagonalization.matrix(Nvar)
  Dk     <- duplic(Nfact)
  DM     <- duplic(Nvar)
  DMplus <- i.duplic(Nvar)

  # kappa-row  and -column

  #Ikk <- t(B) %*% i.Sigma %*% B
  Ikk <- crossprod(B, i.Sigma) %*% B
  #Ibk <- t(d.vecB.db) %*% (kappa %x% (i.Sigma %*% B))
  Ibk <- crossprod(d.vecB.db, (kappa %x% (i.Sigma %*% B)))
  Ikb <- t(Ibk)
  Ikp <- matrix(0,Nfact, (Nfact * (Nfact + 1))/2)
  Ipk <- t(Ikp)
  Iko <- matrix(0,Nfact, Nvar)
  Iok <- t(Iko)

  # omega-row and -column

  Ioo <- (i.Sigma * i.Sigma)/2  # elementwise multiplication!
#  Iob <- t(HM) %*% ((i.Sigma %*% B %*% Phi) %x% i.Sigma) %*% d.vecB.db
  Iob <- crossprod(HM, ((i.Sigma %*% B %*% Phi) %x% i.Sigma)) %*% d.vecB.db
  Ibo <- t(Iob)
#  Iop <- t(HM) %*% ((i.Sigma %*% B) %x% (i.Sigma %*% B)) %*% Dk/2
  Iop <- crossprod(HM, ((i.Sigma %*% B) %x% (i.Sigma %*% B))) %*% Dk/2
  Ipo <- t(Iop)

  # phi-row and -column

  Ipp <- t(Dk) %*% ((t(B) %*% i.Sigma %*% B) %x%
                    (t(B) %*% i.Sigma %*% B)) %*% Dk/2
  Ipb <- t(Dk) %*% ((t(B) %*% i.Sigma %*% B %*% Phi) %x%
                    (t(B) %*% i.Sigma)) %*% d.vecB.db
  Ibp <- t(Ipb)

  # B-row and -column

  GB  <- DMplus %*% ((B %*% Phi) %x% diag(1,Nvar))
  Ibb <- t(d.vecB.db) %*% (
           (kappa %*% t(kappa)) %x% i.Sigma +
            2 * t(GB) %*% t(DM) %*% (i.Sigma %x% i.Sigma) %*% DM %*% GB
         ) %*% d.vecB.db

  # Combine all submatrices in Information matrix

  return( rbind( cbind(Ibb, Ibo, Ibk, Ibp),
                 cbind(Iob, Ioo, Iok, Iop),
                 cbind(Ikb, Iko, Ikk, Ikp),
                 cbind(Ipb, Ipo, Ipk, Ipp) ) )
}


######################################################################

update <- function(theta.old, Lold, Nfact, S, xbar, tol=1.0e-8) {

  # One iteration of numerical optimization algorithm for
  # quasi-ML estimation of TSFA model.

  # theta.old = parameter vector (current value)
  # Lold      = fit function (current value)
  # Nfact     = number of factors
  # S         = sample covariance matrix (with sample size in denominator,
  #               not sample size minus 1)
  # xbar      = sample mean
  # tol       = tolerance (convergence criterion)

  # Compute score (gradient, derivative of fit function)
  # and its norm (mean square).

  g     <- score(theta.old, Nfact, S, xbar)
  normg <- sqrt(mean(g^2))

  # Information matrix (expected second derivative).

  I <- info(theta.old, Nfact, S, xbar)
  A <- solve(I); # inverse of the information matrix

  # Descent direction.

  d     <-  -A %*% g
  normd <- sqrt(mean(d^2))

  # Step size selection:
  # theta.new = theta.old + alpha * d

  # If the sample size is large and the function is
  # approximately quadratic (e.g., close to a minimum)
  # then the minimum is close to   theta.old + d
  # i.e., alpha = 1. Therefore, we start with alpha = 1.

  # In this step, we are satisfied with a point with a
  # lower function value, we do not search for the minimum
  # w.r.t. alpha. We know that d is a descent direction, so that
  # the function value is lower for alpha small. Therefore,
  # we divide alpha by 2 until a lower function value is
  # obtained or alpha is so small that we must conclude
  # that a minimum has been found.

  alpha <- 1
  theta <- theta.old + alpha * d
  normdtheta <- alpha * normd
  L     <- minlogL(theta, Nfact, S, xbar)
  if (is.infinite(L) | is.nan(L)) { L <- Lold + 1.0 }
  while ((L > Lold) & (normdtheta > tol)) {
     alpha <- alpha/2
     theta <- theta.old + alpha * d
     normdtheta <- alpha * normd
     L     <- minlogL(theta, Nfact, S, xbar)
     if (is.infinite(L) | is.nan(L)) { L <- Lold + 1.0 }
  }

  # Use steepest descent step if alpha is small but g is not

  if ((normdtheta < tol) & (normg > tol)) {
     d     <- -g
     normd <- sqrt(mean(d^2))
     alpha <- 1
     theta <- theta.old + alpha * d
     normdtheta <- alpha * normd
     L     <- minlogL(theta, Nfact, S, xbar)
     if (is.infinite(L) | is.nan(L)) { L <- Lold + 1.0 }

     while ((L > Lold) & (alpha > tol)) {
        alpha <- alpha/2
        theta <- theta.old + alpha * d
        normdtheta <- alpha * normd
        L     <- minlogL(theta, Nfact, S, xbar)
        if (is.infinite(L) | is.nan(L)) { L <- Lold + 1.0 }
     }
  }

  # Still no improvement, then stop in current point

  if (L > Lold) {
    theta <- theta.old
    L     <- Lold
  }

  return (list(theta.new=theta, Lnew=L, i.Info=A, normg=normg))
}

######################################################################

CFA.ML <- function(theta.start, Nfact, S, xbar, tol=1.0e-8,
                   itmax=250) {

  # theta.start = starting value of parameter vector
  # Nfact       = number of factors
  # S           = sample covariance matrix (with sample size in denominator,
  #               not sample size minus 1)
  # xbar        = sample mean
  # tol         = tolerance (convergence criterion)
  # itmax       = maximum number of iterations

  # Starting values.

  theta.new  <- theta.start
  Lnew       <- minlogL(theta.new, Nfact, S, xbar)
  dL         <- 10000   # Improvement in loglikelihood
  normdtheta <- 10000   # Norm of diff. between old and new theta
  normg      <- 10000   # Norm of gradient
  iter       <- 0       # Number of iterations
  Table      <- cbind(iter, Lnew, dL, normdtheta, normg) # Iteration history

  # Iterate until convergence.

  while ((normdtheta > 0) & (dL > tol | normdtheta > tol | normg > tol) &
         (iter < itmax)) {

     # Bookkeeping: increase iteration number and replace "old" by "new"

     iter       <- iter + 1
     theta.old  <- theta.new
     Lold       <- Lnew

     # Compute new value of parameter vector

     upd        <- update(theta.old, Lold, Nfact, S, xbar, tol)
     theta.new  <- upd$theta.new
     Lnew       <- upd$Lnew
     i.Info     <- upd$i.Info
     normg      <- upd$normg

     # Convergence indicators

     dL         <- Lold - Lnew
     dtheta     <- theta.old - theta.new
     normdtheta <- sqrt(mean(dtheta^2))

     # Store convergence info

     Table <- rbind(Table,
                    cbind(iter, Lnew, dL, normdtheta, normg))
  }

  # Estimation results in more accessible form

  Nvar   <- nrow(S)
  parmat <- parameter.matrices(theta.new, Nvar, Nfact)

  return(list(loadings=parmat$loadings, uniquenesses=parmat$uniquenesses,
              kappa=parmat$kappa, Phi=parmat$Phi, Table=Table))
}

######################################################################
##  start main CFA function

  # data  = data matrix to be analyzed
  # Nfact = number of factors
  # tol   = tolerance (convergence criterion)
  # itmax = maximum number of iterations

  # Compute mean vector and covariance matrix

  Nobs <- nrow(data)
  Nvar <- ncol(data)
  xbar <- matrix(colMeans(data),ncol=1)
  Y    <- sweep(data, 2, xbar, "-")   # Centered data
  #Y    <- data - matrix(1, Nobs,1) %*% t(xbar)   # Centered data
  S    <- t(Y) %*% Y/Nobs      # Covariance matrix with Nobs in denominator
  stds <- diag(sqrt(diag(S)))  # Standard deviations

  # Preliminary estimates

  requireNamespace("stats")
  orig.par <- factanal(data, Nfact, scores="none", rotation="none")

  # Transform to CFA model structure

  omegav <- diag(S) * orig.par$uniquenesses
  B      <- stds  %*% orig.par$loadings
  B1     <- B[1:Nfact, , drop=FALSE]
  B2     <- B[(Nfact+1):Nvar, , drop=FALSE] %*% solve(B1)
  Phi    <- B1 %*% t(B1)
  B      <- rbind(diag(1,Nfact), B2) # So I reuse the symbol B
#  kappa  <- solve(t(B) %*% diag(1/omegav) %*% B) %*%
#                  t(B) %*% diag(1/omegav) %*% xbar
  kappa  <- solve(crossprod(B, diag(1/omegav)) %*% B,
                  crossprod(B, diag(1/omegav))) %*% xbar

  # Store in theta
  theta.start <- parameter.vector(B, omegav, kappa, Phi)

  # Compute and return ML estimators

  parmat <- CFA.ML(theta.start, Nfact, S, xbar, tol, itmax)

  return(list(loadings=parmat$loadings, uniquenesses=parmat$uniquenesses,
              kappa=parmat$kappa, Phi=parmat$Phi, Table=parmat$Table))
}

######################################################################


#######################################
#######################################
