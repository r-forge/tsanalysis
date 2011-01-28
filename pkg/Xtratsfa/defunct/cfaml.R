# cfaml.R: confirmatory factor analysis by ML.
# Erik Meijer, October 2004

# The model is the model used in the TSFA paper:
#   x = B xi + eps,
# with E(xi) = kappa, Cov(xi) = Phi, E(eps) = 0, Cov(eps) = Omega (diagonal),
# so that
#   mu    = E(x)   = B kappa
#   Sigma = Cov(x) = B Phi B' + Omega

######################################################################
CFA <- function(data, Nfact, tol=1.0e-8, itmax=250) {

# several utility functions are defined locally in CFA

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

  require("stats")
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

