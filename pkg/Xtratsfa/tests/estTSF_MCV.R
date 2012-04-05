require("Xtratsfa")

data("CanadianMoneyData.asof.6Feb2004", package="CDNmoney")

fuzz <- 1e-6
all.ok <- TRUE  

### Construct data

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
    "N-P demand & notice", "N-P term", "Investment")
    )

z <- tfwindow(z, start=c(1986,1))
if( all(c(2003,12) ==end(z))) z <-tfwindow(z, end=c(2003,11))
MBcomponents <- 1e8 * z/matrix(tfwindow(popm * cpi,tf=tframe(z)),Tobs(z),6)

### Specify "true" parameters and factors

Omega <- diag(c(72.633490218431234, 1233.026245431895177, 87.337721037020572,
              629.392699084312198, 3967.982989812266169, 12163.258995566555313))

Boblq <- t(matrix(c( 
     8.8424730199723260,   5.2034757439511159,
    23.8239003553122046, -12.5767326858555819,
     5.1878379834837549,  -1.9710231572687940,
    36.7834439249370746,  16.9430526918934632,
    -2.8494845070603847,  31.0224248853059343,
     2.6047417719514878,  47.6304267232332990), 2,6))

PhiOblq <- t(matrix(c(
    1.0000000000000002220, 0.0094910545788177599,
    0.0094910545788177599, 1.0000000000000002220),2,2))

etaBart <- MBcomponents %*% solve(Omega) %*% Boblq %*% (
              solve( t(Boblq) %*% solve(Omega) %*% Boblq ) )

DetaBart <- diff(etaBart, lag=1)
SDE      <- cov(DetaBart)       
RR1 <- chol(SDE)      # upper triangular: SDE = RR1' RR1
RR2 <- chol(PhiOblq)  # ditto
PP  <- t(RR2) %*% solve(t(RR1))

etaTrue <- tframed(etaBart %*% t(PP), tf=tframe(MBcomponents))
Psi       <- 0.5 * Omega

rngValue10 <- list(seed=10, kind="Mersenne-Twister", normal.kind="Inversion")

simBoblq  <- simulate(TSFmodel(Boblq, f=etaTrue, 
             positive.measures=FALSE), Cov=Psi, rng=rngValue10) #, noIC=TRUE)


###  Tests to check that calculated values have not changed

MCV <- estTSF.MCV(simBoblq, 2, BpermuteTarget=Boblq)

fuzz <- 1e-3  # these calculations seems pretty loose
fuzz <- 5e-3  # Ubuntu R-2.15.0 32bit with new BLAS, April 2012

sc <- c(1, .001, 1, .1, .001, .001)
# tst <- diag(c(71.42258,   799.9341,   98.45952,   551.7778,   3347.282,   15654.85))
# tst <- diag(c(71.4225821, 799.934200, 98.4595138, 551.777863, 3347.28245, 15654.8487081)
#  tst <- diag(c(71.422581583681989, 799.934205589292446, 98.459513786151646,
#             551.777874545039595, 3347.282450463255373, 15654.848694313757733))
#  above values were for standardized solution in early code version
#  tst <- diag(c(11846.2227854133052, 1473425.34799820324, 13339.2314805768619,
#                1211867.99363448448, 13121645.4180298559, 276236562.08169359))
#  above was with noIC=TRUE
# tst <- diag(c(9966.78703285981646, 2870921.23687064834, 10806.4046910142024, 1278091.05062660715, 13390468.3517206945, 228421802.981480092))  #Ubuntu R-2.15.0 32bit with new BLAS, April 2012
# tst <- diag(c(9966.78707990818839, 2870921.21104566799, 10806.4047895932163, 1278091.0373137875,  13390468.3000131808, 228421805.47522977 ))  #Linux Mandrake 32bit
# tst <- diag(c(9966.78709684525347, 2870921.20979451109, 10806.4046430631024, 1278091.03411219502, 13390468.3642241284, 228421806.29665792 ))  # rhlinux 2.4.2 32 bit
  tst <- diag(c(9966.7870963179721,  2870921.20457918523, 10806.4047684349462, 1278091.03316546604, 13390468.3072009329, 228421806.313319385))  #Solaris 

 err <- max(diag(sc) * abs( TSFmodel(MCV)$Omega - tst ))
 if( fuzz < err ) {
    cat("Calculated value is not the same as test value in test MCV Omega. Value:\n")
    cat("fuzz:", fuzz, "  err:", err, "\n")   
    printTestValue(diag(TSFmodel(MCV)$Omega), digits=18)
    cat("sc * difference:\n")
    print(sc * (diag(TSFmodel(MCV)$Omega) - tst), digits=18)
    all.ok <- FALSE  
    }

fuzz <- 1e-4 
fuzz <- 2e-4 # Ubuntu R-2.15.0 32bit with new BLAS, April 2012


# using early version of GPA (which had maxit=500, eps=1e-5, and possibly did
#   normalizing by default)
# tst <- t(matrix(c( 
#       9.6866236556731,   1.2156968549497,
#      17.9895767862875, -26.1636780627097,
#       4.3301526007547,  -4.0822934487357,
#      39.4829847914003,   9.0531912201123,
#       7.4118579519281,  23.2372975261621,
#      10.5951625314755,  43.3583874127802,), 2,6)) 

# tst <- t(matrix(c( 
#      9.68663723189133563 , 1.21554402804356032 , 
#      17.9891888023571624 , -26.1639729507464942, 
#      4.33009141537641185 , -4.08236356269950562, 
#      39.4830997183881465 , 9.05256988612227786 , 
#      7.41219266841411439 , 23.2371892270431815 , 
#      10.5957883486466713 , 43.3582364486415983 ), 2,6 ))
#  above values were for standardized solution in early code version

# tst <- t(matrix(c( 
#      124.751337808165744 , 15.6546322554203083 , 
#      772.055488039573902 , -1122.89882148998618, 
#      50.4003585755806895 , -47.516915385944209 , 
#      1850.36224102330857 ,  424.24565500000989 , 
#      464.081940294668414 , 1454.89470466839225 , 
#      1407.50328648021605 , 5759.53938387771541), 2, 6))
#  above was with noIC=TRUE
 tst <- t(matrix(c( 
     109.354937454847843, 73.001795485260331 ,  849.958197318555676, -82.564164508841003 ,  75.9666761936852453, -33.779799193871618 ,  1447.47533281976303, 1207.13633223128204, 13.4443728967860263, 1822.2528547668453,  -1210.59538374664044, 7054.36195742316522
#    109.354938938463064, 73.0017935937713673,  849.958181642596173, -82.5641635307219559,  75.9666761758222009, -33.7798011412103563,  1447.47535841832178, 1207.13629711310136, 13.4444178739732099, 1822.2528378858824,  -1210.59537627989221, 7054.3621375790517 # Ubuntu R-2.15.0 32bit with new BLAS, April 2012
     ), 2, 6))

 err <- max(abs( loadings(TSFmodel(MCV)) - tst))
 if( fuzz < err ) {
    cat("Calculated value is not the same as test value in test MCV B. Value:\n")
    cat("fuzz:", fuzz, "  err:", err, "\n")   
    printTestValue(loadings(TSFmodel(MCV)), digits=18)
    cat("difference:\n")
    print(loadings(TSFmodel(MCV)) - tst, digits=18)
    all.ok <- FALSE  
    }

      
# tst <- t(matrix(c(
#     0.0282660945928011054 , 0.00532388742712527983 , 0.0100080365499765726 ,
#  0.0147802011762550936 , 0.00034203296490857262 , 9.29305758269045027e-05 ,
#     0.0091296645921125838 , -0.0226178722663341047 , -0.0289490407797269797,
#  0.00986116492877566093, 0.00467540068524106671, 0.00186900858818767773), 6,2))
#  above values were for standardized solution in early code version

#tst <- t(matrix(c( 
#    0.0021948054681218453 , 0.000124040336464103525 , 0.00085979049475167033 ,
#    0.000315383954203503239 , 5.46404221332856348e-06 , 6.99811003524386613e-07, 
#    0.000708864031780438763 , -0.000527006691047342724 , -0.0024871365499258228,
#    0.000210413247346309727 , 7.46741413033312425e-05 , 1.40700186026543045e-05),
#      6, 2))
#  above was with noIC=TRUE
tst <- t(matrix(c( 
      0.00235110282533738555 , 0.000154202151030251197 , 0.00464088114676523313,
      0.000167135414647799797 , -5.40725249262582089e-05 , -1.49349497208849697e-05, 
      0.00130144904891139131 , -0.000141069169012622624 , -0.00525085351390403975, 
      0.000280987014057921451 , 0.000105501927863463382 , 2.61584287352887356e-05),
        6, 2 ))

 if( fuzz < max(abs(MCV$LB - tst ))) {
    cat("Calculated value is not the same as test value in test MCV LB. Value:\n")
    printTestValue(MCV$LB, digits=18)
    cat("difference:\n")
    print(MCV$LB - tst, digits=18)
    all.ok <- FALSE  
    } 


cat("tests completed.\n")

if (! all.ok) stop("some tests FAILED")
