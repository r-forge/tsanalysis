require("Xtratsfa")

data("CanadianMoneyData.asof.6Feb2004", package="CDNmoney")

#require("dse")
#require("EvalEst")  # for EstEval

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
MBcomponents <- 1e8 * z/matrix(tfwindow(popm * cpi,tf=tframe(z)), Tobs(z),6)

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
########  replace with simulate
# oldrng <- setRNG(rngValue10) # do this to be able to reproduce the result
# simBoblq  <- tframed(etaTrue %*% t(Boblq) +  
#                     matrix(rnorm(215*6),215,6) %*% Psi^0.5, tframe(etaTrue))
# attr(simBoblq, "TSFmodel") <- TSFmodel(Boblq, f=etaTrue, positive.measures=FALSE) 
########
simBoblq  <- simulate(TSFmodel(Boblq, f=etaTrue, 
             positive.measures=FALSE), Cov=Psi, rng=rngValue10) #, noIC=TRUE)


###  Tests to check that calculated values have not changed

R2M <- estTSF.R2M(simBoblq, 2, BpermuteTarget=Boblq)

## estTSF.R2M

# tst <- diag(c(77.99645, 748.9573, 99.53656, 415.0742, 3398.993, 15744.05))
 tst <- diag(c(77.996445788386325, 748.957233304161718, 99.536559405226782,
              415.074278197450212, 3398.993363841897008, 15744.044865312540423))
#  above was with noIC=TRUE
 tst <- diag(c(60.1921658615183119,  1521.92750673138289, 80.1775315356583462,
     538.78541161218584,  3278.92245163269126,  13716.5150374459536))

 if( fuzz < max(abs( TSFmodel(R2M)$Omega - tst ))) {
    cat("Calculated value is not the same as test value in test R2M Omega. Value:\n")
    printTestValue(diag(TSFmodel(R2M)$Omega), digits=18)
    cat("difference:\n")
    print(TSFmodel(R2M)$Omega - tst, digits=18)
    all.ok <- FALSE  
    }


# using early version of GPA (which had maxit=500, eps=1e-5, and possibly did
#   normalizing by default)
# tst <- t(matrix(c( 
#	 9.5827796079501262,   1.194853488131824,
#	18.3146972876954202, -27.064310418133847,
#	 4.3641559737993809,  -3.965655542475369,
#	41.8253613768086652,  10.727641838770557,
#	 7.2454495413908111,  22.659136649733295,
#	11.0746701624519517,  43.028949876914211), 2,6)) 

 tst <- t(matrix(c( 
      9.58279926941862747 , 1.19468110010036321 , 
      18.3141811702646855 , -27.064649111931633 , 
      4.36407984329363074 , -3.96573546828498991, 
      41.8255511946882521 , 10.7268911600826318 , 
      7.24587486298229511 , 22.6590131466900822 , 
      11.0754786411397301 ,  43.028763740921562 ), 2,6))
#  above was with noIC=TRUE
 tst <- t(matrix(c( 
      8.30417180888775697 ,  5.9189517689730069 , 
      19.2899646003996743 , -1.10169539929101057 , 
      6.78784954005505003 , -2.99664699926621569 , 
      30.1804808001785112 , 27.0028414319191192 , 
      -0.0226193992892303811 , 29.1059985036404107 , 
      -9.99041567888333937 , 55.5319485924123271), 2,6))
      
 if( fuzz < max(abs( loadings(TSFmodel(R2M)) - tst ))) {
    cat("Calculated value is not the same as test value in test R2M B. Value:\n")
    printTestValue(loadings(TSFmodel(R2M))  , digits=18)
    cat("difference:\n")
    print(loadings(TSFmodel(R2M)) - tst, digits=18)
    all.ok <- FALSE  
    }
      
 tst <- t(matrix(c(
     0.020023526108538952, 0.00581641466686578981, 0.00926283108801265534,
   0.0158018466954418756, 4.83143348135837777e-05, -8.97171237737207903e-06,
     0.00346727161930753675, -0.0228639823708243414, -0.0259972997881601428,
   0.0108268112952340824 , 0.00390757155775263605 , 0.00160995131439499038), 6,2))
#  above was with noIC=TRUE
 tst <- t(matrix(c(
      0.029012912423980592 , 0.00646674389461628866 , 0.0558705582001856327 , 
      0.00780976386778681718 , -0.00346133050041043258, -0.00193253873557866561, 
      0.0156845837155822933 , -0.0054485846455110936 , -0.0593682414836703906, 
    0.0135641196765327893, 0.00627000005559145979, 0.00314217277690974957), 6,2))


 if( fuzz < max(abs(R2M$LB - tst ))) {
    cat("Calculated value is not the same as test value in test R2M LB. Value:\n")
    printTestValue(R2M$LB, digits=18)
    cat("difference:\n")
    print(R2M$LB - tst, digits=18)
    all.ok <- FALSE  
    } 

cat("tests completed.\n")

if (! all.ok) stop("some tests FAILED")
