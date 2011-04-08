require("Xtratsfa")

data("CanadianMoneyData.asof.6Feb2004", package="CDNmoney")

# for monte carlo
require("setRNG")
require("dse")
require("EvalEst")  # for EstEval

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
Psi       <- 0.5 * Omega

etaTrue <- tframed(etaBart %*% t(PP), tf=tframe(MBcomponents))


rngValue10 <- list(seed=10, kind="Mersenne-Twister", normal.kind="Inversion")

cat("Starting the trucated monte carlo experiment test...\n")

EE.MCV5 <- EstEval(TSFmodel(Boblq, f=etaTrue, positive.measures=FALSE),
    replications=5, rng=rngValue10, quiet=FALSE,
    simulation.args=list(Cov=Psi),
    estimation="estTSF.MCV", 
    estimation.args=list(2, BpermuteTarget=Boblq),
    criterion ="TSFmodel")

mc5SummaryMCV <- summary(EE.MCV5)

# next values were not tested with early version of GPA
 
# tst <-  c(-14.5402216460025961,  12.9892255173833906)
# above was with standardized solution in early code version
 tst <-  c(-60.0854468825016355,  -37.9938324777091481)

 if( fuzz < max(abs(  (mc5SummaryMCV$meanhatf.error) - tst ))) {
    cat("Calculated value is not the same as test value in test 20. Value:\n")
    printTestValue(mc5SummaryMCV$meanhatf.error, digits=18)
    cat("difference:\n")
    print(mc5SummaryMCV$meanhatf.error - tst, digits=18)
    all.ok <- FALSE  
    }

# tst <-  c(17.7942289140145107,  13.3447238388902569)
# above was with standardized solution in early code version
 tst <-  c(0.940164020252536314,  0.918165023875998854)

 if( fuzz < max(abs( mc5SummaryMCV$meanSDhatf  - tst ))) {
    cat("Calculated value is not the same as test value in test 22. Value:\n")
    printTestValue(mc5SummaryMCV$meanSDhatf, digits=18)
    cat("difference:\n")
    print(mc5SummaryMCV$meanSDhatf - tst, digits=18)
    all.ok <- FALSE  
    }

 fuzz <- 1e-4

# tst <- t(matrix(c( 
#      -2.81491897554047821 , 1.94577957258058287 , 
#      0.251395472562911948 , 9.46297427407351144 , 
#      -0.00529870794034792425 , 1.16018378047807236 , 
#      -10.550212414310046 , 8.84361111777386455 , 
#      -12.6316643834945115 , -2.0248245319356819 , 
#      -3.66249232979138561 , -2.58927951928310307), 2,6))
# above was with standardized solution in early code version
# Last element below is rounded to split tolerance error on different machines.
 tst <- t(matrix(c( 
      72.2362405131913476 , 89.6526338623291394 , 
      1074.14772498394609 , -126.153605876463416 , 
       53.270936364048616 , -6.65211394291860181 , 
      1208.95463314411791 , 1192.77746653310192 , 
      -1099.15865136841808 , 1971.80446544728056 , 
      -135.053343251278761 , 5294.9263085),  2, 6))

 if( fuzz < max(abs( mc5SummaryMCV$meanhatB - tst ))) {
    cat("Calculated value is not the same as test value in test 25. Value:\n")
    printTestValue(mc5SummaryMCV$meanhatB, digits=18)
    cat("difference:\n")
    print(mc5SummaryMCV$meanhatB - tst, digits=18)
    all.ok <- FALSE  
    }

 
# tst <- t(matrix(c( 
#       3.197043585803101 , 2.29771850555523338 , 
#      2.61490486645632592 , 9.81625786817305546 , 
#      0.792075099664212634 , 1.78399000598131963 , 
#      11.3422036195092986 , 8.37751968700625049 , 
#       15.724026037478394 , 2.64924537423176076 , 
#      6.47871909255147216 ,  10.064326660623788), 2,6)) 
# above was with standardized solution in early code version
# Last elements below is rounded to split tolerance error on different machines.
 tst <- t(matrix(c( 
      43.2467709803125402 , 29.9129953030542133 , 
      138.022222282095925 , 444.623392604936953 , 
      10.0732091693383818 , 21.1241337496177124 , 
       548.70000671915659 , 372.864638053559077 , 
       1139.3803136211618 , 204.074255658419645 , 
      799.112542          , 1398.688407), 2, 6))

# 10*  added to fuzz for 64 bit amd  in R-2.3.0
 if( 10*fuzz < max(abs(mc5SummaryMCV$SDhatB - tst))) {
    cat("Calculated value is not the same as test value in test 26. Value:\n")
    printTestValue(mc5SummaryMCV$SDhatB, digits=18)
    cat("difference:\n")
    print(mc5SummaryMCV$SDhatB - tst, digits=18)
    all.ok <- FALSE  
    }

# tst <- c(-0.0652945975227083308,  -0.00644026488751029118)
# above was with standardized solution in early code version
 tst <- c( -0.122850167837187746,  -0.237284792727374044)

 if( fuzz < max(abs( (mc5SummaryMCV$meanhatDf.error)   - tst ))) {
    cat("Calculated value is not the same as test value in test 28. Value:\n")
    printTestValue(mc5SummaryMCV$meanhatDf.error, digits=18)
    cat("difference:\n")
    print(mc5SummaryMCV$meanhatDf.error - tst, digits=18)
    all.ok <- FALSE  
    }


 tst <- c(0.71082590597566353,  0.787992361585781786)
# above was with standardized solution in early code version
 tst <- c(0.0335434181794614927,  0.0364096808659631135)

 if( fuzz < max(abs( mc5SummaryMCV$meanSDhatDf  - tst ))) {
    cat("Calculated value is not the same as test value in test 30. Value:\n")
    printTestValue(mc5SummaryMCV$meanSDhatDf, digits=18)
    cat("difference:\n")
    print(mc5SummaryMCV$meanSDhatDf - tst, digits=18)
    all.ok <- FALSE  
    }


#### this is fairly long 

cat("Starting the main monte carlo experiment...\n")


EE.MCV100 <- EstEval(TSFmodel(Boblq, f=etaTrue, positive.measures=FALSE),
    replications=100, rng=rngValue10, quiet=FALSE,
    simulation.args=list(Cov=Psi),
    estimation="estTSF.MCV", 
    estimation.args=list(2, BpermuteTarget=Boblq),
    criterion ="TSFmodel")

mc100SummaryMCV <- summary(EE.MCV100)

fuzz <- 1e-6  # there seems to be a fair amount of "wobble" in these results 


# tst <- c(-1.969009,        -4.709795)
# tst <- c(-1.9685544113573, -4.7101848975681)
# with early version of GPA
# tst <-  c(-1.9563611544622, -4.6784700057953)
# tst <-  c(-1.95637500641310136,  -4.67869413871841378)
# above was with standardized solution in early code version
tst <-  c(-59.7240184426877434,  -38.6184927178763431)
#  above was with noIC=TRUE
tst <-  c(-59.8054091666985954,  -38.7211770796564565)

 if( fuzz < max(abs(  (mc100SummaryMCV$meanhatf.error) - tst ))) {
    cat("Calculated value is not the same as test value in test 20. Value:\n")
    printTestValue(mc100SummaryMCV$meanhatf.error, digits=18)
    cat("difference:\n")
    print(mc100SummaryMCV$meanhatf.error - tst, digits=18)
    all.ok <- FALSE  
    }

#  tst <- c(12.22897,        20.30249)
#  tst <- c(12.228344427490, 20.302285491114)
# with early version of GPA
# tst <-  c(12.246121710438, 20.321084105824)
# tst  <-  c(12.2464400925217767,  20.3212420947333676)
# above was with standardized solution in early code version
 tst  <-  c( 0.693732372288098476,  1.11508558591030726)
#  above was with noIC=TRUE
 tst  <-  c(0.712394816901717132,  1.23567297792369213)

 if( fuzz < max(abs( mc100SummaryMCV$meanSDhatf  - tst ))) {
    cat("Calculated value is not the same as test value in test 22. Value:\n")
    printTestValue(mc100SummaryMCV$meanSDhatf, digits=18)
    cat("difference:\n")
    print(mc100SummaryMCV$meanSDhatf - tst, digits=18)
    all.ok <- FALSE  
    }


 fuzz <- 1e-4

# with early version of GPA
# tst <- t(matrix(c( 
#      -0.57244331788222, -0.469127560179515,
#      -2.54918273271896,  0.839393090347130,
#      -0.13508878359689,  0.079958547641539,
#      -1.91210761555443, -1.683415697759372,
#       0.69900927505192,  0.404495681826813,
#       1.88683415246242,  0.233523262819610), 2,6))

# tst <- t(matrix(c(
#      -0.57244579504683557 , -0.469152073029648342 ,
#      -2.54925121977118607 ,  0.839367053122787254 ,
#      -0.135100884059201398 , 0.0799524876656514394 ,
#      -1.91213401162778496 , -1.68351659077544369 ,
#	0.69910453525281957 ,  0.404452705643581822 ,
#	1.88698348844267594 ,  0.233478421553797943 ), 2,6))
# above was with standardized solution in early code version
 tst <- t(matrix(c(
      99.5755487983387439 ,  60.833867375597336 , 
      915.457389232527817 , -487.23073582824378 , 
      50.1522100058669338 , -17.2253632829292407 , 
      1599.12419342207886 , 758.054751018554953 , 
      -247.904856341337506 , 2129.43463562061652 , 
      405.778248485433608 , 5845.30972689446389 ), 2,6 ))
#  above was with noIC=TRUE
 tst <- t(matrix(c(
      99.6036539062267536 , 62.7237434374467853 , 
      966.350825349187176 , -492.356068711589046 , 
      47.3474752993959243 , -18.8496012296282309 , 
      1607.26083284697938 , 806.766734911688445 , 
      -174.26622770515155 , 2088.64120132036214 , 
      274.649755999069498 , 5922.54941422435877), 2,6 ))

 if( fuzz < max(abs( mc100SummaryMCV$meanhatB - tst ))) {
    cat("Calculated value is not the same as test value in test 25. Value:\n")
    printTestValue(mc100SummaryMCV$meanhatB, digits=18)
    cat("difference:\n")
    print(mc100SummaryMCV$meanhatB - tst, digits=18)
    all.ok <- FALSE  
    }
  
# with early version of GPA
# tst <- t(matrix(c( 
#       2.2032597938090,  2.7963412429274,
#       5.9399052026813,  8.7184952853986,
#       2.815108,         1.6811329393403,
#       7.3437728557175, 10.9770313355281,
#       9.5889665183615,  9.4149614078890,
#      12.1406397583375, 15.6324847), 2,6)) #split the wobble in some values

#tst <- t(matrix(c(
#      2.20330703856080135 , 2.79636321807480259 ,
#      5.93988027793994533 ,  8.7186126999584328 ,
#      2.81510505122467469 ,  1.6811557334351972 ,
#      7.34393097840708009 , 10.9771845058240078 ,
#      9.58911641599635445 , 9.41497832077391905 ,
#      12.1408159296730283 , 15.6324626238231588 ),2,6))
# above was with standardized solution in early code version
tst <- t(matrix(c( 
       33.747782503615035 , 37.1939941237903113 , 
      263.144412452769245 , 411.961667559562329 , 
      31.0273433199815685 , 19.1019205888667116 , 
      404.991277073292792 , 535.013427289835818 , 
      828.455914415739471 , 666.548519344609531 , 
      1384.08555353802331 , 2146.98127123055883), 2, 6))
#  above was with noIC=TRUE
tst <- t(matrix(c( 
       33.079269170830699 , 42.0577590125263896 , 
      309.167601919968376 , 380.932703496654085 , 
       15.409937346315564 , 21.6353434743948441 , 
      427.894001825597229 , 551.166258434966835 , 
      793.989828077939251 , 501.948126837912412 , 
      1564.85128634172997 , 1848.87016947527695), 2, 6))

 if( fuzz < max(abs(mc100SummaryMCV$SDhatB - tst))) {
    cat("Calculated value is not the same as test value in test 26. Value:\n")
    printTestValue(mc100SummaryMCV$SDhatB, digits=18)
    cat("difference:\n")
    print(mc100SummaryMCV$SDhatB - tst, digits=18)
    all.ok <- FALSE  
    }

# tst <- c(0.0047052827044636, -0.034559617091503)
# above was with standardized solution in early code version
 tst <- c( -0.120904756316695422,  -0.238458044081586856)
#  above was with noIC=TRUE
 tst <-  c(-0.121215578704896854,  -0.238682736644732246)

 if( fuzz < max(abs( (mc100SummaryMCV$meanhatDf.error)   - tst ))) {
    cat("Calculated value is not the same as test value in test 28. Value:\n")
    printTestValue(mc100SummaryMCV$meanhatDf.error, digits=18)
    cat("difference:\n")
    print(mc100SummaryMCV$meanhatDf.error - tst, digits=18)
    all.ok <- FALSE  
    }

# tst <- c(0.6828631592909, 1.0076767432928)
# above was with standardized solution in early code version
 tst <- c(0.0348549346280125072,  0.0456615156880329012)

 if( fuzz < max(abs( mc100SummaryMCV$meanSDhatDf  - tst ))) {
    cat("Calculated value is not the same as test value in test 30. Value:\n")
    printTestValue(mc100SummaryMCV$meanSDhatDf, digits=18)
    cat("difference:\n")
    print(mc100SummaryMCV$meanSDhatDf - tst, digits=18)
    all.ok <- FALSE  
    }

cat("tests completed.\n")

if (! all.ok) stop("some tests FAILED")

