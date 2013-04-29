if ( !require("stats"))   stop("package stats is required.")
if ( !require("EvalEst"))    stop("package EvalEst is required.")
if ( !require("monitor")) stop("package monitors is required.")
if ( !require("TSdbi"))   stop("package TSdbi is required.")

 Sys.info()
 DSEversion()

 cat("search path ", search(),"\n")
 cat("user name set to ", Sys.info()[["user"]], "\n")
 cat("nodename is ",      Sys.info()[["nodename"]], "\n")


###########################################################################

# Tests function for TSdbi data retrieval for simple monitoring    <<<<<<<<<<<<

###########################################################################

con <- TSfinddb(dbname="ets", driverOrder =c("padi","MySQL"))

if( is.null(con)) warning("Warning: ets is needed.") else {
  fuzz.small <- 1e-14
  fuzz.large <- 1e-8
 

  cat("TSdbi monitor test 1 ...\n")

  options(TSconnection=con)

  all.ok <- TRUE

 cat("TSdbi monitor test 2 ...\n")
  EXCH.IDs <- t(matrix(c(
 	"M.SDR.CCUSMA02.ST",	 "SDR/USD exchange rate",
 	"M.CAN.CCUSMA02.ST",	 "CAN/USD exchange rate",
 	"M.MEX.CCUSMA02.ST",	 "MEX/USD exchange rate",
 	"M.JPN.CCUSMA02.ST",	 "JPN/USD exchange rate",
 	"M.EMU.CCUSMA02.ST",	 "Euro/USD exchange rate",
 	"M.OTO.CCUSMA02.ST",	 "OECD /USD exchange rate",
 	"M.G7M.CCUSMA02.ST",	 "G7   /USD exchange rate",
 	"M.E15.CCUSMA02.ST",	 "Euro 15. /USD exchange rate"
	), 2, 8))
  # N.B. This should show that the last 3 are NA.
  print(TSdates(EXCH.IDs[,1]) )
  print(TSdates(EXCH.IDs[,1], con) )
   
  z    <- TSget(EXCH.IDs[1:5,1], con)
  EXCH <- TSget(EXCH.IDs[1:5,1])
  seriesNames(EXCH)
  seriesNames(EXCH) <- EXCH.IDs[1:5,2]
  seriesNames(EXCH)
  ok <- testEqual(z, EXCH)
  all.ok <- all.ok & ok 
  if (ok) cat("ok\n") else  cat("failed!\n") 


cat("TSdbi monitor test 3 ...\n")

  ETScpixa  <- TSget(serIDs="V41444257", con, names="CPIxa")
 
  ETStbc  <- annualizedGrowth(aggregate(TSget(serIDs="V122647", con),
  			      nfrequency=4,FUN=mean), names="Y/Y Growth TBC")
  tfplot(ETStbc )
 
  ETScpixa  <- aggregate(TSget("V41444257", con, names="CPIxa"), nfrequency=4,FUN=mean)
 
  ETSsbc    <- annualizedGrowth(aggregate( TSget("V122646", con),
  			      nfrequency=4,FUN=mean), names="Y/Y Growth Short.Bus.Cr.")
  tfplot(ETSsbc )
  ETSlbc    <- annualizedGrowth(aggregate( TSget("V36419", con),
  			      nfrequency=4,FUN=mean), names="Y/Y Growth Long.Bus.Cr.")
  ETSthc    <- annualizedGrowth(aggregate( TSget("V36415", con),
  			      nfrequency=4,FUN=mean), names="Y/Y Growth Tot House.Cr")
  ETScc    <- annualizedGrowth(aggregate( TSget("V122707", con),
  			      nfrequency=4,FUN=mean), names="Y/Y Growth Cons.Cr.")
  ETSrmc    <- annualizedGrowth(aggregate( TSget("V122746", con),
  			      nfrequency=4,FUN=mean), names="Y/Y Growth Res.Mort.Cr.")
  ETSgm1    <- annualizedGrowth(aggregate( TSget("V37141", con),
  			      nfrequency=4,FUN=mean), names="Y/Y Growth Gross M1")
  #ETSm1p    <- annualizedGrowth(aggregate( TSget("V37151", con),
  z <- tfwindow( TSget("V37151", con), start=c(1976,1))
  ETSm1pp    <- annualizedGrowth(aggregate( TSget("V37152", con),
        		    nfrequency=4,FUN=mean), names="Y/Y Growth M1++")
  ETSm2    <- annualizedGrowth(aggregate( TSget("V37128", con),
        		    nfrequency=4,FUN=mean), names="Y/Y Growth M2")
  ETSm2p    <- annualizedGrowth(aggregate( TSget("V37131", con),
  			  nfrequency=4,FUN=mean), names="Y/Y Growth M2+")
  ETSm2pp   <- annualizedGrowth(aggregate( TSget("V37150", con),
                            nfrequency=4,FUN=mean), names="Y/Y Growth M2++")

  # NEED SOMETHING HERE ok <- testEqual(z, EXCH)
  all.ok <- all.ok & ok 
  if (ok) cat("ok\n") else  cat("failed!\n") 


cat("TSdbi monitor test 4 ...\n")
  test.data.names <-  t(matrix(c( 
      "V37151",  "M1+",
      "V37128",  "M2",
      "V371311", "M2+",
      "V37150",  "M2++"), 2, 4))
               
 
  zz <-TSdates(test.data.names[,1], con) 
    
  z <-TSdates(test.data.names[,1], verbose=TRUE)
  print(z)
  print(start(z)) 
  print(end(z)) 
  ok <- all(c(start(z)[[1]] == c(1975,3), 
              start(z)[[2]] == c(1968,1), 
              is.na(start(z)[[3]]), 
              start(z)[[4]] == c(1968,1)))
  all.ok <- all.ok & ok 
   {if (ok) cat("ok\n") else  cat("failed!\n") }

cat("TSdbi monitor test 5 ...\n")

  cat("TSdescription not supported by padi.")
  print(TSdescription(TSget("B103", con, TSdescription=TRUE)))
  cat("TSdoc not supported by padi.")
  print(TSdoc(TSget("B103", con, TSdoc=TRUE)))

  tfplot(TSget("B103", con, names="B103"))

  z <- TSget("B103", con, TSdescription=TRUE)
  tfplot(z, Title=TSdescription(z))



cat("TSdbi monitor test 6 ...\n")

#  test.data.names <- seriesIDs(c(
#      "B14017", "P484549", "I37026", "lfsa201","b3400"),   pad.end =TRUE)
#  test.data.names <- IOseriesIDs(output=
#      seriesIDs(c( "V37151", "V37128", "V37150" ), pad.end =TRUE))
  test.data.names <- IOseriesIDs(
        output=c( "V37151", "V37128", "V37150" ),
        output.transforms=c( "", "", "" ),
        pad.end =TRUE)
   
  z <- TSdates(test.data.names) 
  ok <- all(c(start(z)[[1]] == c(1975,3), 
              start(z)[[2]] == c(1968,1),
	      start(z)[[3]] == c(1968,1)))
  all.ok <- all.ok & ok 
   {if (ok) cat("ok\n") else  cat("failed!\n") }


# the following sets ets.test.data, monitor.test.data, verification.data
#      and  monitoring.test
#  source(paste(path.package("monitor"),"/otherdata/monitoring.test.info", sep=""))
  source(system.file("otherdata", "monitoring.test.info", package="monitor"))

  cat("TSdbi monitor test 7 ...\n") 
  v.data <- verification.data
  outputData(v.data) <- outputData(v.data)[,c(1,2,6,7)]
  tframe(outputData(v.data)) <- tframe(outputData(verification.data))
  ok <- is.TSdata(v.data)
  all.ok <- all.ok & ok 
   {if (ok) cat("ok\n") else  cat("failed!\n") }

  cat("Skipping TSdbi monitor test 8 ...\n")
##THE VERIFICATION DATA NEEDS TO BE UPDATED TO NEW SERIES
#  hist.data <-retrieve.and.verify.data(test.data.names, 
#                                    verification.data=v.data)
#  ok <- testEqual(hist.data, ets.test.data, fuzz=fuzz.small)
#  all.ok <- all.ok & ok 
#   {if (ok) cat("ok\n") else  cat("failed!\n") }
#
#
#  cat("TSdbi monitor test 9 ...\n")
#  monitoring<-simpleMonitoring (monitoring.test.model, test.data.names, 
#        previous.data=NULL, mail.list=Sys.info()[["user"]], error.mail.list=Sys.info()[["user"]]) 
#  ok <-  monitoring$status == "Simple monitoring initialized."   
#  cat("\n This test produces a warning: Input is not longer than output data. No forecasts produced...")
#  # note that the following does not result in forecasts (and the forecast
#  #   function produces a warning) because the input data does not extend
#  #   beyond the output data.
#  monitoring<-simpleMonitoring (monitoring.test.model, test.data.names, 
#           previous.data=monitoring$data, 
#	   mail.list=Sys.info()[["user"]], error.mail.list=Sys.info()[["user"]]) 
#  ok <- ok & (monitoring$status == "Simple monitoring updates not necessary.")
#  monitoring<-simpleMonitoring (monitoring.test.model, test.data.names, 
#               previous.data=monitoring$data, 
#               mail.list=Sys.info()[["user"]],
#	       error.mail.list=Sys.info()[["user"]], run.again=TRUE) 
#  ok <- ok & (monitoring$status == "Simple monitoring re-run.")
#  ok <- ok & monitoring$message[7] == 
#          "1993 Sep   0.110000   0.383440   0.397520   0.355500   0.947460 "
#  ok <- ok & sum(outputData(monitoring$data))==235.64806565791809589
#  outputData(monitoring$data) <- 
#               tfwindow(outputData(monitoring$data), end=c(1993,8))
#  monitoring<-simpleMonitoring (monitoring.test.model, test.data.names, 
#          previous.data=monitoring$data, 
#	  mail.list=Sys.info()[["user"]], error.mail.list=Sys.info()[["user"]]) 
#  ok <- ok & (monitoring$status == "Simple monitoring updated.") &
#      sum(outputData(monitoring$data)) == 235.64806565791809589
#  all.ok <- all.ok & ok 
#   {if (ok) cat("ok\n") else  cat("failed!\n") }
#
#
#  cat("TSdbi monitor test 10 ...\n")
#
#  watch <- watch.data(test.data.names, previous.data=NULL, mail.list=Sys.info()[["user"]])
#  ok <- (watch$status == "System watch.data initialized.") & 
#         sum(outputData(watch$data))== 235.64806565791809589
#  watch <- watch.data(test.data.names, previous.data=watch, mail.list=Sys.info()[["user"]])
#  ok <- ok & (watch$status == "No data updates.") & 
#           sum(inputData(watch$data))== -4.1300000572204575988
#  watch$data <- tfwindow(watch$data, end=c(1993, 8))
#  watch <- watch.data(test.data.names, previous.data=watch, mail.list=Sys.info()[["user"]])
#  ok <- ok & (watch$status == "Data has been updated.")  
#  if (ok) cat("ok\n") else  cat("failed!\n") 
#
#  all.ok <- all.ok & ok 
#  tst <-  sum(outputData(watch$data))
#  good <- 235.64806565791809589
#
#   error <- max(abs(good - tst))
#   cat("max. error ", max(error), "\n")
#   
#   if (any(is.na(error)) || any(is.nan(error)) || fuzz.small < error) 
#     {if (any(is.na(error)))  cat("na's: ",  is.na(error), "\n")
#      if (any(is.nan(error))) cat("nan's: ", is.nan(error), "\n")
#      if (fuzz.small < error) cat("error: ", error, "\n")
#      printTestValue(c(tst), digits=18)
#      all.ok <- FALSE
#     }
#


if (!all.ok) stop("TSdbi monitor tests FAILED")

}
