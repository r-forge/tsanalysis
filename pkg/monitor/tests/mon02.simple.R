   require("stats")
   require("EvalEst")
   require("monitor")
#   A TS PADI server is necessary for these tests.
   require("TSpadi")

  Sys.sleep(15) # just in case a previous server has not yet died


 Sys.info()
 DSEversion()

 cat("search path ", search(),"\n")
 cat("PATH set to ",  Sys.getenv("PATH"), "\n")
 cat("PADI set to ",  Sys.getenv("PADI"), "\n")
 cat("PADI_LDLIB set to ",  Sys.getenv("PADI_LDLIB"), "\n")
 cat("PADI_STARTUP set to ", Sys.getenv("PADI_STARTUP"), "\n")
 cat("PADI_CLEANUP set to ", Sys.getenv("PADI_CLEANUP"), "\n")
 cat("user name set to ", Sys.info()[["user"]], "\n")


###########################################################################

# Tests function for data retrieval for simple monitoring    <<<<<<<<<<<<

###########################################################################

if ( ! require("TSpadi") )
     warning("Warning: package TSpadi is needed.") else
if (system.file("exec", "/fame.server", package="TSpadi")
               != PADIserverProcess() ) 
     warning("Warning: padi fame server is needed.")   else {

fuzz.small <- 1e-14
fuzz.large <- 1e-8
server.process <- PADIserverProcess()
cleanup.script <- PADIcleanupScript()
	 
 # The main short coming of these tests is that they do not test
 #     functions which produce output or graphs.
 # These tests require access to Fame data bases and the files:
 #          monitoring.test.db    fake database 
 #          monitoring.test.info  comparison info. to check results

 # Note also that the test data is not real data (it may have been differenced
 #  or otherwise transformed) and is only intended to test that functions
 #  work as originally specified. 

  server <- Sys.info()[["nodename"]]
#  db     <- paste(path.package("monitor"),"/otherdata/monitoring.test.db",sep="")
  db     <- system.file("otherdata", "monitoring.test.db", package="monitor")

  all.ok <- TRUE

  cat("simple monitor test 0 ...\n")
  # simulate a database server
  pid <- startPADIserver(server=server,
           dbname=db, 
           server.process=server.process)
	   

  # wait for server to start 
     for (i in 1:30)
       {if (checkPADIserver(server)) break
        Sys.sleep(1)
       }
  ok <- TRUE
  all.ok <- all.ok & ok 
   {if (ok) cat("ok\n") else  cat("failed!\n") }


  cat("simple monitor test 1 ...\n")
  #  db=db would not be nec. with a public mode fame server   
  test.data.names <- TSPADIdata(
      input  ="B14017", 
      output = c( "P484549", "I37026", "lfsa201","b3400"), 
      server=server, db=db, pad.end =TRUE,
      start.server=FALSE)
   
  z <-TSdates(test.data.names, verbose=TRUE) 
  ok <- all(c(z$start == t(matrix(c(1974,2),2,5)), 
              z$end   == t(matrix(c(1993,9),2,5)), 
              z$freq==rep(12,5) ))
  all.ok <- all.ok & ok 
   {if (ok) cat("ok\n") else  cat("failed!\n") }


# the following sets ets.test.data, monitor.test.data, verification.data
#      and  monitoring.test
#  source(paste(path.package("monitor"),"/otherdata/monitoring.test.info", sep=""))
  source(system.file("otherdata", "monitoring.test.info", package="monitor"))

  cat("simple monitor test 2 ...\n") 
  v.data <- verification.data
  outputData(v.data) <- outputData(v.data)[,c(1,2,6,7)]
  tframe(outputData(v.data)) <- tframe(outputData(verification.data))
  ok <- is.TSdata(v.data)
  all.ok <- all.ok & ok 
   {if (ok) cat("ok\n") else  cat("failed!\n") }

  cat("simple monitor test 3 ...\n")
  hist.data <-retrieve.and.verify.data(test.data.names, 
                                    verification.data=v.data)
  ok <- testEqual(hist.data, ets.test.data, fuzz=fuzz.small)
  all.ok <- all.ok & ok 
   {if (ok) cat("ok\n") else  cat("failed!\n") }


  cat("simple monitor test 4 ...\n")
  monitoring<-simpleMonitoring (monitoring.test.model, test.data.names, 
        previous.data=NULL, mail.list=Sys.info()[["user"]], error.mail.list=Sys.info()[["user"]]) 
  ok <-  monitoring$status == "Simple monitoring initialized."   
  cat("\n This test produces a warning: Input is not longer than output data. No forecasts produced...")
  # note that the following does not result in forecasts (and the forecast
  #   function produces a warning) because the input data does not extend
  #   beyond the output data.
  monitoring<-simpleMonitoring (monitoring.test.model, test.data.names, 
           previous.data=monitoring$data, 
	   mail.list=Sys.info()[["user"]], error.mail.list=Sys.info()[["user"]]) 
  ok <- ok & (monitoring$status == "Simple monitoring updates not necessary.")
  monitoring<-simpleMonitoring (monitoring.test.model, test.data.names, 
               previous.data=monitoring$data, 
               mail.list=Sys.info()[["user"]],
	       error.mail.list=Sys.info()[["user"]], run.again=TRUE) 
  ok <- ok & (monitoring$status == "Simple monitoring re-run.")
  ok <- ok & monitoring$message[7] == 
          "1993 Sep   0.110000   0.383440   0.397520   0.355500   0.947460 "
  ok <- ok & sum(outputData(monitoring$data))==235.64806565791809589
  outputData(monitoring$data) <- 
               tfwindow(outputData(monitoring$data), end=c(1993,8))
  monitoring<-simpleMonitoring (monitoring.test.model, test.data.names, 
          previous.data=monitoring$data, 
	  mail.list=Sys.info()[["user"]], error.mail.list=Sys.info()[["user"]]) 
  ok <- ok & (monitoring$status == "Simple monitoring updated.") &
      sum(outputData(monitoring$data)) == 235.64806565791809589
  all.ok <- all.ok & ok 
   {if (ok) cat("ok\n") else  cat("failed!\n") }


  cat("simple monitor test 5 ...\n")

  watch <- watch.data(test.data.names, previous.data=NULL, mail.list=Sys.info()[["user"]])
  ok <- (watch$status == "System watch.data initialized.") & 
         sum(outputData(watch$data))== 235.64806565791809589
  watch <- watch.data(test.data.names, previous.data=watch, mail.list=Sys.info()[["user"]])
  ok <- ok & (watch$status == "No data updates.") & 
           sum(inputData(watch$data))== -4.1300000572204575988
  watch$data <- tfwindow(watch$data, end=c(1993, 8))
  watch <- watch.data(test.data.names, previous.data=watch, mail.list=Sys.info()[["user"]])
  ok <- ok & (watch$status == "Data has been updated.")  
  if (ok) cat("ok\n") else  cat("failed!\n") 

  all.ok <- all.ok & ok 
  tst <-  sum(outputData(watch$data))
  good <- 235.64806565791809589

   error <- max(abs(good - tst))
   cat("max. error ", max(error), "\n")
   
   if (any(is.na(error)) || any(is.nan(error)) || fuzz.small < error) 
     {if (any(is.na(error)))  cat("na's: ",  is.na(error), "\n")
      if (any(is.nan(error))) cat("nan's: ", is.nan(error), "\n")
      if (fuzz.small < error) cat("error: ", error, "\n")
      printTestValue(c(tst), digits=18)
      all.ok <- FALSE
     }


cleanupPADIserver(pid, cleanup.script=cleanup.script)

if (!all.ok) stop("Simpmon tests FAILED")

}
