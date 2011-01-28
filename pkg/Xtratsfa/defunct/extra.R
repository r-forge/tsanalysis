

# tfplotFiles could go in tframe, but it is not clear how useful and generic it is (ie.
#  it should use standard pstscript args, etc, etc.
tfplotFiles <- function(x, ..., baseName="xxx", 
         postscriptArgs=list(file=paste(baseName,".ps", sep=""),
	                horizontal=FALSE, width=6, height=8),
         pngArgs=list(file=paste(baseName,".png", sep=""),
	                width = 480, height = 480, pointsize=12, bg = "white"),
	 ps2eps=TRUE, 
	 plotFunction="tfplot", plotFunctionArgs=NULL)
  {do.call(plotFunction, append(list(x, ...), plotFunctionArgs))
   do.call("postscript", postscriptArgs)
   #postscript(file=psName, horizontal=FALSE, width=6, height=8)
   do.call(plotFunction, append(list(x, ...), plotFunctionArgs))
   dev.off()
   if (ps2eps) 
      system(paste("ps2eps ", baseName, ".ps ", baseName, ".eps", sep=""))
   if(capabilities()["png"])
     {do.call("png", pngArgs)
      #png(filename =pngName, width = 480, height = 480, pointsize=12, bg = "white")
      do.call(plotFunction, append(list(x, ...), plotFunctionArgs))
      dev.off()
     }
    invisible()
   }


Dfactors <- function(x)UseMethod("Dfactors")
Dfactors.TSFmodel    <- function(x) diff(factors(x))
Dfactors.TSFestModel <- function(x) Dfactors(TSFmodel(x))

Dfactors.EstEval  <- function(x)
   {N <- length(x$result)
    r <- vector(mode="list", length=N)
    for (i in 1:N) r[[i]] <- Dfactors(x$result[[i]])
    classed(list(result=r, truth=Dfactors(x$truth)),
            c("DfactorsEstEval", "EstEval"))
  }


distribution.DfactorsEstEval <- distribution.factorsEstEval
