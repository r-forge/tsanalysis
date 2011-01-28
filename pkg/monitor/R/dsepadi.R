
############################################################################

#    functions for TSdata interface to TSdbi    <<<<<<<<<<

##############################################################################
##################################################################

refresh <- function(data, serIDs=TSseriesIDs(data), con=options()$TSconnection){
   if (is.null(serIDs)) stop("data must include source seriesIDs to use refresh.")
   TSget(serIDs=serIDs, con=con )
  }

############################################################

#   Definition of class "IOseriesIDs" <<<<<<<<<<

############################################################




# This IOseriesIDs constructor was once called make.TSnames

# Making seriesIDs an S4 class causes difficulties because then
#  tfstart.seriesIDs, etc need to be S4 methods.

setClassUnion("OptionalCharacter", c("character", "NULL"))

#setClass("seriesIDs", representation(
#       input   = "OptionalCharacter"), 

#setClassUnion("OptionalseriesIDs", c("seriesIDs", "NULL"))

# It would be nice if this could have two args
#  input=seriesIDs(...), output=seriesIDs(...) but
#  that may require making seriesIDs S4 (see above)
# IOseriesIDs <- function( output=NULL, input=NULL)
#     classed(list(input=input, output=output), c("IOseriesIDs", "TSdata")) # constructor 

#setClass("IOseriesIDs", representation( 
#     input = "OptionalseriesIDs, output = "OptionalseriesIDs)

# The problem here is that I need :periodsInput.IOseriesIDs etc to dispatch as
#  an S3 class for IOseriesIDs and TSdates, etc to dispatch as S4.

# Date IS NOT A DEFINED CLASS
# I'M NOT SURE transforms IS ACTUALLY USED ANYWHERE
setClass("IOseriesIDs", representation( 
    output="OptionalCharacter",	      input="OptionalCharacter",
    output.transforms="OptionalCharacter",    input.transforms="OptionalCharacter", 
    output.names="OptionalCharacter", input.names="OptionalCharacter",
    #start="Date", end="Date", frequency="integer", 
    pad.start="logical", pad.end="logical")
    )

IOseriesIDs <- function( output=NULL,		input=NULL,
			output.transforms=NULL,  input.transforms=NULL, 
			output.names=NULL,     input.names=NULL,
			 start=NA, end=NA, frequency=NA, 
			 pad=FALSE, pad.start=pad, pad.end=pad)
			 #server="", db="", start.server=NULL, 
			 #server.process=NULL, cleanup.script=NULL,
			 #stop.on.error=TRUE, warn=TRUE)
  {# a more logical way to do this would be for i and o to be class seriesIDs 
   #  but that requires making seriesIDs S4
    new("IOseriesIDs", 
    output=output,	                    input=input,
    output.transforms=output.transforms,    input.transforms=input.transforms, 
    output.names=output.names,              input.names=input.names,
    pad.start=pad.start,                   pad.end=pad.end)
   }

#IOseriesIDs <- function( output=NULL,           input=NULL,
#                        output.server=server,  input.server=server,
#                        output.db=db,          input.db=db,
#                        output.transforms="",  input.transforms="", 
#                        output.names=NULL,     input.names=NULL,
#                         start=NA, end=NA, frequency=NA, 
#                         pad=FALSE, pad.start=pad, pad.end=pad)
#                         #server="", db="", start.server=NULL, 
#                         #server.process=NULL, cleanup.script=NULL,
#                         #stop.on.error=TRUE, warn=TRUE)
#  {i <- if (is.null(input)) NULL else seriesIDs(input, 
#      transforms=input.transforms, names=input.names, 
#      start=start, end=end, frequency=frequency,
#      pad.start=pad.start, pad.end=pad.end)
#      #server=input.server, db=input.db, start.server=start.server, 
#      #server.process=server.process,  cleanup.script=cleanup.script,
#      #stop.on.error=stop.on.error, warn=warn)
#
#   o <- if (is.null(output)) NULL else seriesIDs(output, 
#      transforms=output.transforms, names=output.names, 
#      start=start, end=end, frequency=frequency,
#      pad.start=pad.start, pad.end=pad.end)
#      #server=output.server, db=output.db, start.server=start.server, 
#      #server.process=server.process, cleanup.script=cleanup.script,
#      #stop.on.error=stop.on.error, warn=warn)
#
#    classed(list(input=i, output=o), c("IOseriesIDs", "TSdata")) # constructor 
#   }





IOseriesIDs2 <- function(input=NULL, output=NULL,
    start = NA, end = NA, frequency = NA, pad.start = FALSE, 
    pad.end = FALSE)
    #start.server = NULL, 
    #server.process = NULL, cleanup.script = NULL, stop.on.error = TRUE, 
    #warn = TRUE)
  {i <- o <- NULL
   for (j in seq(length=length( input))) i <- cbind(i,  input[[j]])
   for (j in seq(length=length(output))) o <- cbind(o, output[[j]])
   IOseriesIDs(input =     i[1,], output=            o[1,],
        input.transforms= i[2,], output.transforms= o[2,],
        input.names=      i[3,], output.names=      o[3,],
      start = start, end = end, frequency = frequency, pad.start = pad.start, 
      pad.end = pad.end)
      #start.server = start.server, 
      #server.process = server.process, cleanup.script = cleanup.script,
      #stop.on.error = stop.on.error, warn = warn)
   }
  

############################################################

#     methods for IOseriesIDs class objects <<<<<<<<<<

############################################################

print.IOseriesIDs <- function(x, ...) {print.default(x) }

is.IOseriesIDs <- function(obj) {inherits(obj, "IOseriesIDs") }

# TSdata methods should work for start, end, frequency


tsp.IOseriesIDs <- function(x)
  {i <- tsp( inputData(x))
   o <- tsp(outputData(x))
   if (is.null(o)) return(i)
   if (is.null(i)) return(o)
   if (!all(i == o)) 
      warning("tsp results differ for input and output data. Using output")
   o
}

#periodsInput.IOseriesIDs <- function(x) periods( inputData(x))  
#periodsOutput.IOseriesIDs <- function(x) periods(outputData(x))  
#periods.IOseriesIDs <- function(x) periods(outputData(x))

 
#inputData.IOseriesIDs <- function(x, series=seq(length=nseriesInput(x)))
#{if(is.null(x@input))  NULL else  x@input[series]}

#outputData.IOseriesIDs <- function(x,series=seq(length=nseriesOutput(x)))
#{if(is.null(x@output)) NULL else  x@output[series]}


setMethod("nseriesInput",  "IOseriesIDs",
  definition= function(x) {length(x@input)} )

setMethod("nseriesOutput", "IOseriesIDs",
  definition= function(x) {length(x@output)} )

# seriesNamesInput, seriesNamesOutput default should work

#identifiers.IOseriesIDs <- function(obj) 
#	{list(input=identifiers(obj$input), output=identifiers(obj$output))}
#sourcedb.IOseriesIDs <- function(obj) 
#	{list(input=sourcedb(obj$input), output=sourcedb(obj$output))}
#sourceserver.IOseriesIDs <- function(obj) 
#	{list(input=sourceserver(obj$input), output=sourceserver(obj$output))}
setMethod("TSmeta",  signature(x="IOseriesIDs", con="missing"),
  definition= function(x, con, ...){
	i <- TSmeta(x$input)
	o <- TSmeta(x$output)
        new("TSmeta", 
	   serIDs=c(i@serIDs, o@serIDs), 
           con=c(i@con, o@con), 
           ExtractionDate= c(i@ExtractionDate,o@ExtractionDate)) 
	} )

setMethod("TSmeta",  signature(x="TSdata", con="missing"),
  definition= function(x, con, ...){
	i <- TSmeta(x$input)
	o <- TSmeta(x$output)
        new("TSmeta", 
	   serIDs=c(i@serIDs, o@serIDs), dbname=c(i@dbname, o@dbname),
           con=c(i@con, o@con), 
           ExtractionDate= c(i@ExtractionDate,o@ExtractionDate),
	   TSdescription="", TSdoc="") 
	} )

setMethod("TSmeta",  signature(x="TSestModel", con="missing"),
  definition= function(x, con, ...){TSmeta(TSdata(x))})

#
############################################################

#      Database interface for IOseriesIDs  <<<<<<<<<<

############################################################

# this seems to be needed for an S3 class defined somewhere
setMethod("TSget",   signature(serIDs="seriesIDs", con="missing"),
   definition= function(serIDs, con=options()$TSconnection, ...) 
       TSget(c(serIDs), con=con, ...) )


# Try to make this S4 without above
#   see also IOseriesIDs generator at top of file
setMethod("TSget",   signature(serIDs="IOseriesIDs", con="missing"),
   definition= function(serIDs, con=options()$TSconnection, ...) 
       TSget(serIDs, con=con, ...) )


setMethod("TSget",   signature(serIDs="IOseriesIDs", con="ANY"),
  definition= function(serIDs, con=options()$TSconnection,  ...)
{ # ... arguments unused
  # This function retreives data from a PADI server using getpadi
  # See TSget.
  # combine to align input and output 
  z <- TSget(c(serIDs@input,serIDs@output),  con=con, ...)
  i <- if (is.null(serIDs@input))  NULL else 
     selectSeries(z, series= seqN(nseries(serIDs@input)))
  o <- if (is.null(serIDs@output))  NULL else 
     selectSeries(z, series= nseries(serIDs@input) + seqN(nseries(serIDs@output)))

  TSdata(input=i, output=o)
} )



setMethod("TSdates", signature(serIDs="IOseriesIDs", con="ANY"),
  definition= function(serIDs, con=getOption("TSconnection"),  
       vintage=getOption("TSvintage"), panel=getOption("TSpanel"),  ...)  
{#  (... further arguments, currently disregarded)
 # Indicate  dates for which data is available. 

  i <- if (0 ==  nseriesInput(serIDs)) NULL else 
	     TSdates( serIDs@input, con=con, vintage=vintage, panel=panel, ...)
  o <- if (0 == nseriesOutput(serIDs)) NULL else 
	     TSdates(serIDs@output, con=con, vintage=vintage, panel=panel, ...)
  if (is.null(i) & is.null(o)) stop("No data.")
  r <- cbind(i,o)
  attr(r, "TSdates") <-     c(attr(i, "TSdates"), attr(o, "TSdates"))
  attr(r, "start")    <- append(attr(i, "start"),     attr(o, "start"))
  attr(r, "end")      <- append(attr(i, "end"),       attr(o, "end"))
  attr(r, "tbl")       <- rbind(attr(i, "tbl"),       attr(o, "tbl"))
  attr(r, "TSrefperiod") <- rbind(attr(i, "TSrefperiod"), attr(o, "TSrefperiod"))
  class(r) <- "TSdates"
  r
} )


# This should be converted to TSdbi, but TSpadi does not (yet)support TSput.
putpadi.TSdata <- function(data, server=PADIserver(),  dbname,
                   series=seriesNames(data),
                   start.server=TRUE, server.process=PADIserverProcess(), 
                   cleanup.script=PADIcleanupScript(),
                   user=Sys.info()[["user"]], passwd="",
                   stop.on.error=TRUE, warn=TRUE, timeout=60){   
   #dbname and server can be a single string in which case it is applied to
   # all series. Otherwise it should be a structure like series: a list with
   # elements input and output, each vectors with a string for each series.

   # This function uses putpadi.default 

   m <-nseriesInput(data)
   p <-nseriesOutput(data)

   if(!is.list(dbname)) 
     {z <-dbname[1]
      dbname  <- list(input  = if (m==0) NULL else rep(z,m),
                      output = if (p==0) NULL else rep(z,p) )
     }

   if(!is.list(server)) 
     {z <-server[1]
      server <- list(input  = if (m==0) NULL else rep(z,m),
                     output = if (p==0) NULL else rep(z,p) )
     }

   if (m == 0) i <- NULL else
     {if(all (1 == start(inputData(data))))
         warning("Fame may choke on a start date of 1,1")
      mat <- tframed(inputData(data), list(start = start(inputData(data)), 
                frequency=frequency(inputData(data))))

      i <- putpadi(mat, server=server$input, dbname=dbname$input, 
         series=series$input,
         start.server = start.server, server.process = server.process, 
         cleanup.script = cleanup.script,
         user=user, passwd=passwd, stop.on.error=stop.on.error, warn=warn,
	 timeout=timeout)   
     }
   if (p == 0) o <- NULL else
     {if(all (1 == start(outputData(data))))
         warning("Fame may choke on a start date of 1,1")
      mat <- tframed(outputData(data), list(start = start(outputData(data)),
                 frequency=frequency(data)))
      o <- putpadi(mat,  server=server$output, dbname=dbname$output, 
         series = series$output,
         start.server = start.server, server.process = server.process, 
         cleanup.script = cleanup.script,
         user=user, passwd=passwd, stop.on.error=stop.on.error, warn=warn,
	 timeout=timeout)   

     }
   #This bypasses the constructor (structures are already built by putpadi):
   invisible(classed(list(input=i, output=o), c("IOseriesIDs", "TSdata"))) # bypass constructor 
  }



retrieve.and.verify.data <- function(data.names,
             verification.data=verification.data, fuzz=1e-10)  
{# retrieve  data from a data base and do some verification. 
 # It is often useful if one of these is set:
 #    data.names$pad =T
 #    data.names$pad.end = T
 data <- TSget(data.names)  
 #   check that data has not been rebased or otherwise messed up.
 if (0 != (nseriesInput(data)))
   {s <-startInput(verification.data)
    e <-endInput(verification.data)
    error <- inputData(verification.data) -
               tfwindow(inputData(data),start=s, end=e, warn=FALSE)
    if (fuzz < max(abs(error)) )
      {warning(paste("Retrieved input variables do not compare with the verification data.",
       "  Differences occur at ", sum(abs(error)>fuzz), " data points. ",
       " The maximum error is ", max(abs(error))))
       key<-as.character(parse(prompt="plot discrepancy?  y/n: "))
       if (key=="y" | key=="Y")
         {z <- TSdata(input=error)
          tfplot(z, 
              select.inputs=(1:nseriesInput(z))[apply(error,2,any)],
              select.outputs= 0)
         }
      key<-as.character(parse(prompt="plot data and verification data?  y/n: "))
       if (key=="y" | key=="Y")
         {graph.data <- data
          outputData(graph.data) <-tfwindow(outputData(data),start=s,end=e)
          inputData(graph.data)  <-tfwindow(inputData(data), start=s,end=e)
          tfplot(verification.data, graph.data,
            select.inputs=(1:nseriesInput(data))[apply(error,2,any)], select.outputs= 0)
         }
      }
   }
 s <-startOutput(verification.data)
 e <-endOutput(verification.data)
 error <-  outputData(verification.data) -
              tfwindow(outputData(data),start=s,end=e, warn=FALSE)
 if (fuzz < max(abs(error))  )
   {warning(paste("Retrieved output variables do not compare with the verification data.",
    "  Differences occur at ", sum(abs(error)>fuzz), " data points. ",
    " The maximum error is ", max(abs(error))))
    key<-as.character(parse(prompt="plot discrepancy?  y/n: "))
    if (key=="y" | key=="Y")
      {z <- TSdata(output=error)
       tfplot(z, select.inputs=0,
            select.outputs= (1:nseriesOutput(z))[apply(error,2,any)])
      }
    key<-as.character(parse(prompt="plot data and verification data?  y/n: "))
    if (key=="y" | key=="Y")
      {graph.data <- data
       outputData(graph.data) <-tfwindow(outputData(data),start=s,end=e)
       inputData(graph.data)  <-tfwindow(inputData(data), start=s,end=e)
       tfplot(verification.data, graph.data, select.inputs=0,
            select.outputs= (1:nseriesOutput(data))[apply(error,2,any)])
      }
   }
data
}
