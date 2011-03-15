#seriesIDs
#starty=0,startm=0,startd=1, endy=0,endm=0,endd=1, 
#        nobs=0,max.obs=2000, transformations=NULL, pad=FALSE,
#	names=series
        

seriesIDs <- function(serIDs,  transforms= "",  
           start=NA, end=NA, frequency=NA, names=NULL, 
           pad=FALSE, pad.start=pad, pad.end=pad)
  {# This is the constructor .
   if (is.null(serIDs)) return(NULL)
   if (is.null(names))   names <- serIDs
   if(length(serIDs) != length(names) )
           stop("number of names does not match number of serIDs.")
   # this makes r a matrix so some TSdata defaults work on IOseries
   #r <- rbind(serIDs, transforms)
   #attr(r, "transforms")     <- transforms
   #dimnames(r) <- list(c("serIDs", "transforms") ,names)
   r <- serIDs
   attr(r, "transforms")     <- transforms
   seriesNames(r) <- names
   attr(r, "start")     <- start
   attr(r, "end")       <- end
   attr(r, "frequency") <- frequency
   attr(r, "pad.start") <- pad.start
   attr(r, "pad.end")   <- pad.end
   class(r) <- "seriesIDs"
   r
   }

is.seriesIDs <- function(obj) {inherits(obj, "seriesIDs") }

print.seriesIDs <- function(x, ...)
  {print.default(x)
   invisible(x)
  }

tframe.seriesIDs <- function(x) 
   if(is.null(attr(x, "tframe"))) NA else attr(x, "tframe")

tfstart.seriesIDs <- function(x, ...)
     {if(is.null(attr(x, "start"))) NA else attr(x, "start")}
tfend.seriesIDs <- function(x, ...)
     {if(is.null(attr(x, "end")))   NA else attr(x, "end")}
tffrequency.seriesIDs <- function(x, ...)
     {if(is.null(attr(x, "frequency")))   NA else attr(x, "frequency")}
Tobs.seriesIDs <- function(x) NA  # could be better
seriesNames.seriesIDs <- function(x) {dimnames(x)[[2]]}
# nseries default should work
 
