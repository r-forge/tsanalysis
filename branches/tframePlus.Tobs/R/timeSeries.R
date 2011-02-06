require("tframe")
require("timeSeries")

is.tframed.timeSeries <- function(x) {TRUE}

setMethod("is.tframed", "timeSeries", is.tframed.timeSeries)

tframe.timeSeries <- function (x) {
  tf <- as.POSIXct(time(x))
  tfc <-  class(tf)
  # is.numeric(value) is TRUE for integer but is(value, "numeric") is FALSE and
  # that causes timeSeries(x, charvec=value) to fail, so this works around the prob
  #tfc[tfc == "integer"] <- "numeric"
  # cannot keep the S4 class: warning no longer S4
  class(tf) <- c( "timeSeriestframe", tfc, "tframe")
  tf
  }
setMethod("tframe", "timeSeries", tframe.timeSeries)

tfUnSet.timeSeries <- function(x){as.matrix(x)}
#setMethod("tframe:::tfUnSet", "timeSeries", tfUnSet.timeSeries)

tfSet.timeSeriestframe <- function(value, x) { 
  class(value) <- class(value)[class(value) != "timeSeriestframe"]
  timeSeries:::timeSeries(x, charvec=value) }

"seriesNames<-.timeSeries" <- function (x, value) 
  {names(x) <- value
   x
  }
setMethod("seriesNames<-", "timeSeries", get("seriesNames<-.timeSeries"))

tfperiods.timeSeries <- function(x)  NROW(x)
setMethod("tfperiods", "timeSeries", tfperiods.timeSeries)

tfstart.timeSeriestframe <- function(x) x[1]
tfend.timeSeriestframe   <- function(x) x[length(x)]
tfperiods.timeSeriestframe   <- function(x) length(x)
periods.timeSeriestframe     <- function(x) length(x)
# FATAL CONFLICT: the timeSeries package defines periods differently. 
#periods.timeSeries     <- function(x) NROW(x)
#setMethod("periods", "timeSeries", periods.timeSeries)

tfL.timeSeries <- function (x, p = 1) lag(x, k = -p)
setMethod("tfL", "timeSeries", tfL.timeSeries)

tfwindow.timeSeries <- function(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
  {# With the default warn=T warnings will be issued if no truncation takes
   #  place because start or end is outside the range of data.
   if (!warn) 
     {opts <- options(warn = -1)
      on.exit(options(opts))
     }
   if(is.null(start)) start <- start(x)
   if(is.null(end))   end   <- end(x)
   y <- .window.timeSeries(x, start=start, end=end)
   seriesNames(y) <- seriesNames(x)
   attr(y, "TSrefperiod") <- attr(x, "TSrefperiod")
   y
  }
setMethod("tfwindow", "timeSeries", tfwindow.timeSeries)

tfExpand.timeSeries <- function(x, add.start = 0, add.end = 0){
   idx <- time(x)
   r <- as.matrix(coredata(x))
   if (add.start > 0 ) {
     idx <- c(start(x) - seq(add.start), idx)
     r <- rbind(matrix(NA, add.start, ncol(r)), r)
     }
   if (add.end > 0 ) {
     idx <- c(idx, end(x) + seq(add.end))
     r <- rbind(r, matrix(NA,add.end, ncol(r)))
     }
   timeSeries(r, order.by = idx) 
   }
setMethod("tfExpand", "timeSeries", tfExpand.timeSeries)

tbind.timeSeries <- function(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
 {nm <- seriesNames(x)
  ref <- attr(x, "TSrefperiod")
  for (z in list(...)) {
    if (!is.null(z)) {
      nm  <- c(nm,  seriesNames(z))
      ref <- c(ref, attr(z, "TSrefperiod"))
      x <- cbind(x, z)
      }
    }
  if (!pad.start | !pad.end)
     x <- trimNA(x, startNAs= !pad.start, endNAs= !pad.end)
  seriesNames(x) <- nm
  attr(x, "TSrefperiod") <- ref
  x
 }  
setMethod("tbind", "timeSeries", tbind.timeSeries)

