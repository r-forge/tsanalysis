
# Title is not passed from tfplot to tfOnePlot, but that would be nice for
# the case when tfOnePlot is used directly (to put matrix on one plot.
# If that can be done then this can be part of tfOnePlot

# (I don't think tfOnePlot needs to support ...)

tfOne<- function(x, tf=tframe(x), start=tfstart(tf), end=tfend(tf),
        Title=NULL, lty=1:5, lwd=1,
        pch=NULL, col=1:6, cex=NULL, xlab=NULL, ylab=NULL,
        xlim=NULL, ylim=NULL, par=NULL, 
	lastObs=TRUE,  source=NULL, footnote=NULL,
	legend=NULL, legend.loc="topleft"){
    #BUG?  Title is not passed in tfOnePlot (main instead)
    tfOnePlot(x, tf=tf, start=start, end=end, lty=lty, lwd=lwd,
           pch=pch, col=col, cex=cex, xlab=xlab, ylab=ylab,
           xlim=xlim, ylim=ylim, par=par) 
    if (!is.null(Title) && (is.null(options()$PlotTitles) ||
        options()$PlotTitles)) title(main = Title)	
    if (!is.null(source) && (is.null(options()$Plotsource) ||
        options()$Plotsource)) 
	     mtext(source, side=1, line = 2, adj=0, cex=0.7)	
    if (lastObs) {
       if(frequency(x) == 12)dt <- paste(c("Jan", "Feb","Mar","Apr","May",
          "Jun","Jul","Aug","Sep","Oct","Nov","Dec")[end(x)[2]],end(x)[1],
	      collapse=" ")
       else if(frequency(x) == 4)dt <- paste(
                c("Q1", "Q2","Q3","Q4")[end(x)[2]],end(x)[1], collapse=" ")
       else   dt <- end(x)
       last <- paste("Last observation:", dt)
       mtext(last, side=1, line = 2, adj=1, cex=0.7)
       }
    # footnote will go on another line with \n
    if (!is.null(footnote) && (is.null(options()$Plotfootnote) ||
        options()$Plotfootnote)) 
	     mtext(footnote, side=1, line = 3, adj=0, cex=0.7)	
    if (!is.null(legend)) legend(legend.loc, inset = c(0.05, .05), 
       col=col, lty=lty, cex=0.7, legend=legend)
    invisible(x)
    }

############################################
 
# Note that there is no need to account for the observation lag, the data is
#  entered as NA if it is not available, and the filter fills it in.

expandMtoW <- function(x, fromStart=start(x), notreleased=-7000, na=-99999){
     # assign monthly data into the last Friday of a month to 
     # give a weekly Friday series with na inserted where there is no data.
     # notreleased is for values which will eventually be available.
   if (12 != frequency(x)) stop("data must be monthly.")
   x <- tfwindow(x, start=fromStart)
   require("zoo")

   #as.Date(0) = Thursday Jan 1, 1970
   # from 1970 to end of x + a bit
   fridays <- as.Date(1, origin="1970-01-01") + 7* 0:(53* (1+ end(x)[1] -1970))
   #require("tis") also defines month and year
   year <- function(x) {1900 + as.POSIXlt(x)$year}
   month <- function(x) {1 + as.POSIXlt(x)$mon}
   # window to span of x
   st <-  (year(fridays)  < start(x)[1]) | 
         ((year(fridays) == start(x)[1]) & (month(fridays) < start(x)[2]))
   en <-  (year(fridays)  > end(x)[1])   | 
         ((year(fridays) == end(x)[1])   & (month(fridays) > end(x)[2]))
   
   fridays <- fridays[(!st) & (!en)]
   mn <- c(month(fridays), 13) # 13 to insure last obs is included
   # logical indicating last Fridays of the month, plus the last 
   Wind <- mn[-1] != mn[-length(mn)]
   # lastFridays <- fridays[Wind]

   x[is.na(x)] <- notreleased 
   
   r <- matrix(NA, Tobs(Wind), nseries(x))
   r[Wind,] <- x
   r[is.na(r)] <- na
   r <- zoo(r, order.by=fridays)
   seriesNames(r) <- seriesNames(x)
   r
   }

expandQtoW <- function(x, fromStart, notreleased=-7000, na=-99999){
     # assign quarterly data into the last Friday of a quarter to 
     # give a weekly Friday series with na inserted where there is no data.
     # notreleased is for values which will eventually be available.
   if (4 != frequency(x)) stop("data must be quarterly.")
   x <- tfwindow(x, start=fromStart)
   require("zoo")

   #as.Date(0) = Thursday Jan 1, 1970
   # from 1970 to end of x + a bit
   fridays <- as.Date(1, origin="1970-01-01") + 7* 0:(53* (1+ end(x)[1] -1970))
   # window to span of x
   #require("tis") also defines month and year
   year <- function(x) {1900 + as.POSIXlt(x)$year}
   month <- function(x)   {1 + as.POSIXlt(x)$mon}
   q <- ceiling(month(fridays)/3)
   st <-  (year(fridays)  < start(x)[1]) | 
         ((year(fridays) == start(x)[1]) & (q < start(x)[2]))
   en <-  (year(fridays)  > end(x)[1])   | 
         ((year(fridays) == end(x)[1])   & (q > end(x)[2]))
   
   fridays <- fridays[(!st) & (!en)] # fridays in range of x
   q       <-     c(q[(!st) & (!en)], 5)# 5 to insure last obs is included
   # logical indicating last Fridays of the quarter
   Wind <- q[-1] != q[-length(q)]
   # lastFridays <- fridays[Wind]

   x[is.na(x)] <- notreleased 
   
   r <- matrix(NA, Tobs(Wind), nseries(x))
   r[Wind,] <- x
   r[is.na(r)] <- na
   r <- zoo(r, order.by=fridays)
   seriesNames(r) <- seriesNames(x)
   r
   }


extractDtoW <- function(x, fromStart, notreleased=-7000, na=-99999){
     # extract daily data from the last Friday of a week to 
     # give a weekly Friday series with na inserted where there is no data.
     # NEED NA="THURSDAY" OPTION
   if (1 != frequency(x)) stop("data must be daily.")
   require("zoo")
   x <- tfwindow(x, start=fromStart)
   x[is.na(x)] <- notreleased # or na?

   #as.Date(0) = Thursday Jan 1, 1970
   # from 1970 to end of x + a bit
   #require("tis") also defines month and year
   year <- function(x) {1900 + as.POSIXlt(x)$year}
   month <- function(x)   {1 + as.POSIXlt(x)$mon}
   fridays <- as.Date(1, origin="1970-01-01") + 7* 0:(53*(1+ year(end(x))-1970))
   # window to span of x
   st <-  fridays  < time(x)[1]
   en <-  fridays  > time(x)[Tobs(x)]
   fridays <- fridays[(!st) & (!en)]

   Wind <- time(x) %in% fridays
      
   r <- x[Wind,]
   r <- zoo(r, order.by=fridays)
   seriesNames(r) <- seriesNames(x)
   r
   }
