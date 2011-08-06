
############################################
 
# Note that there is no need to account for the observation lag, the data is
#  entered as NA if it is not available, and the filter fills it in.

# can these be combined with as.weekly in tframePlus?

#  need also conversion to daily

expandMtoW <- function(x, fromStart=start(x), notreleased=NA, na=NA){
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

expandQtoW <- function(x, fromStart, notreleased=NA, na=NA){
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


extractWeekly.daily <- function(x, fromStart, day=5, notreleased=NA, na=NA){
    # NEED NA=-1 OPTION for previous day (Thursday)
   if (1 != frequency(x)) stop("data must be daily.")
   require("zoo")
   x <- tfwindow(x, start=fromStart)
   x[is.na(x)] <- notreleased # or na?

   #as.Date(0) = Thursday Jan 1, 1970
   # from 1970 to end of x + a bit
   #require("tis") also defines month and year
   fridays <- as.Date(day-4, origin="1970-01-01") + 
                    7 * 0:(53*(1+ 1900 + as.POSIXlt(end(x))$year - 1970))
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
