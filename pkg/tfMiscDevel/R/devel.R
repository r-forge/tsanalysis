
# Title is not passed from tfplot to tfOnePlot, but that would be nice for
# the case when tfOnePlot is used directly (to put matrix on one plot.
# If that can be done then this can be part of tfOnePlot

(I don't think tfOnePlot needs to support ...)

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

