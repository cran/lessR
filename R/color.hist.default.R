color.hist.default <- 
function(x, col="lightsteelblue", border="black", col.bg="ghostwhite",
         col.grid="grey90", over.grid=FALSE,
         breaks="Sturges", bin.start=NULL, bin.width=NULL,
         show.values=FALSE, prop=FALSE, cumul=c("off", "on", "both"),
         col.reg="snow2", digits.d=5, xlab=NULL, ylab=NULL, main=NULL, ...) {

  dash <- function(n.dash) { for (i in 1:(n.dash)) cat("-"); cat("\n") }
    
  if (!is.numeric(x)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "A histogram is only computed from a numeric variable.\n",
      deparse(substitute(x)), " is not a numeric variable.\n",
      "For the analysis of a categorical variable use color.barhchart.\n\n")
  }
        
  if (is.numeric(breaks) && !is.null(bin.start)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Choose only one option to specify a start value.\n",
      "Either choose the option  breaks  or the option  bin.start.\n\n")
  }

  # produce actual argument, such as from an abbreviation, and flag if not exist
  cumul <- match.arg(cumul)

  # set the labels  
  if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
  if (is.null(ylab)) if (!prop) y.lbl <- "Frequency" else y.lbl <- "Proportion"
    else y.lbl <- ylab
  if (is.null(main)) main.lbl <- "" else main.lbl <- main
    
  # get breaks from user supplied bin width and/or supplied start value
  if (!is.null(bin.width)  || !is.null(bin.start)) {
    if (is.null(bin.start)) 
      bin.start <- pretty(min(x, na.rm = TRUE):max(x, na.rm = TRUE))[1]
    if (is.null(bin.width)) {
      h <- hist(x, plot=FALSE, breaks="Sturges")
      bin.width <- h$breaks[2]-h$breaks[1]
    }
    max.x <- max(x, na.rm = TRUE)
    seq.end <- max.x
    breaks <- seq(bin.start,seq.end,bin.width)
    while (max(breaks) < max.x) {
      seq.end <- seq.end + bin.width
      breaks <- seq(bin.start,seq.end,bin.width)
    }
  }
  
  # for user supplied bins, from seq function or bin.start, 
  #  make sure entire data range is spanned
  if (is.numeric(breaks)) {
    cc <- cut(x, breaks, ...)   # replace each data value with its bin
    labs <- levels(cc)  # get list of unique bins, ordered
    bins <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
    bin.min <- min(bins)
    bin.max <- max(bins)
    n.under <- length(x[which(x<bin.min)])
    n.over <- length(x[which(x>bin.max)])
    if (n.under+n.over > 0) {
      cat("\nRange of the data: ", min(x), " to ", max(x), "\n", sep="")
      if (length(breaks) > 3)
        cat("Specified bin cutpoints: ", bin.min, breaks[2], "...", 
          breaks[length(breaks)-1], bin.max, "\n\n")
    else
      cat("Range of the specified bins: ", bin.min, " to ", bin.max, "\n", sep="")
      if (n.under > 0) 
        cat("Data values too small to fit in the bins: ", x[which(x<bin.min)], "\n")
      if (n.over > 0)
        cat("Data values too large to fit in the bins: ", x[which(x>bin.max)], "\n")
      cat("\n")
      cat("Each data value must be in a bin.\n")
      txt <- "To fix this problem, extend the bin range "
      if (n.under > 0) cat(txt, "below ", bin.min, ".\n", sep="")
      if (n.over > 0) cat(txt, "above ", bin.max, ".\n", sep="")
      stop("Extend the bin range and rerun.\n\n", call. = FALSE)
    }  
  }

  # calculate but do not plot the histogram
  # stop warnings about any unused parameters due to not plotting 
  h <- suppressWarnings(hist(x, plot=FALSE, breaks, ...))

  
  # summarize data
  cat("\n")
  dash(49)
  cat("Data Summary:", x.lbl, "\n")
  dash(49)
  cat("\n")
  cat("Present:", sum(!is.na(x)), "\n")
  cat("Missing:", sum(is.na(x)), "\n")  # hist automatically deals with missing data
  cat("Total  :", length(x), "\n")
  cat("\n")
  cat("Mean   :", format(signif(mean(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("Std Dev:", format(signif(sd(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("\n")
  cat("Minimum      :", format(signif(min(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("1st Quartile :", format(signif(quantile(x, na.rm=TRUE)[2],digits.d), scientific=FALSE), "\n")
  cat("Median       :", format(signif(median(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("3rd Quartile :", format(signif(quantile(x, na.rm=TRUE)[4],digits.d), scientific=FALSE), "\n")
  cat("Maximum      :", format(signif(max(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")
  cat("\n")
  cat("IQR          :", format(signif(IQR(x, na.rm=TRUE),digits.d), scientific=FALSE), "\n")

  cat("\nNumber of Bins:", length(h$breaks)-1, "\n")
  
  # relative frequency histogram option
  if (prop) h$counts <- h$counts/length(x)
    
  # cumulative histogram option
  if (cumul != "off") {
    old.counts <- h$counts
    h$counts <- cumsum(h$counts)
  }
  
  # set up plot area
  plot(h, border="transparent", xlab=x.lbl, ylab=y.lbl, main=main.lbl, font.main=1, 
    freq=TRUE, ...)
  
  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  
  # grid lines computation
  vy <- pretty(h$counts)

  # plot the histogram and grid lines
  if (!over.grid) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  plot(h, add=TRUE, col=col, border=border, labels=show.values, freq=TRUE, ...)
  if (cumul == "both") {
    h$counts <- old.counts
     plot(h, add=TRUE, col=col.reg, freq=TRUE)
  }
  if (over.grid) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)

  # frequency table
  # j<17 condition is to stop the 0.99999... problem
  cum.c <- cumsum(h$counts)
  prop <- round(h$counts/length(x),2)
  cum.p <- cumsum(prop)
  max.dg <- 0
  for (i in 1:length(h$breaks)) {
    j <- nchar(as.character(h$breaks[i]))
    if (j>max.dg && j<17) max.dg <- j
  }
  max.dg.mid <- 0
  for (i in 1:length(h$mids)) {
    j <- nchar(as.character(h$mids[i]))
    if (j>max.dg.mid && j<19) max.dg.mid <- j
  }
  x.breaks <- format(h$breaks, width=max.dg, justify="right", scientific=FALSE)
  x.mids <- format(h$mids, width=max.dg.mid+3, justify="right", scientific=FALSE)
  
  cat("\n")
  buf1 <- format("", width=max.dg+1)
  strt <- 9 + 2*max.dg
  my.width <- strt-(nchar(buf1)+nchar("Bin"))-7
  buf2 <- format(" ", width=my.width)
  dash(66)
  cat(buf1,"Bin",buf2,"  Midpoint","  Count","  Prop","  Cumul.c"," Cumul.p",sep="","\n")
  dash(66)
  for (i in 2:length(h$breaks)-1)
    cat(sprintf("%s > %s %s %6.0f %6.2f %6.0f %6.2f", x.breaks[i], 
      x.breaks[i+1], x.mids[i], h$counts[i], prop[i], cum.c[i], cum.p[i]), "\n") 
  dash(66)
  cat("\n")
  
}
