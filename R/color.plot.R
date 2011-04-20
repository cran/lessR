color.plot <-
function(x, y=NULL, type=NULL, col.line="darkblue", col.area=NULL,  
           col.point="darkblue", col.fill=NULL, col.grid="grey90", 
           col.bg="ghostwhite", col.box="black", xy.ticks=TRUE, 
           xlab=NULL, ylab=NULL, pch=NULL, cex=NULL, center.line=NULL,
           kind=c("default", "regular", "bubble.freq", "sunflower.freq"),
           x.start=NULL, x.end=NULL, size=.25, 
           fit.line=c("none", "lowess", "ls"), col.fit.line="grey55", 
           col.bubble="lightsteelblue", col.flower="steelblue",
           time.start=NULL, time.by=NULL, time.reverse=FALSE, ...) {
           
max.dd <- function(x) {

  n.dec <-function(x) {
    xc <- as.character(x)
    nchar(xc)
    ipos <- 0
    for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
    if (ipos > 0) n.dec <- nchar(xc)-ipos else n.dec <- 0
    return(n.dec)
  }
    
  max.dd <- 0
  for (i in 1:length(x))
    if (!is.na(x[i])) if (n.dec(x[i]) > max.dd ) max.dd <- n.dec(x[i])   
  return(max.dd)
}

dashes <- function(ndash) { for (i in 1:(ndash)) cat("-"); cat("\n") }

# -------------------------
# Preliminaries
# -------------------------

# produce actual argument, such as from an abbreviation, and flag if not exist
fit.line <- match.arg(fit.line)
kind <- match.arg(kind)

if ( (kind == "bubble.freq") || (kind == "sunflower.freq") ) {
  if (is.null(y))  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Option 'bubble.freq' or 'sunflower.freq' are only used in scatterplots.\n\n")
  }
  if ( (!is.integer(x)) || !is.integer(y)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Option 'bubble.freq' or 'sunflower.freq' can only be used with integer data.\n\n")
  }
}

if (!is.null(type)) if (type != "p" && type != "l" && type != "b") { 
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option 'type' can only be \"p\" for points,\n",
      "  \"l\" for line or \"b\" for both.\n\n")
    }

if (!is.null(center.line))
  if (center.line != "mean" && center.line != "median"&& center.line != "off") { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Option 'center.line' can only be \"mean\", \"median\", or \"off\".\n\n")
    }
    
if (!is.null(y)) {
  txt <- "ignored when two variables are specified.\n"
  if (!is.null(time.start)) cat("Warning: Option 'time.start'", txt)
  if (!is.null(time.by)) cat("Warning: Option 'time.by'", txt)
  if ((time.reverse)) cat("Warning: Option 'time.reverse'", txt)
}

if (!is.null(time.start) && is.null(time.by)) {
   cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specified  time.start  so also need  time.by.\n\n")
  
}
    
#  pch=21 is a filled circle
if (is.null(pch)) point.type <- 21 else point.type <- pch

nrows <- length(x)
if (is.null(cex)) pt.size <- 1 else pt.size <- cex

# -------------------------
# y only, so create x index
# -------------------------
  if (is.null(y)) {
  kind <- "regular"  # kind only applicable when x and y both specified

  if (is.null(ylab)) y.lbl <- deparse(substitute(x)) else y.lbl <- ylab
  
  if (!time.reverse) y <- x
  else for (i in 1:nrows) y[i] <- x[nrows+1-i]
    
  if (is.null(time.start)) {
    if (is.null(xlab)) x.lbl <- "Index" else x.lbl <- xlab
    x <- 1:length(y)  # ordinal position of each value on x-axis
  }
  else   {  # time.start date specified
    if (is.null(xlab)) x.lbl <- "" else x.lbl <- xlab      
    date.seq <- seq(as.Date(time.start), by=time.by, length.out=nrows)
    x <- date.seq  # dates on x-axis
  }
  
  if (is.null(center.line)) {  # by default display center.line only if many runs
    m <- mean(y)
    n.change <- 0
    for (i in 1:(length(y)-1)) if ((y[i+1]>m) != (y[i]>m)) n.change <- n.change+1 
    if (n.change/(length(y)-1) < .15) center.line <- "off"
  }
  
  if (!is.null(time.start) && is.null(col.area)) col.area <- "slategray3"  # fill ts chart

  if (is.null(type)) 
    if (is.null(col.area) || col.area == "transparent") type <- "b" else type <- "l"
    
  if (type == "b" && is.null(cex))  # set point size      
    if (nrows < 50) pt.size <- 1.0 else pt.size <- 1 - 0.002*nrows
  if (is.null(col.fill)) col.fill <- "plum"
  if (is.null(center.line)) center.line <- "median"  # default
  
}

# ----------------------------------------------------------
# x and y specified
# ----------------------------------------------------------
else {

  if (is.null(col.fill)) col.fill <- "transparent"
  if (is.null(col.area)) col.area <- "transparent"
  
  if (!is.factor(x)) {
    if (is.null(type)) {  # if x is sorted with equal intervals, plot a line chart
      if ( sum(diff(diff(x))) == 0 ) equal.int <- TRUE else equal.int <- FALSE
      if (!is.unsorted(x) && equal.int) type <- "l" else type <- "p"
    }
  if (kind == "default")  # set default
    if ( length(unique(x))<10 && length(unique(y))<10 ) kind <- "bubble.freq"
    else kind <- "regular"
  }
  else {  # x is a factor
    type <- "p"
    kind <- "xcat"
  }

  if ((kind == "bubble.freq") || (kind == "sunflower.freq")) {  
    if (is.null(x.start)) x.start=min(x)  # x.start and x.min for Likert bubble plot
    if (is.null(x.end)) x.end=max(x)
  }

  if (xy.ticks) {  # assign axes labels with variable names as default
    if (is.null(xlab)) x.lbl <- deparse(substitute(x)) else x.lbl <- xlab
    if (is.null(ylab)) y.lbl <- deparse(substitute(y)) else y.lbl <- ylab
  }
  else {
    x.lbl <- ""
    y.lbl <- ""
  }
  
}
digits.d <- max.dd(y) + 1


# -------------------------
# Plot
# -------------------------
# plot setup
if (kind == "regular") { 
  plot(x, y, type="n", axes=FALSE, xlab=x.lbl, ylab=y.lbl, ...)
  if (xy.ticks){
    if (is.null(time.start)) axis(1, ...)
      else axis.Date(1, at=seq(min(date.seq), max(date.seq), length.out=20), ...)
    axis(2, ...) 
  }
}
else if (kind == "xcat") {
  plot.default(y ~ x, xlim=c(.5,nlevels(x)+.5), type="n", axes=FALSE, xlab=x.lbl, ylab=y.lbl)
  axis(2)
  axis(1,labels=levels(x), at=1:nlevels(x))
}
else { # bubble or sunflower plot, requires integer values of x and y
  mytbl <- table(x, y)  # get the counts
  x.lo <- x.start-.5
  x.hi <- x.end+.5

  # melt the table to a dataframe
  k <- 0
  xx <- integer(length=0)
  yy <- integer(length=0)
  count <- integer(length=0)
  for (i in 1:nrow(mytbl)) {
    for (j in 1:ncol(mytbl)) {
      k <- k + 1
      count[k] <- mytbl[i,j]
      xx[k] <- as.integer(rownames(mytbl)[i])
      yy[k] <- as.integer(colnames(mytbl)[j])
    }
  }
  cords <- data.frame(xx, yy, count)

  if (kind == "bubble.freq")   # bubble plot
    plot(x,y, type="n", xlab=x.lbl, ylab=y.lbl, xlim=c(x.lo,x.hi), ylim=c(x.lo,x.hi))
}

# colored plotting area
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)

# grid lines
vx <- pretty(c(usr[1],usr[2]))
vy <- pretty(c(usr[3],usr[4]))
abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)

# fill area under curve
if (type != "p") col.border <- col.line else col.border <- "transparent"
  if (!is.null(col.area)) 
    polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)), col=col.area, 
      border=col.border)

# the plot
if (kind == "regular") {  # plot lines and/or points
  if (type == "l" | type == "b") {
    lines(as.numeric(x),y, col=col.line, ...)
  }
  if (type == "p" | type == "b") {
    points(x,y, col=col.point, pch=point.type, bg=col.fill, cex=pt.size, ...)
  }
}
else if (kind == "xcat") {
  for (i in (1:nlevels(x))) {
    points(rep(i,length(y[x==levels(x)[i]])), y[x==levels(x)[i]], col="darkblue")
    points(i, mean(y[x==levels(x)[i]]), pch=23, bg="steelblue")
  }
}
else if (kind == "bubble.freq") {
  symbols(cords$xx, cords$yy, circles=cords$count, bg=col.bubble, inches=size, add=TRUE, ...)
  zeros <- cords[cords$count==0, ] # 0 plots to a single pixel, so remove
  points(zeros$xx, zeros$yy, col=col.bg, bg=col.bg, pch=21, cex=.5)
}
else if (kind == "sunflower.freq") {
  sunflowerplot(cords$xx, cords$yy, number=cords$count, 
    seg.col=col.flower, col=col.flower,
    xlab=x.lbl, ylab=y.lbl, xlim=c(x.lo,x.hi), ylim=c(x.lo,x.hi))
}

if (fit.line != "none") {  # fit line option
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    if (fit.line == "lowess") lines(lowess(x[ok], y[ok]), col=col.fit.line)
    if (fit.line == "ls") {
      model <- lm(y[ok] ~ x[ok])
      abline(model$coef, col=col.fit.line)
    }
}


# plot center line, analyze runs
if (!is.null(center.line) && center.line != "off") {
  if (center.line == "mean") {
    m.y <- mean(y)
    lbl <- "mean"
    lbl.cat <- "Mean:"
  }
  else if (center.line == "median") {
    m.y <- median(y)
    lbl <- "medn"
    lbl.cat <- "Median:"
  }
  abline(h=m.y, col="gray50", lty="dashed")
  mtext(lbl, side=4, cex=.9, col="gray50", las=2, at=m.y, line=0.1)
  dashes(20); cat(lbl.cat, round(m.y,digits.d), "\n"); dashes(20)
      
  dashes(12); cat("Run Analysis\n"); for (i in 1:12) cat("-")
  run <- integer(length=200)  # length of ith run in run[i]
  n.runs <- 1  # total number of runs
  run[n.runs] <- 1
  line.out <- "    1"
  cat("\n")
  for (i in 2:length(y)) {
    if (y[i] != m.y) {  # throw out values that equal m.y
      if (sign(y[i]-m.y) != sign(y[i-1]-m.y)) {  # new run
        if (n.runs < 10) buf <- "  " else buf <- " "
        cat("size=", run[n.runs], "  Run", buf, n.runs, ":", line.out, "\n", sep="")
        line.out <- ""
        n.runs <- n.runs + 1
        run[n.runs] <- 0
      }
      run[n.runs] <- run[n.runs] + 1
      if (i < 10) buf <- "  " else buf <- " "
      line.out <- paste(line.out, buf, i)
    }
  }
  cat("size=", run[n.runs], "  Run", buf, n.runs, ":", line.out, "\n", sep="")
  eq.ctr <- which(y==m.y)
  cat("\nTotal number of runs:", n.runs, "\n")
  txt <- "Total number of values that do not equal the "
  cat(txt, lbl.cat, " ", length(y)-length(eq.ctr), "\n", sep="")
  if (length(eq.ctr) != 0) {
    cat("\nValues ignored that equal the", lbl.cat, eq.ctr, "\n")
    cat("Total number of values ignored:", length(eq.ctr), "\n")
  }
  else 
    cat("Total number of values ignored that equal the", lbl.cat, length(eq.ctr), "\n") 
}

# -------------------------
# correlation analysis
# -------------------------
if (!is.null(y)  &&  !is.factor(x)  &&  type == "p") {
  ct <- cor.test(x,y) 
  cat("\n")
  dashes(55)
  cat("Correlation Analysis for Variables", ct$data.name, "\n")
  dashes(55);  cat("\n")
  cat("Sample Estimate: r =", round(ct$estimate,3), "\n\n")
  cat("Hypothesis Test that Population Correlation is Zero", "\n")
  cat("  t-value: ", round(ct$statistic,4), ",  df: ", ct$parameter, sep="") 
  cat(",  p-value: ", round(ct$p.value,4), sep="", "\n\n")
  cat("95% Confidence Interval of Population Correlation", "\n")
  cat("  Lower Bound:", round(ct$conf.int,3)[1])
  cat("     Upper Bound:", round(ct$conf.int,3)[2], "\n")
  dashes(55)
}

  cat("\n")

}
