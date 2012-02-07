plt <-
function(x, y=NULL, dframe=mydata, type=NULL,
         col.line="darkblue", col.area=NULL,  
         col.point="darkblue", col.fill=NULL, col.grid="grey90", 
         col.bg="ghostwhite", col.box="black", xy.ticks=TRUE, 
         xlab=NULL, ylab=NULL, main=NULL,
         pch=NULL, cex=NULL, center.line=NULL,
         kind=c("default", "regular", "bubble.freq", "sunflower.freq"),
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         fit.line=c("none", "loess", "ls"), col.fit.line="grey55", 
         col.bubble="lightsteelblue", bubble.size=.25, col.flower="steelblue",
         time.start=NULL, time.by=NULL, time.reverse=FALSE,
         ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE, 
         text.out=TRUE, ...) {


plt.main <- 
function(x, y=NULL, ...) {


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

if (is.null(y)) if (is.factor(x)) { 
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "For a single variable plot, the variable must be numeric.\n\n")
}

if (!is.null(y)) if (is.factor(y)) { 
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The first variable can be an R factor (categorical),\n",
      "but the second variable must be numeric.\n\n")
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

if (ellipse) {
  check.car <- suppressWarnings(require(car, quietly=TRUE))
  if (!check.car) {
    cat(">>> Placing an ellipse around the scatterplot requires package car.", "\n")
    cat(">>> The ellipse is not provided here, but all other output unaffected.", "\n")
    cat(">>> To get the car package, run one time only: install.packages('car')", "\n")
  }
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

  if (is.null(ylab)) y.lab <- x.name else y.lab <- ylab
  # use variable label for main if it exists and main not specified
  if (!is.null(main)) main.lbl <- main
  else {
    main.lbl <- ""
    if (exists("mylabels")) {
      lbl <- mylabels[which(row.names(mylabels)==x.name), "label"]
      if (length(lbl) > 0) main.lbl <- lbl
    }
  }
  
  if (!time.reverse) y <- x
  else for (i in 1:nrows) y[i] <- x[nrows+1-i]
  if (is.null(time.start) && class(x) != "ts") {
    if (is.null(xlab)) x.lab <- "Index" else x.lab <- xlab
    x <- 1:length(y)  # ordinal position of each value on x-axis
  }
  else   {  # time.start date specified
    if (is.null(xlab)) x.lab <- "" else x.lab <- xlab      
    if (class(x) == "ts") {
      time.start <- paste(start(x)[1], "/", start(x)[2], "/01", sep="") 
      frq <- frequency(x)
      if (frq == 1) time.by <- "year"
      if (frq == 12) time.by <- "month"
      if (frq == 52) time.by <- "week"
      if (frq == 365) time.by <- "year"
    }
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

  if (!is.null(main)) main.lbl <- main else main.lbl <- ""

  # use variable label if it exists
  x.lbl <- ""
  y.lbl <- ""
  if (exists("mylabels")) {
    x.lbl <- as.character(mylabels[which(row.names(mylabels)==x.name), "label"])
    if (length(x.lbl) == 0) x.lbl <- "" 
    y.lbl <- as.character(mylabels[which(row.names(mylabels)==y.name), "label"])
    if (length(y.lbl) == 0) y.lbl <- ""
  }

  # axis and legend labels
  if (!is.null(xlab)) x.lab <- xlab else x.lab <- x.name
  if (!is.null(ylab)) y.lab <- ylab else y.lab <- y.name

  # title
  if (!is.null(main)) main.lbl <- main 
  else {
    main.lbl <- x.lbl
    if (exists("y.lbl"))
      if (nchar(y.lbl) > 0) main.lbl <- paste(x.lbl, "\nBY\n", y.lbl, sep="")
  }

  if (!is.factor(x)) {
    if (is.null(type)) {  # if x is sorted with equal intervals, plot a line chart
      if (sum(is.na(x)) > 0) equal.int <- FALSE  # missing data in x present
      else {
        diff.x <- diff(x)
        for (i in 2:(length(x)-1)) 
          if (diff.x[i-1] != diff.x[i]) equal.int <- FALSE else equal.int <- TRUE
        rm(diff.x)
      }
      if (!is.unsorted(x) && equal.int  && sum(is.na(y))==0)  # also no y missing 
        type <- "l" else type <- "p"
    }
  if (kind == "default")  # set default
    if ( length(x)>10 && length(y)>10 && length(unique(x))<10 && length(unique(y))<10 )
      kind <- "bubble.freq"
    else kind <- "regular"
  }
  else {  # x is a factor
    type <- "p"
    kind <- "xcat"
  }

  if ((kind == "bubble.freq") || (kind == "sunflower.freq")) {  
    if (is.null(x.start)) x.start=min(x, na.rm=TRUE)  # x.start and x.min for Likert bubble plot
    if (is.null(x.end)) x.end=max(x, na.rm=TRUE)
    if (is.null(y.start)) y.start=min(y, na.rm=TRUE) 
    if (is.null(y.end)) y.end=max(y, na.rm=TRUE)
  }
  
}
digits.d <- max.dd(y) + 1


# -------------------------
# plot
# -------------------------
# plot setup
if (kind == "regular") {
  suppressWarnings(plot(x, y, type="n", axes=FALSE, xlab=x.lab, ylab=y.lab, 
                        main=main.lbl, ...))
  if (xy.ticks){
    if (is.null(time.start) && class(x)!="ts") suppressWarnings(axis(1, ...))
      else axis.Date(1, x, ...)
    suppressWarnings(axis(2, ...))
  }
}
else if (kind == "xcat") {
  plot.default(y ~ x, xlim=c(.5,nlevels(x)+.5), type="n", axes=FALSE, 
               main=main.lbl, xlab=x.lab, ylab=y.lab)
  axis(2)
  axis(1,labels=levels(x), at=1:nlevels(x))
}
else { # bubble or sunflower plot, requires integer values of x and y
  mytbl <- table(x, y)  # get the counts
  x.lo <- x.start-.5
  x.hi <- x.end+.5
  y.lo <- y.start-.5
  y.hi <- y.end+.5

  # melt the table to a data frame
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
  if (kind == "bubble.freq")   # bubble plot, convert to factor if x is numeric
    plot(as.factor(x),y, type="n", xlab=x.lab, ylab=y.lab, main=main.lbl,
         xlim=c(x.lo,x.hi), ylim=c(y.lo,y.hi))
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
    polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)), col=col.area, border=col.border)

# the plot
if (kind == "regular") {  # plot lines and/or points
  if (type == "l" | type == "b") {
    lines(as.numeric(x),y, col=col.line, ...)
  }
  if (type == "p" | type == "b") {
    suppressWarnings(points(x,y, col=col.point, pch=point.type, bg=col.fill, cex=pt.size, ...))
    if (ellipse && check.car) {
      dataEllipse(x, y, col=col.ellipse, levels=.95, lwd=1.5, fill=fill.ellipse, 
        fill.alpha=.06, center.cex=0, segments=100, plot.points=FALSE)
    }
  }
}
else if (kind == "xcat") {
  for (i in (1:nlevels(x))) {
    abline(h=mean(y[x==levels(x)[i]]), col="gray80")
    points(rep(i,length(y[x==levels(x)[i]])), y[x==levels(x)[i]], col="darkblue")
    points(i, mean(y[x==levels(x)[i]]), pch=23, bg="steelblue")
  }
}
else if (kind == "bubble.freq") {
  symbols(cords$xx, cords$yy, circles=cords$count, bg=col.bubble, 
          inches=bubble.size, add=TRUE, ...)
  zeros <- cords[cords$count==0, ] # 0 plots to a single pixel, so remove
  points(zeros$xx, zeros$yy, col=col.bg, bg=col.bg, pch=21, cex=.5)
  if (ellipse && check.car) {
    dataEllipse(x, y, col=col.ellipse, levels=.95, lwd=1.5, fill=fill.ellipse, 
      fill.alpha=.06, center.cex=0, segments=100, plot.points=FALSE)
  }
}
else if (kind == "sunflower.freq") {
  sunflowerplot(cords$xx, cords$yy, number=cords$count, 
    seg.col=col.flower, col=col.flower,
    xlab=x.lab, ylab=y.lab, xlim=c(x.lo,x.hi), ylim=c(x.lo,x.hi))
}

# fit line option
if (fit.line != "none") {  
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    x.ok <- x[ok]
    y.ok <- y[ok]
    ord <- order(x.ok)
    x.ord <- x.ok[ord]
    y.ord <- y.ok[ord] 
    if (fit.line == "loess") {
      lines(x.ord, fitted(loess(y.ord~x.ord, ...)), col=col.fit.line)
    }
    if (fit.line == "ls") {
      if(!is.factor(x)) {
        model <- lm(y.ord ~ x.ord)
        abline(model$coef, col=col.fit.line)
      }
      else cat("\nLeast squares line not permitted for a factor.\n")
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
  
  if (text.out) {
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
}

# -------------------------
# correlation analysis
# -------------------------
if (text.out && !is.null(y)  &&  !is.factor(x)  &&  type == "p") {
  n.pair <- sum(!is.na(x - y))  # number of points after pairwise deletion
  n.del <- sum(is.na(x - y))  # number of pairwise deleted observations
  ct <- cor.test(x,y)
  cat("\n")
  dashes(55)
  cat("Correlation Analysis for Variables", x.name, "and", y.name, "\n")
  dashes(55);  cat("\n")
  if ( (nchar(x.lbl) > 0) || (nchar(y.lbl) > 0) ) {
    cat(x.name, ", ", as.character(y.lbl), sep="", "\n")
    cat(y.name, ", ", as.character(x.lbl), sep="", "\n")
    cat("\n")
  }
  cat("Number of paired values with neither missing:  n =", n.pair, "\n")
  cat("Number of observations (rows of data) deleted: n =", n.del, "\n\n")
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

}  # end plt.main



#-------------------------------------------------------------------------

# get actual variable name before potential call of dframe$x
x.name <<- deparse(substitute(x)) 
if (!missing(y)) y.name <<- deparse(substitute(y)) 

# evaluate x
#-----------

# see if x exists in the Global Environment
if (exists(x.name, where=.GlobalEnv)) in.global <- TRUE else in.global <- FALSE

# see if x exists from a function call
# indicate a function call with sys.frame returns larger than 1 
if (exists(x.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
  in.call <- TRUE else in.call <- FALSE

# see if the data frame exists, if x not in Global Env or function call
dframe.name <- deparse(substitute(dframe))
if (!in.global && !in.call) {
  if (!exists(dframe.name)) {
    if (dframe.name == "mydata") 
      txtA <- ", the default data frame name, " else txtA <- " "
    txtB1 <- "So either create data frame by reading with the rad function, or\n"
    txtB2 <- "  specify the actual data frame with the parameter: dframe\n"
    txtB <- paste(txtB1, txtB2, sep="")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Data frame ", dframe.name, txtA, "does not exist\n\n", txtB, "\n")
 }
}

# see if x exists in the data frame, if x not in Global Env or function call 
if (!missing(x) && !in.global && !in.call) {
  if (!exists(x.name, where=dframe)) { 
    if (dframe.name == "mydata") {
      txt1 <- ", the default name \n\n"
      txt2 <- "So either make sure you are using the correct variable name, or\n"
      txt3 <- "  specify the actual data frame with the parameter: dframe\n"
      txt <- paste(txt1, txt2, txt3, sep="")
    }
    else txt <- " "
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Variable ", x.name, " does not exist either by itself ",
        "or in the data frame ", dframe.name, txt, "\n\n")
  }
}
if (in.global || in.call)
   x.call <- x else x.call <- eval(substitute(dframe$x))

# evaluate y
#-----------
if (!missing(y)) {

  # see if y exists in the Global Environment
  if (exists(y.name, where=.GlobalEnv)) in.global <- TRUE else in.global <- FALSE

  # see if y exists from a function call
  # indicate a function call with sys.frame returns larger than 1 
  if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
    in.call <- TRUE else in.call <- FALSE
  if (!in.global && !in.call) {
  # see if y exists in the data frame, if y not in Global Env or function call 
    if (!exists(y.name, where=dframe)) { 
      if (dframe.name == "mydata") {
        txt1 <- ", the default name \n\n"
        txt2 <- "So either make sure you are using the correct variable name, or\n"
        txt3 <- "  specify the actual data frame with the parameter: dframe\n"
        txt <- paste(txt1, txt2, txt3, sep="")
      }
      else txt <- " "
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Variable ", y.name, " does not exist either by itself ",
          "or in the data frame ", dframe.name, txt, "\n\n")
    }
  }
        
  if (in.global || in.call)
     y.call <- y else y.call <- eval(substitute(dframe$y))
}

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


if (missing(y)) plt.main(x.call, ...)
else plt.main(x.call, y.call, ...)

rm(x.name, envir=.GlobalEnv)
if (!missing(y)) rm(y.name, envir=.GlobalEnv)

}
