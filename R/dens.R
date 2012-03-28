dens <-
function(x, dframe=mydata, 
         bw="nrd0", type=c("both", "general", "normal"),
         col.bg="ghostwhite", col.grid="grey90", col.bars="grey86",
         col.nrm="black", col.gen="black",
         col.fill.nrm=rgb(80,150,200, alpha=70, maxColorValue=255), 
         col.fill.gen=rgb(250,210,230, alpha=70, maxColorValue=255),
         bin.start=NULL, bin.width=NULL, text.out=TRUE,
         x.pt=NULL, xlab=NULL, main=NULL, y.axis=FALSE, 
         x.min=NULL, x.max=NULL, ...) {


dens.main <- 
function(x, ...) {

  if (!is.null(x.pt)) {
   y.axis=TRUE
   type <- "general"
  }

  # set the labels
  if (is.null(xlab)) x.lbl <- x.name else x.lbl <- xlab
  # use variable label for main if it exists and main not specified
  if (is.null(x.pt)) {
    if (!is.null(main)) main.lbl <- main
    else {
      main.lbl <- ""
      if (exists("mylabels")) {
        lbl <- mylabels[which(row.names(mylabels)==x.name), "label"]
        if (length(lbl) > 0) main.lbl <- lbl
      }
    }
  }
  else main.lbl <- ""
      
  # get breaks from user supplied bin width and/or supplied start value
  # otherwise, breaks="Sturges by default
  # copied from histogram
  if (!is.null(bin.width)  || !is.null(bin.start)) {
    if (is.null(bin.start)) bin.start <- pretty(min(x):max(x))[1]
    if (is.null(bin.width)) {
      h <- hist(x, plot=FALSE, breaks="Sturges")
      bin.width <- h$breaks[2]-h$breaks[1]
    }
    seq.end <- max(x)
    breaks <- seq(bin.start,seq.end,bin.width)
    while (max(breaks) < max(x)) {
      seq.end <- seq.end + bin.width
      breaks <- seq(bin.start,seq.end,bin.width)
    }
  }
  else breaks="Sturges"

  # histogram calculations, no plot
  h <- hist(x, plot=FALSE, breaks)

  n <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  if (n.miss > 0) x <- na.omit(x)
 
  # general density curve, no plot
  # suppress warnings about possible graphic parameters
  d.gen <- suppressWarnings(density(x, bw, ...))
  
  mx <- mean(x)

  # min and max x coordinates for graph, make symmetric
  min.dev.x <- min(d.gen$x) - mx
  max.dev.x <- max(d.gen$x) - mx
  if (abs(min.dev.x) > abs(max.dev.x)) {
    if (is.null(x.min)) x.min <- min(d.gen$x)
    if (is.null(x.max)) x.max <- mx + abs(min.dev.x)
  }
  if (abs(max.dev.x) > abs(min.dev.x)) {
    if (is.null(x.min)) x.min <- mx - abs(max.dev.x)
    if (is.null(x.max)) x.max <- max(d.gen$x)
  }
  
  # normal density curve, no plot
  xx <- seq(x.min, x.max, length=200)
  d.nrm <- dnorm(xx,mean(x),sd(x))

  # max y coordinate for graph
  max.y <- max(max(d.nrm), max(d.gen$y), max(h$density))
  
  # set up plot area
  # bw if specified also gets passed to plot, so suppress warning
  if (!y.axis) y.lab="" else y.lab="Density"
  suppressWarnings(plot(h, border="transparent", freq=FALSE, xlab=x.lbl,
     ylab=y.lab, main=main.lbl, xlim=c(x.min,x.max), ylim=c(0,max.y), 
     axes=FALSE, ...))
  op <- options()  # save current options to reset later
  options(scipen=30) # turn off scientific notation
  suppressWarnings(axis(1, ...))
  if (y.axis) suppressWarnings(axis(2, ...))
  options(op)
  
  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  
  # grid lines computation and print
  vy <- pretty(h$density)
  abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)

  # plot the histogram
  plot(h, add=TRUE, freq=FALSE, col=col.bars, border=col.bars)

  # plot the normal curve
  if (type == "normal" || type == "both") 
    polygon(c(x.min,xx,x.max), c(0,d.nrm,0), col=col.fill.nrm, border=col.nrm)
  
  # plot the general curve
  if (type == "general" || type == "both")
    polygon(d.gen, col=col.fill.gen, border=col.gen)
  
  # plot the optional bar about a chosen point for general curve only
  if (!is.null(x.pt)  &&  type == "general") {
    d <- d.gen
    y.pt <- d$y[which.min(abs(x.pt-d$x))]
    xbeg <- x.pt - 0.5
    xend <- x.pt + 0.5
    xsub <- d$x[d$x > xbeg & d$x < xend]
    ysub <- d$y[d$x > xbeg & d$x < xend]
    txt.ypt <- toString(round(y.pt, 3))
    txt <- paste("Density =", txt.ypt, "at x =", toString(round(x.pt, 2)))
    title(main=txt)
    polygon(c(xbeg, xsub, xend), c(0, ysub, 0), col="lightsteelblue")
    if (min(x) > 0) left <- -2*min(x) else left <- 2*min(x) 
    lines(c(left,x.pt), c(y.pt,y.pt), col="darkblue", lwd = 0.5)
    lines(c(x.pt,x.pt), c(0,y.pt), col="darkblue", lwd = 0.5)
  }

  # text output
  if (text.out) {

    cat("\nDensity bandwidth for general curve: ", round(d.gen$bw,4), sep="", "\n")
    cat("For a smoother curve, increase bandwidth with option: bw\n")

    cat("\nSample Size: ", n, "\n")
    cat("Missing Values: ", n.miss, "\n")

    digits.d <- 3
    if (n > 2 && n < 5000) {
      nrm <- shapiro.test(x)
      W <- round(nrm$statistic,min(4,digits.d+1))
      p.val <- round(nrm$p.value,min(4,digits.d+1))
      cat("\nNull hypothesis is a normal population\n")
      cat(nrm$method, ":  W = ", W, ",  p-value = ", p.val, sep="", "\n")
    }
    else
      cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
  }

  cat("\n")

}  #  end dens.main


#-------------------------------------------------------------------------

# get actual variable name before potential call of dframe$x
x.name <<- deparse(substitute(x)) 

# see if the variable exists in the Global Environment
if (exists(x.name, where=1)) in.global <- TRUE else in.global <- FALSE

# see if the variable exists from a function call
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

# see if variable exists in the data frame, if x not in Global Env or function call 
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
  dens.main(x, ...)
else
  dens.main(eval(substitute(dframe$x)), ...)

rm(x.name, envir=.GlobalEnv)

}
