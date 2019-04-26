# Plot quantiles, tails of normal and t for specified alpha
prob_tcut <- 
function(df, alpha=0.05, digits_d=3, y_axis=FALSE,
         fill="aliceblue", color_tail="palevioletred4",
         nrm_color=gray(.7), color_t=gray(.08),
         pdf_file=NULL, width=5, height=5, ...) {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("digits.d", "y.axis", "color.tail", "nrm.color",
                "color.t", "pdf.file")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (substr(names(dots)[i], 1, 4) == "col.") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "options that began with the abbreviation  col  now begin with  ",
          "color \n\n")
      }
    }
  }

  if (df < 2) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "The parameter  df  must be 2 or larger.\n\n")
  }

  # set up plot 
  .opendev(pdf_file, width, height)

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))
  par(mar=c(4, 3, 1, 2), mgp=c(2,.6,0))

  xmin <- -5.5
  xmax <- 5.5

  x1min <- xmin
  x2max <- xmax

  tail.area <- alpha / 2


  #-------------
  # t Dist
  #-------------
  x <- seq(xmin,xmax,length=200)
  y <- dt(x, df=df)
  plot(x,y, type="n", ylim=c(0,.42), axes=FALSE, lwd=2, las=1, main="", 
       xlab="Standard Errors from Zero", ylab="")

  axis(side=1)
  if (y_axis) {
    axis(side=2)
    ylab="Density"
    title(ylab=ylab)
  }

  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], 
       col=rgb(249,249,252,maxColorValue=255), border="black")

  # curve and fill
  x1max <- qt(tail.area, df=df)
  x2min <- qt(tail.area, df=df, lower.tail=FALSE)
  lines(x,y, type="l", lwd=4, col=color_t, las=1, main="", xlab="z", ylab="")
  polygon(c(x1max,x,x2min), c(0,y,0), col=fill, border="transparent")
  # rgb(.91, .95, .98)

  # Left Tail
  x1max <- qt(tail.area, df=df)
  x <- seq(x1min,x1max,by=.01)
  y <- dt(x,df=df)
  polygon(c(x1min,x,x1max), c(0,y,0), col=color_tail, border=nrm_color)
  lines(c(x1max,x1max), c(0,.19), col=color_tail)
  text(x1max, .20, col=color_t, cex=1.1, .fmt(x1max, digits_d, w=5))

  # Right Tail
  x2min <- qt(1-tail.area, df=df)
  qntl <- x2min
  x <- seq(x2min,x2max,by=.01)
  y <- dt(x,df=df)
  polygon(c(x2min,x,x2max), c(0,y,0), col=color_tail, border=nrm_color)
  lines(c(x2min,x2min), c(0,.19), col=color_tail)
  text(x2min, .20, col=color_t, cex=1.1, .fmt(x2min, digits_d, w=5))

  #-------------
  # Normal Dist
  #-------------
  x1max <- qnorm(tail.area)
  x2min <- qnorm(1-tail.area)
  x <- seq(xmin,xmax,length=200)
  y <- dnorm(x)
  lines(x,y, type="l", lwd=2, col=nrm_color, las=1, main="", xlab="z", ylab="")
  abline(h=0)

  # fill
  txtht <- 0.175
  rlbl <- paste(toString(round(100*(1-alpha),0)), "%", sep="")
  text(0, txtht, rlbl, cex=1.4, col=nrm_color)

  # Left Tail
  lines(c(x1max,x1max), c(0,.25), col=nrm_color)
  text(x1max, .26, col=nrm_color, cex=1.1, .fmt(x1max, digits_d, w=5))

  # Right Tail
  lines(c(x2min,x2min), c(0,.25), col=nrm_color)
  text(x2min, .26, col=nrm_color, cex=1.1, .fmt(x2min, digits_d, w=5))

  #-------------
  # Legend
  #-------------
  dflbl <- paste("t, df=",toString(df),sep="")
  legend("topright", c("Normal", dflbl), lwd=c(1,1.5),
    col=c(nrm_color,color_t), box.lwd=.5, cex=.75)


  # terminate pdf graphics system
  par(orig.params)        
  if (!is.null(pdf_file)) {
    dev.off()
    .showfile(pdf_file, "t-curve probability")
  }

  cat("Probability: ", qntl, "\n")

}

