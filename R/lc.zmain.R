.lc.main <- 
function(y, type,
       col.line, col.area, col_color, col_fill, shape_pts,
       col.box, col.bg, lab_cex, axis_cex, col.axis,
       rotate_x, rotate_y, offset, xy_ticks,
       line_width, xlab, ylab, main, sub, cex,
       time_start, time_by, time_reverse, 
       center_line, show_runs, quiet, ...) {


  if (!is.numeric(y)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The variable must be numeric.\n\n")
  }

  if (!is.null(type)) if (type != "p" && type != "l" && type != "b") { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Option 'type' can only be \"p\" for points,\n",
        "  \"l\" for line or \"b\" for both.\n\n")
  }

  if (!is.null(time_start) && is.null(time_by)) {
     cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Specified  time_start  so also need  time_by.\n\n")
  }
  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_y_cex <- getOption("lab_y_cex")
  lab_x_cex <- ifelse(is.null(lab_x_cex), lab_cex, lab_x_cex)
  adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex
  lab_y_cex <- ifelse(is.null(lab_y_cex), lab_cex, lab_y_cex)
  adj <- .RSadj(lab_cex=lab_y_cex); lab_y_cex <- adj$lab_cex

  nrows <- length(y)
  pt.size <- ifelse (is.null(cex), 0.8, cex)

  # get variable label and axis labels if they exist
  gl <- .getlabels(ylab=ylab, main=main, lab_y_cex=lab_y_cex)
  y.name <- gl$yn;  y.lbl <- gl$yl;  y.lab <- gl$yb
  main.lab <- gl$mb
  sub.lab <- gl$sb

  # count and remove missing data
  n <- sum(!is.na(y))
  n.miss <- sum(is.na(y))
  if (!is.ts(y)) y <- na.omit(y)

  x <- numeric(length = 0)
  if (time_reverse) {  # use x as a temporary place holder
    for (i in 1:nrows) x[i] <- y[nrows+1-i]
    y <- x
  }

  if (is.null(time_start) && !is.ts(y)) {
    x.lab <- ifelse (is.null(xlab), "Index", xlab)
    x <- 1:length(y)  # ordinal position of each value on x-axis
  }
  else {  # time_start date specified or a ts
    x.lab <- ifelse (is.null(xlab), "", xlab)      
    if (is.ts(y)) {
      x <- .ts.dates(y)
      # time_start <- paste(start(y)[1], "/", start(y)[2], "/01", sep="") 
      # frq <- frequency(y)
      # if (frq == 1) time_by <- "year"
      # if (frq == 4) time_by <- "3 months"
      # if (frq == 12) time_by <- "month"
      # if (frq == 52) time_by <- "week"
      # if (frq == 365) time_by <- "year"
    }
    else {
      date.seq <- seq.Date(as.Date(time_start), by=time_by, length.out=nrows)
      x <- date.seq  # dates on x-axis
    }
  }

  # by default display center_line only if runs
  if (center_line == "default"  &&  !is.ts(y)) {
    m <- mean(y, na.rm=TRUE)
    n.change <- 0
    for (i in 1:(length(y)-1)) if ((y[i+1]>m) != (y[i]>m)) n.change <- n.change+1 
    if (n.change/(length(y)-1) < .15)
      center_line <- "off" 
    else 
      center_line <- "median"
  }

  
  # fill ts chart 
  #if (!is.null(time_start) && is.null(area))
    #col.area <- getOption("bar_fill")

  if (is.null(type))
    if (is.null(col.area) || col.area == "transparent") type <- "b" 
    else type <- "l"

  # set point size       
  if (type == "b" && is.null(cex))  
    if (nrows < 50) pt.size <- 0.8 else pt.size <- .9 - 0.002*nrows
  if (pt.size < 0) pt.size <- 0
  
  digits_d <- .max.dd(y) + 1
  options(digits_d=digits_d)
  
  # set margins
  max.width <- strwidth(as.character(max(pretty(y))), units="inches")
  
  margs <- .marg(max.width, y.lab, x.lab, main,
                 rotate_x=getOption("rotate_x"),
                 lab_x_cex=lab_x_cex, lab_y_cex=lab_y_cex)
  lm <- margs$lm
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm
  n.lab_x.ln <- margs$n.lab_x.ln
  n.lab_y.ln <- margs$n.lab_y.ln
 
  if (center_line != "off") rm <- rm + .3
 
  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  par(bg=getOption("window_fill"))
  par(mai=c(bm, lm, tm, rm))

  # plot setup
  plot(x, y, type="n", axes=FALSE, ann=FALSE, ...)
          
  usr <- par("usr")

  # colored plotting area
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="transparent")

  # grid lines
  vx <- pretty(c(usr[1],usr[2]))
  abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=getOption("grid_x_color"),
         lwd=getOption("grid_lwd"), lty=getOption("grid_lty"))
  vy <- pretty(c(usr[3],usr[4]))
  abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=getOption("grid_y_color"),
         lwd=getOption("grid_lwd"), lty=getOption("grid_lty"))

  # box around plot
  rect(usr[1], usr[3], usr[2], usr[4], col="transparent", border=col.box,
    lwd=getOption("panel_lwd"), lty=getOption("panel_lty"))

  if (xy_ticks){
    axis_x_color <- ifelse(is.null(getOption("axis_x_color")), 
      getOption("axis_color"), getOption("axis_x_color"))
    if (is.null(time_start) && !is.ts(x)) 
     .axes(x.lvl=NULL, y.lvl=NULL, axTicks(1), axTicks(2),
        rotate_x=rotate_x, rotate_y=rotate_y, offset=offset, ...)
    else {
      axis.Date(1, x, cex.axis=axis_cex, col.axis=col.axis, ...)
      #lbl.dt <- as.Date(axTicks(1), origin = "1970-01-01")
      #axis.Date(1, x, labels=FALSE, tck=-.01, ...)
      #text(x=lbl.dt, y=par("usr")[3], labels=lbl.dt,
           #pos=1, xpd=TRUE, cex=axis_cex, col=col.axis)
      axis(2, at=axTicks(2), labels=FALSE, tck=-.01, ...)
      dec.d <- .getdigits(round(axTicks(2),6),1) - 1
      text(x=par("usr")[1], y=axTicks(2), labels=.fmt(axTicks(2),dec.d),
           pos=2, xpd=TRUE,  col=col.axis)
    }
  }

  # axis labels
  .axlabs(x.lab, y.lab, main.lab, sub.lab,
          x.val=NULL, xy_ticks=TRUE, offset=offset,
          lab_x_cex=lab_x_cex, lab_y_cex=lab_y_cex,
          main_cex=getOption("main_cex"),
          n.lab_x.ln, n.lab_y.ln, ...) 
  # fill area under curve
  if (!is.null(col.area)  && !is.null(col_color)) {
    if (col.area=="transparent"  &&  col_color=="transparent")
      col.area <- NULL
    else if (type != "p"  &&  is.null(col_color))
      col_color <- col.line
  }
    if (!is.null(col.area)) 
      polygon(c(x[1],x,x[length(x)]), c(min(y),y,min(y)),
              col=col.area, border=col_color)

  # plot lines and points
  if (type == "l" || type == "b") {
    lines(as.numeric(x),y, col=col.line, lwd=line_width, ...)
  }
  if (type == "p" || type == "b") {
    points(x,y, col=col_color, pch=shape_pts, bg=col_fill, cex=pt.size, ...)
  }

  # plot center line
  if (center_line != "off") {
    if (center_line == "mean") {
      m.y <- mean(y)
      lbl <- "mean"
      lbl.cat <- "mean:"
    }
    else if (center_line == "median") {
      m.y <- median(y)
      lbl <- "medn"
      lbl.cat <- "median:"
    }
    else if (center_line == "zero") {
      m.y <- 0
      lbl <- ""
      lbl.cat <- "median:"
    }

    if (!is.ts(y)) {
      abline(h=m.y, col="gray50", lty="dashed")
      mtext(lbl, side=4, cex=.9, col="gray50", las=2, at=m.y, line=0.1)
    }
    if (center_line == "zero") m.y <- median(y) 

    gl <- .getlabels()
    x.name <- gl$xn; x.lbl <- gl$xl;
    y.name <- gl$yn; y.lbl <- gl$yl
    
    
# -----------
# text output

    if (!quiet) {
      ttlns <- .title2(x.name, y.name, x.lbl, y.lbl, TRUE)

    txsug <- ""
    if (getOption("suggest")  &&  !is.ts(y)) {
      txsug <- ">>> Suggestions"
      fc <- paste("\nLineChart(", x.name, ", area=\"steelblue\")", sep="")         
      txsug <- paste(txsug, fc, sep="")
      fc <- paste("\nLineChart(", x.name, ", show_runs=TRUE)", sep="")           
      txsug <- paste(txsug, fc, sep="")
    }

    class(ttlns) <- "out"
    class(txsug) <- "out"
    output <- list(out_title=ttlns, out_suggest=txsug)
    class(output) <- "out_all"
    print(output)    
  }

    # analyze runs
    if (!quiet  &&  !is.ts(y)) {
      cat("\n")
      cat("n:", n, "\n")
      cat("missing:", n.miss, "\n")
      cat(lbl.cat, round(m.y,digits_d), "\n")
      cat("\n")
      .dash(12); cat("Run Analysis\n"); .dash(12)
      run <- integer(length=0)  # length of ith run in run[i]
      n.runs <- 1  # total number of runs
      run[n.runs] <- 1
      line.out <- "    1"
      cat("\n")
      for (i in 2:length(y)) {
        if (y[i] != m.y) {  # throw out values that equal m.y
          if (sign(y[i]-m.y) != sign(y[i-1]-m.y)) {  # new run
            if (show_runs) {
              if (n.runs < 10) buf <- "  " else buf <- " "
              cat("size=", run[n.runs], "  Run", buf, n.runs, ":",
                  line.out, "\n", sep="")
            }
            line.out <- ""
            n.runs <- n.runs + 1
            run[n.runs] <- 0
          }
          run[n.runs] <- run[n.runs] + 1
          buf <- ifelse (i < 10, "  ", " ")
          line.out <- paste(line.out, buf, i)
        }
      }
      cat("size=", run[n.runs], "  Run", buf, n.runs, ":", line.out,
          "\n", sep="")
      eq.ctr <- which(y==m.y)
      cat("\nTotal number of runs:", n.runs, "\n")
      txt <- "Total number of values that do not equal the "
      cat(txt, lbl.cat, " ", length(y)-length(eq.ctr), "\n", sep="")
      if (length(eq.ctr) != 0) {
        if (show_runs) {
          cat("\nValues ignored that equal the", lbl.cat, "\n")
          for (i in 1:length(eq.ctr))
            cat("    #", eq.ctr[i], " ", y[eq.ctr[i]], sep="", "\n")
          cat("Total number of values ignored:", length(eq.ctr), "\n")
        }
      }
      else 
        cat("Total number of values ignored that equal the", lbl.cat, 
            length(eq.ctr), "\n")
    }
  }

  if (!quiet) cat("\n")

}

