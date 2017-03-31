.bx.main <-
function(x, col.fill, col.stroke, col.bg, col.grid,
         col.box, cex.axis, col.axis, rotate.x, rotate.y, offset, horiz, add.points,
         xlab, main, sub, digits.d, quiet, do.plot, fun.call, ...) {      


  # scale for regular R or RStudio
  adj <- .RSadj(radius=NULL, cex.axis)
  size.axis <- adj$size.axis
  size.lab <- adj$size.lab

  if (is.null(col.stroke)) col.stroke <- col.fill

  # get variable label if exists
  gl <- .getlabels(xlab, main=main, cex.lab=getOption("lab.size"))
  x.name <- gl$xn;  x.lbl <- gl$xl
  x.lab <- gl$xb
  if (horiz)
    y.lab <- ""
  else {
    y.lab <- x.lab
    x.lab <- ""
  }
  main.lab <- gl$mb
  sub.lab <- gl$sb
  cex.lab <- gl$cex.lab
 
  if (do.plot) {
    # set margins
    margs <- .marg(0, y.lab, x.lab, main)
    lm <- margs$lm
    tm <- margs$tm
    rm <- margs$rm
    bm <- margs$bm
   
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    
    par(mai=c(bm, lm, tm, rm))

    # set up plot area
    bv <- (boxplot(x, col="transparent", bg="transparent",
       horizontal=horiz,  axes=FALSE, ann=FALSE, ...))

    # axes
    if (horiz)
    .axes(x.lvl=NULL, y.lvl=NULL, axTicks(1), NULL,
          par("usr")[1], par("usr")[3], size.axis, col.axis, 
          rotate.x, rotate.y, offset=offset, ...)
    else
    .axes(x.lvl=NULL, y.lvl=NULL, NULL, axTicks(2),
          par("usr")[1], par("usr")[3], size.axis, col.axis,
          rotate.x, rotate.y, offset=offset, ...)

    # axis labels
    max.lbl <- max(nchar(axTicks(2)))
    .axlabs(x.lab, y.lab, main.lab, sub.lab, max.lbl, 
            xy.ticks=TRUE, offset=offset, cex.lab=size.lab, ...) 
   
    usr <- par("usr")
   
    # colored background for plotting area
    rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="transparent")
    
    # grid lines computation and print
    if (horiz) {
      vx <- pretty(c(usr[1],usr[2]))
      abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
    }
    else {
      vy <- pretty(c(usr[3],usr[4]))
      abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    }
    # box plot
    boxplot(x, add=TRUE, col=col.fill, bg=col.stroke, pch=21,
            horizontal=horiz, axes=FALSE, border=col.stroke, ...)
   
    # colored background for plotting area
    rect(usr[1], usr[3], usr[2], usr[4], col="transparent", border=col.box)

    # dots
    if (add.points) 
        .dp.main(x, by=NULL, size=NULL, means=TRUE,
           col.fill, col.stroke, col.bg, col.grid, col.trans=NULL,
           shape.pts=NULL, cex.axis=.85, col.axis="gray30",
           xlab=NULL, main=NULL,
           method="overplot", pt.reg=21, pt.out=19, 
           col.out30="firebrick2", col.out15="firebrick4", bx=FALSE,
           quiet=TRUE, new=FALSE, do.plot=do.plot, fun.call=fun.call, ...)

  }  # end do.plot

  tx=""
  if (!quiet) {
    digits.d <- .max.dd(x)
    options(digits.d=digits.d)

  # summarize data
    n <- sum(!is.na(x))
    n.miss <- sum(is.na(x))
    mn <- .fmt(min(x, na.rm=TRUE))
    lw <- .fmt(bv$stats[1])
    lh <- .fmt(bv$stats[2])
    md <- .fmt(bv$stats[3])
    uh <- .fmt(bv$stats[4])
    uw <- .fmt(bv$stats[5])
    mx <- .fmt(max(x, na.rm=TRUE)) 
    IQR <- .fmt(IQR(x, na.rm=TRUE))

    tx <- character(length = 0)

    # main.lab <- as.character(main.lab)
    if (!is.null(main.lab))
      if (nchar(main.lab) > 0) main.lab <- paste(",", main.lab)
    x.lb <- as.character(x.lab[length(x.lab)])  # x.lab is a vector of class call
    txt <- paste("--- ", x.lb, main.lab, " ---", sep="")
    tx[length(tx)+1] <- txt
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("Present:", n)
    tx[length(tx)+1] <- paste("Missing:", n.miss)
    tx[length(tx)+1] <- paste("Total  :", length(x))
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("Minimum      :", mn)
    tx[length(tx)+1] <- paste("Lower Whisker:", lw) 
    tx[length(tx)+1] <- paste("Lower Hinge  :", lh) 
    tx[length(tx)+1] <- paste("Median       :", md) 
    tx[length(tx)+1] <- paste("Upper Hinge  :", uh) 
    tx[length(tx)+1] <- paste("Upper Whisker:", uw) 
    tx[length(tx)+1] <- paste("Maximum      :", mx )
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("1st Quartile :", .fmt(quantile(x, na.rm=TRUE)[2]))
    tx[length(tx)+1] <- paste("3rd Quartile :", .fmt(quantile(x, na.rm=TRUE)[4]))
    tx[length(tx)+1] <- paste("IQR          :", IQR)

  }  # end !quiet

  return(list(tx=tx, n=n, n.miss=n.miss, mn=mn, lw=lw, lh=lh, md=md, uh=uh,
         uw=uw, mx=mx, IQR=IQR))

}

