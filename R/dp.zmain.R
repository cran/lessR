.dp.main <- 
function(x, by, size, means,
         col.fill, col.stroke, col.bg, col.trans,
         shape.pts, cex.axis, xlab, main, sub,
         rotate.x, rotate.y, offset, method, pt.reg, pt.out, 
         col.out30, col.out15, bx, quiet, new, vertical,
         do.plot, fun.call=NULL, ...) {

         
  if (is.factor(x)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The variable cannot be an R factor (categorical).\n")
  }  

  col.fill <- getOption("bar.fill")
  
  # outlier points shapes and colors for gray scales
  if (getOption("theme") %in% c("gray", "gray.black")) {
    pt.out30 <- 23
    pt.out15 <- 22
  }
  else {
    pt.out30 <- pt.out
    pt.out15 <- pt.out
  }
  if (getOption("theme") == "gray") {
    col.out30 <- "black"
    col.out15 <- "gray30"
  }
  else if (getOption("theme") == "gray.black") {
    col.out30 <- "gray95"
    col.out15 <- "gray60"
  }

  # get values for ... parameter values
  stuff <- .getdots(...)
  col.main <- stuff$col.main
  col.lab <- stuff$col.lab
  col.sub <- stuff$col.sub
  cex.main <- stuff$cex.main
  
  # scale for regular R or RStudio
  adj <- .RSadj(cex.axis=cex.axis)
  size.axis <- adj$size.axis
  size.lab <- adj$size.lab
  
  # get variable labels if exist plus axes labels
  gl <- .getlabels(xlab, main=main, cex.lab=size.lab)
  x.name <- gl$xn; x.lbl <- gl$xl
  x.lab <- gl$xb
  main.lab <- gl$mb
  sub.lab <- gl$sb
  size.lab <- gl$cex.lab
  by.name <- getOption("byname")

  # size of points, a little larger than from .plt.zmain
  if (is.null(size)) {
    sz <- ifelse (.Platform$OS == "windows", 1.20, 0.90)
    if (options("device") == "RStudioGD")
      size.pt <- ifelse (.Platform$OS == "windows", sz*1.05, sz*1.13)
    else
      size.pt <- sz
  }
  else
    size.pt <- size


  # text output (before remove missing)
  if (!quiet && new) .ss.numeric(x, brief=TRUE)

  n <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  if (n.miss > 0) x <- na.omit(x)

  if (do.plot) { 
    if (new) {
      # set up plot area
   
      # set margins
      margs <- .marg(0, y.lab=NULL, x.lab, main)
      lm <- margs$lm
      tm <- margs$tm
      rm <- margs$rm
      bm <- margs$bm
      
      if (!is.null(x.lab)) if (grepl("\n", x.lab[1], fixed=TRUE)) bm <- bm + .10
      if (!is.null(by)) rm <- rm + 0.75
   
      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))
    
      par(bg=getOption("device.fill"))
      par(mai=c(bm, lm, tm, rm))

      stripchart(x, col="transparent", xlab=NULL, ylab=NULL, main=NULL,
         axes=FALSE, ann=FALSE, ...)
                
      usr <- par("usr")

      # axis, axis ticks
      .axes(x.lvl=NULL, y.lvl=NULL, axTicks(1), NULL,
            usr[1], usr[3], cex.axis=size.axis, getOption("axis.x.stroke"),
            rotate.x, rotate.y, offset, ...)

      # axis labels
      y.lab <- ""
      max.lbl <- max(nchar(axTicks(2)))
      .axlabs(x.lab, y.lab, main.lab, sub.lab, max.lbl,
            xy.ticks=TRUE, offset=offset, cex.lab=size.lab, ...)
            
      # colored plotting area
      rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="transparent")

      # grid lines
      vx <- pretty(c(usr[1],usr[2]))
      abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]),
         col=getOption("grid.x.stroke"),
         lwd=getOption("grid.lwd"), lty=getOption("grid.lty")) 

      # box around plot
      rect(usr[1], usr[3], usr[2], usr[4],
        col="transparent", border=getOption("bg.stroke"),
        lwd=getOption("bg.lwd"), lty=getOption("bg.lty"))
    }  # end new

    else
      usr <- par("usr")


    # outline=FALSE suppresses outlier points, are displayed from stripchart
    if (bx)
      boxplot(x, add=TRUE, col=col.fill, border=col.stroke, pch=21,
          horizontal=TRUE, axes=FALSE, outline=FALSE, ...)

    # mark outliers
    q1 <- quantile(x, probs=0.25)
    q3 <- quantile(x, probs=0.75)
    lo30 <- q1 - 3.0*IQR(x)
    lo15 <- q1 - 1.5*IQR(x)
    up15 <- q3 + 1.5*IQR(x)
    up30 <- q3 + 3.0*IQR(x)
    stripchart(x[x<lo30], add=TRUE, method=method,
               col=col.out30, bg=col.out30, pch=pt.out30, cex=size.pt, ...)
    stripchart(x[x<lo15], add=TRUE, method=method,
               col=col.out15, bg=col.out15, pch=pt.out15, cex=size.pt, ...)
    stripchart(x[x>up15], add=TRUE, method=method,
               col=col.out15, bg=col.out15, pch=pt.out15, cex=size.pt, ...)
    stripchart(x[x>up30], add=TRUE, method=method,
               col=col.out30, bg=col.out30, pch=pt.out30, cex=size.pt, ...)

    # dp for regular points

    if (is.null(by)) {
      # see if trans is customized for this analysis
      if (!is.null(col.trans)) {
        trans.pts <- col.trans
        col.fill <- .maketrans(col.fill, (1-trans.pts)*256)
      }
      stripchart(x[x>lo15 & x<up15], add=TRUE, method=method,
                       col=col.stroke, pch=pt.reg, bg=col.fill, cex=size.pt, ...)

      # plot mean
      if (means) {
        m.lvl <- mean(x, na.rm=TRUE)
        abline(v=m.lvl, col="gray50", lwd=.5)
        points(m.lvl, 1, pch=21, bg="black")
        txt.y <- usr[3] + (usr[4] - usr[3]) / 30
        text(m.lvl, txt.y, "mean", col="gray50", cex=0.9)
      }
    }

    else {  # by grouping variable
      n.levels <- nlevels(by)

      clr <- character(length(n.levels))
      if (length(col.stroke) == 1) 
        for (i in 1:n.levels) clr[i] <- col.stroke
      else
        clr <- col.stroke
      clr.tr <- clr

      shp <- integer(length(n.levels))
      if (length(shape.pts) == 1)
        for (i in 1:n.levels) shp[i] <- shape.pts
      else
         shp <- shape.pts
      shape.dft <- c(21,23,22,24,25,7:14)  # shape defaults
      if (length(col.stroke)==1 && length(shape.pts)==1)  # both shape, color default
        for (i in 1:n.levels) shp[i] <- shape.dft[i]  # fill with default shapes

      trans.pts <- getOption("trans.pt.fill")
      for (i in 1:n.levels) {
          clr.tr[i] <- .maketrans(clr.tr[i], (1-trans.pts)*256)
        x.lv <- subset(x, by==levels(by)[i])
        stripchart(x.lv, pch=shp[i], col=clr[i], bg=clr.tr[i], 
               cex=size.pt, lwd=0.75, add=TRUE, ...)
      }

      .plt.by.legend(levels(by), col.stroke, clr.tr, shp, trans.pts, col.bg, usr)

    }  # end by group

  }  # end do.plot

  
  if (getOption("suggest")) {
    # function call for suggestions
    fncl <- .fun.call.deparse(fun.call) 
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)
  }

  txsug <- ""
  if (getOption("suggest")) {
    txsug <- ">>> Suggestions"

    fc <- ""
    if (!grepl("means", fncl))
      fc <- paste(fc, ", means=FALSE", sep="")
    if (nzchar(fc)) {
      fc <- paste(fncl, fc, ") ", sep="")
      txsug <- paste(txsug,"\n", fc, sep="")
    }

    fc <- ""
    if (!grepl("size", fncl))
      fc <- paste(fc, ", size=2", sep="")
    if (nzchar(fc)) {
      fc <- paste(fncl, fc, ") ", sep="")
      txsug <- paste(txsug,"\n", fc, sep="")
    }
 
    fc <- ""
    if (!grepl("boxplot", fncl))
      fc <- paste(fc, ", boxplot=TRUE", sep="")
    if (nzchar(fc)) {
      fc <- paste(fncl, fc, ") ", sep="")
      txsug <- paste(txsug,"\n", fc, sep="")
    }
    
    fc <- ""
    if (!grepl("values", fncl))
      fc <- paste(fc, ", values=\"count\")", sep="")
    if (nzchar(fc)) {
      fc <- paste("Plot(", x.name, fc, sep="")
      txsug <- paste(txsug,"\n", fc, sep="")
    }
   
    fc <- ""
    if (!grepl("values", fncl))
      fc <- paste("Plot(", x.name, ", values=\"count\"", sep="")
    if (!grepl("values", fncl))
      fc <- paste(fc, ", area=TRUE", sep="")
    if (nzchar(fc)) {
      fc <- paste(fc, ") ", sep="")
      txsug <- paste(txsug,"\n", fc, sep="")
    }
    
    txsug <- .rm.arg.2(" x=", txsug) 
    txsug <- .rm.arg.2("(x=", txsug) 

   }

  txss <- ""
  if (!quiet) {
    digits.d <- NULL
    ssstuff <- .ss.numeric(x, digits.d=digits.d, brief=TRUE)
    txss <- ssstuff$tx
    
    txotl <- ""
    txotl <- .outliers(x)
    if (txotl[1] == "") txotl <- "No (Box plot) outliers"

    class(txsug) <- "out_piece"
    class(txss) <- "out_piece"
    class(txotl) <- "out_piece"
    
    if (nzchar(txsug))
      output <- list(out_suggest=txsug, out_ss=txss, out_outliers=txotl)
    else
      output <- list(out_ss=txss, out_outliers=txotl)
    
    class(output) <- "out_all"
    print(output)
  }

}
