.dpmat.main <-   # BPFM
function(x, mylabels, sort.yx,
         col.fill, col.stroke, col.bg, col.trans,
         shape.pts, col.area, col.box, 
         cex.axis, col.low, col.hi,
         xy.ticks, xlab, ylab, main, sub, cex,
         radius, size.cut, txt.color="black", power,
         value.labels, rotate.x, rotate.y, offset, quiet,
         do.plot, fun.call=NULL, ...)  {


  # scale for regular R or RStudio
  adj <- .RSadj(radius, cex.axis)
  radius <- adj$radius
  size.axis <- adj$size.axis
  size.lab <- adj$size.lab
  size.txt <- adj$size.txt

  if (!is.null(value.labels)) value.labels <- gsub(" ", "\n", value.labels) 

  # if exist, get axes labels
  gl <- .getlabels(xlab, ylab, main, cex.lab=size.lab)
  x.lab <- gl$xb
  y.lab <- gl$yb
  main.lab <- gl$mb
  sub.lab <- gl$sb
  size.lab <- gl$cex.lab

  n.var <- ncol(x)

  # get the response categories for each variable
  #   but if numeric or character, are encoded as character
  # if 1st row from table misses first numeric values, 
  #   union does not preserve numeric order
  # if responses are defined from a factor, then order preserved
  resp <- NULL
  for (i in 1:n.var) {
    tbl <- table(x[,i])
    resp <- union(resp,names(tbl))
  }
  n.resp <- length(resp)

  # if responses are numeric encoded as chars, do a numeric sort
  x.resp <- NULL
  if (suppressWarnings(all(!is.na(as.numeric(as.character(resp)))))) {
    x.resp <- as.numeric(as.character(resp))
    x.resp <- sort(x.resp)
    resp <- as.character(x.resp)
  }

  # get the frequencies for each response, including 0
  frq <- NULL
  frq.i <- integer(length=n.resp)
  names(frq.i) <- resp
  for (i in 1:n.var) {
    tbl <- table(x[,i])   
    for (j in 1:length(frq.i)) frq.i[j] <- 0
    for (j in 1:length(frq.i))
      if (names(frq.i)[j] %in% names(tbl))
        frq.i[j] <- tbl[which(names(tbl) == names(frq.i)[j])]
    frq <- c(frq, frq.i)
  }

  # frequency table
  mytbl <- matrix(frq, nrow=n.resp, ncol=n.var)
  rownames(mytbl) <- names(frq.i)
  colnames(mytbl) <- names(x)
  mytbl <- t(mytbl)  # rows are variables, cols are responses

  # calculate weighted means of each variable
  w <- integer(length=n.resp)
  if (!is.null(x.resp))
    w <- x.resp  # use existing integer codes
  else
    for (i in 1:n.resp) w[i] <- i
  m <- integer(length=n.var)
  for (i in 1:n.var) m[i] <- round(weighted.mean(w, mytbl[i,]), 3)
  if (sort.yx) {
    m.o <- order(m, decreasing=FALSE)
    mytbl <- mytbl[m.o,]
    m <- m[m.o]
  }
  else
    m.o <- 1:n.var
    
  # melt the table to a data frame
  k <- 0
  n.count <- nrow(mytbl) * ncol(mytbl)
  xx <- integer(length=n.count)
  yy <- integer(length=n.count)
  count <- integer(length=n.count)
  for (i in 1:nrow(mytbl)) {
    for (j in 1:ncol(mytbl)) {
      k <- k + 1
      count[k] <- mytbl[i,j]
      xx[k] <- j
      yy[k] <- i
    }
  }
  cords <- data.frame(xx, yy, count)

  c <- cords$count  # 0 plots to a single pixel, so remove
  for (i in 1:length(c)) if (c[i]==0) c[i] <- NA

  if (do.plot) {
    # set margins
    y.lvl <- rownames(mytbl)
    max.width <- strwidth(y.lvl[which.max(nchar(y.lvl))], units="inches")
  
    # here keep same bm if 1 or 2 line x labels
    margs <- .marg(max.width, y.lab, x.lab, main) 
    lm <- margs$lm
    tm <- margs$tm
    rm <- margs$rm
    bm <- margs$bm
   
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    
    par(bg=getOption("device.fill"))
    par(mai=c(bm, lm, tm, rm))

    plot(cords$xx, cords$yy, type="n", axes=FALSE, ann=FALSE, 
         xlim=c(.5, n.resp+.5), ylim=c(.5, n.var+.5))

    # axis, axis ticks, value labels
    if (is.null(value.labels))
      x.lvl <- colnames(mytbl)
    else
      x.lvl <- value.labels
      x.lvl <- gsub(" ", "\n", x.lvl) 

    .axes(x.lvl, y.lvl, axTicks(1), 1:n.var,
          par("usr")[1], par("usr")[3], size.axis, getOption("axis.x.stroke"),
          rotate.x, rotate.y, offset=offset, ...)

    # axis labels 
    if (!is.null(y.lvl))
      max.lbl <- max(nchar(y.lvl))
    else
      max.lbl <- max(nchar(axTicks(2)))
      y.lab <- ""
      max.lbl <- 0

    .axlabs(x.lab, y.lab, main.lab, sub.lab, max.lbl, 
            xy.ticks=TRUE, offset=offset, cex.lab=size.lab, ...) 

    usr <- par("usr")

    # color plotting area
    rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="transparent")

    # grid lines
    abline(h=1:n.var, col=getOption("grid.y,stroke"),
         lwd=getOption("grid.lwd"), lty=getOption("grid.lty"))
    abline(v=axTicks(1), col=getOption("grid.x.stroke"),
         lwd=getOption("grid.lwd"), lty=getOption("grid.lty"))  # bubbles only
 
    # box around plot
    rect(usr[1], usr[3], usr[2], usr[4], col="transparent", border=col.box,
      lwd=getOption("bg.lwd"), lty=getOption("bg.lty"))
    if (col.box == "transparent") {
      if (getOption("axis.x.stroke") != "transparent")
        segments(usr[1], usr[3], usr[2], usr[3], lwd=3)
      if (getOption("axis.y.stroke") != "transparent")
        segments(usr[1], usr[3], usr[1], usr[4], lwd=3)
    }

    # colors
    if (is.null(col.low) ||  is.null(col.hi))
      clr <- col.fill
    else {
      color.palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color.palette(n.resp)
    }

    # bubbles
    if (!is.null(col.trans)) {
      trans.pts <- col.trans
      for (i in 1:length(clr)) clr[i] <- .maketrans(clr[i], (1-trans.pts)*256)
    }
    symbols(cords$xx, cords$yy, circles=c, bg=clr, 
          fg=col.stroke, inches=radius, add=TRUE, ...)

    # counts
    if (size.cut) { 
      max.c <- max(c, na.rm=TRUE)  # do not display count if bubble is too small
      #min.bubble <- (.5 - (0.9*radius)) * max.c 
      min.bubble <- (power/2.5) * max.c
      for (i in 1:length(c))
        if (!is.na(c[i])) if (c[i] <= min.bubble) c[i] <- NA
      text(cords$xx, cords$yy, c, cex=size.txt, col=txt.color)
    }
  }

  if (!quiet) {
  
    if (getOption("suggest")) {
      # function call for suggestions
      fncl <- .fun.call.deparse(fun.call) 
      fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
      fncl <- gsub(" = ", "=", fncl)
    }  # display suggestions
    
    txsug <- ""
    if (getOption("suggest")) {
      txsug <- ">>> Suggestions"
      fc <- ""
      if (!grepl("radius", fncl))
        fc <- paste(fc, ", radius=0.2", sep="")
      if (nzchar(fc)) {
        fc <- paste(fncl, fc, ") ", sep="")
        txsug <- paste(txsug,"\n", fc, sep="")
      }
            
      txsug <- .rm.arg.2(" x=", txsug) 
      txsug <- .rm.arg.2("(x=", txsug) 
    }

    # display variable labels
    txlbl <- ""
    if (!is.null(mylabels)) {
      tx <- character(length = 0)
      tx[length(tx)+1] <- ">>> Labels"
      for (i in 1:length(rownames(mytbl))) {
        ml <- mylabels[i]
        if (!is.na(ml))
          tx[length(tx)+1] <- paste(rownames(mytbl)[i], ": ", ml, sep="")
      }
      txlbl <- tx
    }

    # display frequencies and means of each variable
    txttl <- "Frequencies of Responses by Variable"
    tx <- character(length = 0)
    mx.chr <- max(nchar(colnames(mytbl)))
    mx.len <- 8
    if (mx.chr > mx.len) {
      c.nm <- colnames(mytbl)
      colnames(mytbl) <- .abbrev(colnames(mytbl), mx.len)
    }
    myt <- addmargins(mytbl, margin=2)
    txtbl <- .prntbl(myt, 0, cc=NULL)
    for (i in 1:length(txtbl)) tx[length(tx)+1] <- txtbl[i]
    mx <- max(nchar(m))
    txtbl[1] <- paste(txtbl[1], .fmtc("Mean", w=mx+1))
    for (i in 1:n.var) txtbl[i+1] <- paste(txtbl[i+1], .fmt(m[i],3,w=mx+1))
    if (mx.chr > mx.len) {
      txtbl[n.var+2] <- ""
      txtbl[n.var+3] <- ""
      txtbl[n.var+4] <- "Unabbreviated labels"
      txtbl[n.var+5] <- "--------------------"
      txtbl[n.var+6] <- paste(c.nm, sep="", collapse="\n")
    }
    strt <- ifelse (mx.chr > mx.len, 7, 2)
    if (is.null(x.resp)) {
      txtbl[n.var+strt] <- ""
      txtbl[n.var+strt+1] <- paste("Computation of the mean based on coding",
                                   "response categories from 1 to", n.resp)
    }
    
    myt <- cbind(myt, m[m.o])
    colnames(myt)[n.resp+2] <- "Mean"    
    
    txfrq <- txtbl
   
    class(txsug) <- "out_piece"
    class(txlbl) <- "out_piece"
    class(txttl) <- "out_piece"
    class(txfrq) <- "out_piece"
    
    if (nzchar(txsug))
      output <- list(out_suggest=txsug,
                     out_text=txlbl, txout_title=txttl, out_text=txfrq)
    else
      output <- list(out_text=txlbl, txout_title=txttl, out_text=txfrq)
                     
    class(output) <- "out_all"
    print(output)
    
  }  # end !quiet

}

