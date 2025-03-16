.dpmat.main <-   # BPFM (even for just 1 row)
function(x, l, sort_yx,  # no y variable
         fill, color, col.bg,
         trans, shape_pts, col.box, 
         col.low, col.hi,
         xy_ticks, xlab, ylab, main, sub, cex,
         radius, size_cut, txt_color="black", power,
         bm.adj, lm.adj, tm.adj, rm.adj,
         value_labels, rotate_x, rotate_y, offset, quiet,
         do_plot, fun_call=NULL, ...)  {

  col.area <- fill  # kludge
  size.txt <- getOption("axis_cex")

  if (!is.null(value_labels)) value_labels <- gsub(" ", "\n", value_labels) 

  # if exist, get axes labels
  gl <- .getlabels(xlab, ylab, main)
  x.lab <- gl$xb
  y.lab <- gl$yb
  main.lab <- gl$mb
  sub.lab <- gl$sb

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
    w <- 1:n.resp
  m <- integer(length=n.var)
  for (i in 1:n.var) m[i] <- round(weighted.mean(w, mytbl[i,]), 3)
  if (sort_yx != "0") {
    srt.dwn <- ifelse (sort_yx == "-", TRUE, FALSE)
    m.o <- order(m, decreasing=srt.dwn)
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
  cords <- data.frame(xx, yy, count, stringsAsFactors=TRUE)

  c <- cords$count  # 0 plots to a single pixel, so remove
  for (i in 1:length(c)) if (c[i]==0) c[i] <- NA

  if (do_plot) {
    # set margins
    y.lvl <- rownames(mytbl)
    max.width <- strwidth(y.lvl[which.max(nchar(y.lvl))], units="inches")
  
    # here keep same bm if 1 or 2 line x labels
    margs <- .plt.marg(max.width, y.lab, x.lab, main, rotate_x) 
    bm <- margs$bm
    lm <- margs$lm
    tm <- margs$tm
    rm <- margs$rm
  
    # user manual adjustment
    bm <- bm + bm.adj
    lm <- lm + lm.adj
    tm <- tm + tm.adj
    rm <- rm + rm.adj

    bm <- bm + 0.15
    if (rotate_x > 0) bm <- bm + 0.15
   
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    
    par(bg=getOption("window_fill"))
    par(mai=c(bm, lm, tm, rm))
    par(tcl=-0.28)  # axis tic length

    # set up plot area
    plot(cords$xx, cords$yy, type="n", axes=FALSE, ann=FALSE, 
         xlim=c(.5, n.resp+.5), ylim=c(.5, n.var+.5))

    # axis, axis ticks, value labels
    # NEED a bottom margin adjust for two lines of values
    # otherwise margin_adj=c(0,0,.2,0)
    if (is.null(value_labels))
      x.lvl <- colnames(mytbl)
    else
      x.lvl <- value_labels
      x.lvl <- gsub(" ", "\n", x.lvl) 

    .plt.bck(par("usr"), axTicks(1), 1:n.var)

    # adjust axis label from tick with mgp[2]
    # mgp does not work with rotate_x, see .axes()
    my.mgp <- par("mgp")  # save to restore
    ax <- .axes_dim()  # get axis value parameters
    mgp2 <- 0.250 + (1.05 * ax$axis_x_cex)
    par(mgp = c(my.mgp[1], mgp2, my.mgp[3]))  # labels closer to axis
    adj <- .RSadj(axis_cex=ax$axis_x_cex); axis_x_cex <- adj$axis_cex

    .axes(x.lvl, y.lvl, 1:length(x.lvl), 1:n.var,
          rotate_x=rotate_x, rotate_y=rotate_y, offset=offset, ...)

    .axlabs(x.lab, y.lab, main.lab, sub.lab,
            xy_ticks=TRUE, offset=offset, ...) 

    # colors
    if (is.null(col.low) || is.null(col.hi))
      clr <- fill
    else {
      color_palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color_palette(n.resp)
    }

    # bubbles
    # KLUDGE, trans default can be 0, so add some, need a lighter gray instead
    if (!is.null(trans)) {
      if (trans < 0.1) trans <- trans + .2
      trans_pts <- trans
      for (i in 1:length(clr)) clr[i] <- .maketrans(clr[i], (1-trans_pts)*256)
    }

    # scale for regular R or RStudio
    nch <- nchar(as.character(max(cords)))
    if (is.null(radius)) {
      radius <- -.35 + (.2*nch)
      if (radius < .22) radius <- .22
    }
    adj <- .RSadj(radius)
    radius <- adj$radius

    sz <- c**power 
    symbols(cords$xx, cords$yy, circles=sz, bg=clr, 
            fg=color, inches=radius, add=TRUE, ...)

    # counts
    if (size_cut) { 
      max.c <- max(c, na.rm=TRUE)  # do not display count if bubble is too small
      min_bubble <- (power/2.5) * max.c
      for (i in 1:length(c))
        if (!is.na(c[i])) if (c[i] <= min_bubble) c[i] <- NA
      text(cords$xx, cords$yy, c, cex=size.txt, col=txt_color)
    }
  }  # end do_plot


  # ------------
  # text output

  if (!quiet) {
  
    if (getOption("suggest")) {
      # function call for suggestions
      fncl <- .fun_call.deparse(fun_call) 
      fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
      fncl <- gsub(" = ", "=", fncl)
    }  # display suggestions
    
    txsug <- ""
    if (getOption("suggest")) {
      txsug <- ">>> Suggestions"
      fc <- ""
      if (!grepl("radius", fncl))
        fc <- paste(fc, ", radius=0.3", sep="")
      if (nzchar(fc)) {
        fc <- paste(fncl, fc, ") ", sep="")
        txsug <- paste(txsug,"\n", fc, sep="")
      }
            
      txsug <- .rm.arg.2(" x=", txsug) 
      txsug <- .rm.arg.2("(x=", txsug) 
    }

    # display variable labels
    txlbl <- ""
    l <- l[rownames(mytbl),]  # l is a data frame
    if (!is.null(l)) {
      tx <- character(length = 0)
      tx[length(tx)+1] <- ">>> Labels"
      for (i in 1:length(rownames(mytbl))) {
        if (is.data.frame(l)) 
          ml <- l[i,]
        else
          ml <- l[i]
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
   
    class(txsug) <- "out"
    class(txlbl) <- "out"
    class(txttl) <- "out"
    class(txfrq) <- "out"
    
    if (nzchar(txsug))
      output <- list(out_suggest=txsug,
                     out_text=txlbl, txout_title=txttl, out_text=txfrq)
    else
      output <- list(out_text=txlbl, txout_title=txttl, out_text=txfrq)
                     
    class(output) <- "out_all"
    print(output)
    
  }  # end !quiet

}

