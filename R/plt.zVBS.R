.plt.VBS <-
function(x, ID, by1, by1.miss, by0, by.miss,
         bw, bw.miss, bw.iter, iter.details, lx, n.ux, 
         k.iqr, box.adj, a, b,
         x.name, by1.name, by.name, vbs.plot,
         n.col.miss, n.row.miss,
         size, j.x.miss, jitter.x, j.y.miss, jitter.y,
         bin=FALSE, breaks=NULL, bin.start=NULL, bin.width=NULL,
         bin.end=NULL, proportion=NULL, 
         digits.d, quiet, fun.call=NULL, ...) {


  .get.dup <- function(mc.w, x.name, lvl) {
    tx <- ""
    tx <- character(length = 0)

    # header
    max.lvl <- max(nchar(lvl))
    buf <- ifelse (max.lvl - 5 >= 0, max.lvl - 4, 0)
    h1 <- "Level"
    for (i in 1:buf) h1 <- paste0(h1, " ", collapse="")
    h0 <- gsub("Level", "    ", h1, fixed=TRUE)
    tx[length(tx)+1] <- paste(h0, "Max Dupli-")
    tx[length(tx)+1] <- paste(h1, "cations   Values")
    tx[length(tx)+1] <- .dash2(30)    

    # display repetitions
    for (i in 1:ncol(frq)) {
      mc.col <- max(frq[,i])
      val <- rownames(frq)[which(frq[,i]==mc.col) %% nrow(frq)]
      txt <- ""
      if (length(val) > 8) {
        val <- val[1:8]
        txt <- " ..."
      }
      val <- paste0(val, collapse=" ")  # convert char to num
      lvl.c <- .fmtc(lvl[i], max(max.lvl,6), j="left")
      if (mc.col > 1)
        tx[length(tx)+1] <- paste(lvl.c, "    ",  .fmti(mc.col,3),
          "     ", val, txt, sep="")
      else 
        tx[length(tx)+1] <- paste(lvl.c, "   0" )
    }

    txrep <- tx 
    class(txrep) <- "out"
    return(txrep)
  }

  # display current parameter values
  .get.param <- function(size, jitter.y, jitter.x, bw) {
    txprm <- ""
    tx <- character(length = 0)
    tx[length(tx)+1] <- "Parameter values (can be manually set)"
    tx[length(tx)+1] <- .dash2(55)
    if (grepl("s", vbs.plot)) {
      tx[length(tx)+1] <- paste("size:", .fmt(size.pt,2),
          "     size of plotted points")
      tx[length(tx)+1] <- paste("jitter.y:", .fmt(jitter.y,2),
          " random vertical movement of points")
      tx[length(tx)+1] <- paste("jitter.x:", .fmt(jitter.x,2),
          " random horizontal movement of points")
              if (size.pt < 0.02) size.pt <- .02
    }
    if (grepl("v", vbs.plot))
      tx[length(tx)+1] <- paste("bw:" , .fmt(bw,2),
        "    set bandwidth higher for smoother edges")
    txprm <- tx
    class(txprm) <- "out"
    return(txprm)
  }



  # BEGIN
  # -----
  
  if (is.null(by0)) by.name <- ""
  if (is.null(by1)) by1.name <- ""

  # suggestions
  if (getOption("suggest") && !quiet) {
    # function call for suggestions
    fncl <- .fun.call.deparse(fun.call) 
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)

    txsug <- ">>> Suggestions"

    fc <- paste("Plot(", x.name, sep="")

    txts <- character(length=3)
    cmts <- character(length=3)
    if (!grepl("out.cut", fncl)) {
      txts[1] <- ", out.cut=2"
      cmts[1] <- "Label two outliers ..."
    }
    if (!grepl("fences", fncl)) {
      txts[2] <- ", fences=TRUE"
      if (cmts[1] == "") cmts[2] <- "Show inner fences"
    }
    if (!grepl("vbs.mean", fncl)) {
      txts[3] <- ", vbs.mean=TRUE"
      if (cmts[1] == "") cmts[3] <- " Show mean"
    }
    if (any(txts != ""))
      txsug <- paste(txsug, "\n", fc, txts[1], txts[2], txts[3], ") # ",
        cmts[1], cmts[2], cmts[3], sep="")

    if (!grepl("box.adj", fncl)) {
      txt <- ", box.adj=TRUE)  # Adjust boxplot whiskers for asymmetry"
      txsug <- paste(txsug, "\n", fc, txt, sep="")
    }

    if (nlevels(by1) == 2  ||  nlevels(by0) == 2) {
      nm <- ifelse (nlevels(by1) == 2, by1.name, by.name)
      txt1 <- paste("ttest(", x.name, " ~ ", nm, ")", sep="") 
      txt2 <- "  # add the data parameter if not d"
      txsug <- paste(txsug, "\n", txt1, txt2, sep="")
    }

    if (nlevels(by1) > 2  ||  nlevels(by0) > 2) {
      nm <- ifelse (nlevels(by1) > 2, by1.name, by.name)
      txt1 <- paste("ANOVA(", x.name, " ~ ", nm, ")", sep="") 
      txt2 <- "  # add the data parameter if not d"
      txsug <- paste(txsug, "\n", txt1, txt2, sep="")
    }

    txsug <- .rm.arg.2(" x=", txsug) 
    txsug <- .rm.arg.2("(x=", txsug)
    txsug <- .rm.arg.2(" y=", txsug) 

    class(txsug) <- "out"

    if (nzchar(txsug)) {
      output <- list(out_suggest=txsug)
      class(output) <- "out_all"
      print(output)
    }

  }


  if (!is.factor(x)) if (bw.miss)
    bw <- .band.width(na.omit(x), bw.iter, iter.details, ...) # initial bw
#d.gen <- suppressWarnings(density(na.omit(x), bw, ...))
#xd <- diff(d.gen$y)
#cat("\n\n\n*** bw:", bw, "\n")
#xd2 <- diff(xd)
#cat("\n\nxd2:\n")
#print(as.numeric(.fmt(xd2,7)))
#cat("\n\nxd2 sorted:\n")
#print(sort(as.numeric(.fmt(xd2,7))))
  rep.prop <- (lx - n.ux) / lx


  # -------
  # ONE VAR VBS plot: # no by1, so x by itself
  if (by1.miss && by.miss) { 

    # box plot outliers, stats
    bx <- .bx.stats(x, ID, k.iqr, box.adj, a, b, digits.d)
    txbox <- bx$txstat
    txotl <- bx$txotl
    class(txbox) <- "out"
    class(txotl) <- "out"
    txgrp <- ""

    # check for repetitions of x values (as a single variable)
    # mx.c, max category, is max number of values of a value of x
    frq <- as.matrix(table(x))
    mx.c <- ifelse (n.ux == lx, 0, max(table(x))) 
    if (mx.c > 1)
      txrep <- .get.dup(mx.c, x.name, lvl=x.name)
    else
      txrep <- "Number of duplicated values: 0"

    iqr <- IQR(x, na.rm=TRUE)
    mx <- max(x, na.rm=TRUE)
    mn <- min(x, na.rm=TRUE)
    rt <- iqr / (mx - mn)  # ratio (rt) of IQR to range, eval compression

    reps <- ifelse (rep.prop > 0.15  &&  mx.c > (.10*lx), TRUE, FALSE)

    if (is.null(size)) {
      if (!reps)
        size.pt <- ifelse (lx < 2535,  # at this value both equations equal
          1.096 - 0.134*log(lx), 0.226 - 0.023*log(lx))
      else
        size.pt <- 0.842 - 0.109*log(mx.c)
      if (size.pt < 0.01) size.pt <- ifelse (lx < 25000, 0.015, 0.006) 
      if (rt < 0.18) size.pt <- (0.147 + 4.490*rt) * size.pt  # decrease size
    }
    else
      size.pt <- size  # assign user specified value

    if (j.x.miss) jitter.x <- 1.1 * (1-exp(-0.03*mx.c))
    if (j.y.miss) {
      if (!reps) 
         jitter.y <- ifelse (lx <= 10000,
                         -1.644 + 0.579*log(lx), -16.567 + 2.163*log(lx))
      else
         jitter.y <- -0.722 + 0.845*log(mx.c)
      jy.adj <- ifelse (rt < 0.18, 0.882 - 4.864*rt, 0)
      jitter.y <- jitter.y + (jy.adj * jitter.y)   # increases jitter
    } 

    if (grepl("v", vbs.plot) || grepl("s", vbs.plot)) 
      txprm <- .get.param(size, jitter.y, jitter.x, bw)
    else
      txprm <- ""

    # get freq table for discrete, before jitter, not all x are unique
    txdst <- ""
    if (bin) {
      h <- .hst.main(x, breaks=breaks, bin.start=bin.start,
         bin.width=bin.width, bin.end=bin.end, prop=proportion, 
         quite=quiet, fun.call=NULL, do.plot=FALSE, ...) 
      txdst <- h$ttx
      class(txdst) <- "out"
    }

    if (n.ux < 9  &&  n.ux < length(x)) {  # x is discrete 
      ssstuff <- .ss.factor(x, x.name=x.name, ...)
      txttl <- ssstuff$title
      txfrq <- ssstuff$counts
      txXV <- ssstuff$chi
      class(txttl) <- "out"
      class(txfrq) <- "out"
      class(txXV) <- "out"
      output <- list(type="VBS_Plot", call=fun.call,
        out_title=txttl, out_text=txfrq, out_freq=txdst, out_XV=txXV,
        out_tx=txbox, out_outliers=txotl, out_parm=txprm)
    }
    else {  # no freq table for not discrete variable

    if (!bin) txdst <- ""  # no freq distribution from histogram

    output <- list(type="Violin/Box/ScatterPlot",
      call=fun.call,  
      out_tx=txbox, out_outliers=txotl, out_freq=txdst,
      out_rep=txrep, out_parm=txprm)
    }

  }  # one var

  # -------
  # TWO VAR: x-variable with a Y categorical variable, by1
  else {
    if (by1.miss && !by.miss) by1 <- by0
    frq <- table(x, by1)
    n.lvl <- nlevels(by1)
    lvl <- levels(by1)

    # max number of repetitions for a value of x within a Ycat category
    # more jitter.x?
    mc.w <- max(frq)  # max category within, largest cell size
    rep.t <- integer(length=ncol(frq))
    for (i in 1:ncol(frq)) {
      rep.t[i] <- 0
      for (j in 1:nrow(frq))
        if (frq[j,i] > 1) rep.t[i] <- rep.t[i] + (frq[j,i] - 1)
    }
    rep.max <- max(rep.t)
    txrep <- .get.dup(mc.w, x.name, lvl)

    b.name <- ifelse(by.miss, by1.name, by.name)
    ssstuff <- .ss.numeric(x, by=by1, y.name=b.name,
                           digits.d=digits.d, brief=TRUE, by1.nm=TRUE)
    txgrp <- ssstuff$tx
    class(txgrp) <- "out"

    #if (n.col.miss && n.row.miss) n.col <- 1
    mx.c <- max(tapply(x, by1, length))
    if (rep.prop > 0.15  &&  mc.w > (.05 * lx))  # many reps?
      reps <- TRUE
    else
      reps <- FALSE 

    # continuous variable with a categorical variable
    if (!reps) {  # reps-cat
      if (is.null(size))
        size.pt <- 0.926 - 0.108*log(mx.c) - 0.023*n.lvl
      else
        size.pt <- size  # assign user specified value
      if (size.pt < 0.01) size.pt <- 0.01

      if (j.y.miss) {
        jitter.y <-  -1.221 + 0.576*log(mx.c) + 0.032*n.lvl
        if (jitter.y < 0.5) jitter.y <- 0.5
      }
      if (grepl("v", vbs.plot) || grepl("s", vbs.plot)) 
        txprm <- .get.param(size, jitter.y, jitter.x, bw)
      else
        txprm <- ""
      output <- list(out_grp=txgrp, out_rep=txrep, out_parm=txprm)
    }  # end !reps

    # discrete, numerical variable with a categorical variable
    else {  # discrete-cat, by1, yields the same
      if (!by1.miss) {
        mx.c <- max(table(x, by1))
        n.lvl <- nlevels(by1)
        #if (n.col.miss) n.col <- 1
      }

      if (is.null(size))
        size.pt <- 0.841 - 0.124*log(mc.w) 
      else
        size.pt <- size  # assign user specified value
      if (size.pt < 0.01) size.pt <- 0.01

      if (j.y.miss) {
        jitter.y <-  -6.261 + 2.012*log(rep.max) + 1.180*n.lvl
        if (jitter.y < 1.0) jitter.y <- 1.0
      }
      if (j.x.miss) jitter.x <- 0.086 + 0.141*log(mx.c)
      #if (j.x.miss) jitter.x <- 0.086 + 0.141*log(mc.w)
      if (jitter.x < 0) jitter.x <- 0
      if (grepl("v", vbs.plot) || grepl("s", vbs.plot)) 
        txprm <- .get.param(size, jitter.y, jitter.x, bw)
      else
        txprm <- ""

      if (n.ux < 9) {  # get x stats before jitter
        ssstuff <- .ss.factor(x, by=by1,
                              x.name=x.name, y.name=by1.name, ...)
        txfrq <- ssstuff$txfrq
        txXV <- ssstuff$txXV
        class(txfrq) <- "out"
        class(txXV) <- "out"
        output <- list(out_text=txfrq, out_XV=txXV,
           out_rep=txrep, out_parm=txprm)
      }
      else  # 9 or more levels
        output <- list(out_grp=txgrp, out_rep=txrep, out_parm=txprm)

    }

  }  # end two.var


  class(output) <- "out_all"
  if (!quiet) print(output)

  adj.bx.ht <- ifelse(mx.c == 0, lx, 3*lx)  # adjust the height of the box

  return(list(bw=bw, size.pt=size.pt, jitter.y=jitter.y, jitter.x=jitter.x,
              adj.bx.ht=adj.bx.ht))

}
