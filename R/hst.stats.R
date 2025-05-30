.hst.stats <-
function (h, len.x, mx.dd, fun_call) {

    # function call for suggestions
    fncl <- .fun_call.deparse(fun_call)

    tx <- character(length = 0)
    if (getOption("suggest")) {
     tx[length(tx)+1] <- ">>> Suggestions"
     if (!grepl("bin_width", fncl))
        tx[length(tx)+1] <- "bin_width: set the width of each bin"
      if (!grepl("bin_start", fncl))
        tx[length(tx)+1] <- "bin_start: set the start of the first bin"
      if (!grepl("bin_end", fncl))
        tx[length(tx)+1] <- "bin_end: set the end of the last bin"
      txt <- "  # smoothed curve + histogram"
      tx[length(tx)+1] <- paste("Histogram(", getOption("xname"),
         ", density=TRUE)", txt, sep="")
      txt <- "  # Violin/Box/Scatterplot (VBS) plot"
      tx[length(tx)+1] <- paste("Plot(", getOption("xname"), ")", txt,
         sep="")
    }
    txsug <- tx
    if (length(txsug) == 0) txsug <- ""


    tx <- character(length = 0)

    bin_width <- h$breaks[2]-h$breaks[1]
    n.bins <- length(h$breaks)-1
    tx[length(tx)+1] <- paste("\nBin Width:", bin_width)
    tx[length(tx)+1] <- paste("Number of Bins:", n.bins)
    tx[length(tx)+1] <- ""

    # j<17 condition is to stop the 0.99999... problem
    max.dg <- 0
    for (i in 1:length(h$breaks)) {
      j <- nchar(as.character(h$breaks[i]))
      if (j>max.dg && j<17) max.dg <- j
    }
    max.dg.mid <- 0
    for (i in 1:length(h$mids)) {
      j <- nchar(as.character(h$mids[i]))
      if (j>max.dg.mid && j<19) max.dg.mid <- j
    }

    x.breaks <- format(h$breaks, width=max.dg, justify="right", scientific=FALSE)
    x.mids <- format(h$mids, width=max.dg.mid, justify="right", scientific=FALSE)
    # if bin_start < 0, a midpt approx 0, format() then has too many digits
    if (.lead0(h$mids) > mx.dd)
      x.mids <- .fmt(h$mids, d=mx.dd)

    bn <- character(length=0)
    for (i in 1:(length(x.breaks)-1))
      bn[i] <- paste(x.breaks[i], ">", x.breaks[i+1])

    cum.c <- cumsum(h$counts)
    prop <- h$counts / len.x
    cum.p <- cumsum(prop)

    out <- data.frame(bn)
    out$x.mids <- x.mids
    out$counts <- formatC(h$counts, digits=0, format="f")
    out$prop <- formatC(prop, digits=2, format="f")
    out$cum.c <- formatC(cum.c, digits=0, format="f")
    out$cum.p <- formatC(cum.p, digits=2, format="f")
    names(out) <- c("Bin", "Midpnt", "Count", "  Prop", "Cumul.c", "Cumul.p")

    # width of columns
    max.ln <- integer(length=0)
    for (i in 1:ncol(out)) {
      ln.nm <- nchar(colnames(out)[i]) + 1
      max.val <- max(nchar(out[,i]))
      max.ln[i] <- max(ln.nm, max.val) + 1
    }

    # write col labels
    tx[length(tx)+1] <- ""
    for (i in 1:ncol(out))
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc(colnames(out)[i],
                              w=max.ln[i]), sep="")
    tx[length(tx)+1] <- .dash2(sum(max.ln))

    # write values
    for (i in 1:nrow(out)) {
      tx[length(tx)+1] <- ""
       for (j in 1:ncol(out))
          tx[length(tx)] <- paste(tx[length(tx)], .fmtc(out[i,j],
                                   w=max.ln[j]), sep="")
    }
    tx[length(tx)+1] <- ""

    return(list(txsug=txsug, tx=tx, bin_width=bin_width, n.bins=n.bins,
                prop=prop, counts_cum=cum.c, prop_cum=cum.p))

}
