.plt.bins <-
function(x, y, nm.x, nm.y, stat, n_bins=6,
         segments=TRUE, size, digits_d=3, scale_x, scale_y, 
         fill=getOption("pt_fill"), color=getOption("pt_color"),
         trans=getOption("trans_pt_fill"),
         quiet=FALSE) {


  # -------- Conditions

  if (!(stat %in% c("mean", "median")))  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Parameter  stat  can only take two values for binning: \n",
      "  mean   and   median\n\n")
  }

  if (!is.numeric(x) || !is.numeric(y))  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Both variables must be numeric\n\n")
  }

  nx.unq <- length(unique(x))
  if (nx.unq < n_bins)  # too many bins
    message("You specified ", n_bins, " bins",
            ", but only ", nx.unq, " values of ", nm.x, "\n",
            "Empty bins will appear in the solution\n\n")

  if (is.na(n_bins) || n_bins < 2)  {  # not enough bins
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Need at least two bins set by  n_bins\n\n")
  }


  # -------- Process

  # get x-bins and midpoints, based on R cut() but allows format="f"
  nb <- as.integer(n_bins + 1)
  rx <- range(x, na.rm = TRUE)
  dx <- rx[2] - rx[1]
  bns <- seq.int(rx[1L], rx[2L], length.out=nb)  # bns is a vector
  bns[c(1L, nb)] <- c(rx[1L] - dx/1000, rx[2L] + dx/1000)  # extend range
  bins <- cbind(bns[1:nb-1], bns[2:nb])  # bins is a matrix
  midpt <- bins[,1] + (bins[,2] - bins[,1]) / 2
  bins = cbind(bins, midpt)

  # get labels
  ch.br <- formatC(bns, digits=digits_d, width=1L, format="f")
  labs <- paste0("(", ch.br[-nb], ",", ch.br[-1L], "]")
  substr(labs[1], 1, 1) <- "["  # indicate first bin is closed [ and ]

  # get mean or median of y for each x-bin
  n <- integer(length=nrow(bins))
  m <- double(length=nrow(bins))
  for (i in 1:nrow(bins)) {
    lower <- bins[i,1]
    upper <- bins[i,2]
    # only need y's; [x... ] returns T, F, NA; if x[i]=NA, y[i]=NA
    # each x[i]=NA gets in each y.sub, NA behaves like T instead of F
    # x[i]=NA triggers a y[i]=NA and a y[i] by itself can be NA
    y.sub <- na.omit(y[x > lower & x <= upper]) # select, drop all NA's 
    n[i] <- length(y.sub)
    if (stat == "mean")
      m[i] <- mean(y.sub, na.rm=TRUE)
    else if (stat == "median")
      m[i] <- median(y.sub, na.rm=TRUE)
  }

  # plot
  dbin <- data.frame(cbind(bins[,3], m))  # .plt.main input is data frames
  d.x <- dbin[,1, drop=FALSE]  # midpt
  d.y <- dbin[,2, drop=FALSE]  # mean or median
  if (is.null(size)) {
    den <- max(n, na.rm=TRUE) / 10  # scaling factor so max pt.size is around 10
    size <- (n/den)^.6
  }
  .plt.main(d.x, d.y, xlab=nm.x, ylab=paste(stat, "of", nm.y), 
            segments=segments, fill=fill, color=color, size=size, 
            pts_trans=trans, scale_x=scale_x, scale_y=scale_y,
            bubble.title=FALSE)  # processed as object="point", not .plt.bubble


  # -------- Output Tables

  if (!quiet) {
    # global summary stats
    n.pres <- c(length(na.omit(x)), length(na.omit(y)))
    n.miss = c(length(x)-length(na.omit(x)), length(y)-length(na.omit(y)))
    dx <-  ifelse (.is.integer(x), 0, digits_d)
    dy <-  ifelse (.is.integer(y), 0, digits_d)
    min.x = c(.fmt(min(x, na.rm=TRUE), dx), .fmt(min(y, na.rm=TRUE), dy))
    max.x = c(.fmt(max(x, na.rm=TRUE), dx), .fmt(max(y, na.rm=TRUE), dy))
    mean.x = c(.fmt(mean(x, na.rm=TRUE), digits_d),
               .fmt(mean(y, na.rm=TRUE), digits_d))
    d.smr <- data.frame(rbind(n.pres, n.miss, min.x, max.x, mean.x))
    names(d.smr) <- c(nm.x, nm.y)
    rownames(d.smr)[rownames(d.smr) == "n.pres"] <- "n"
    rownames(d.smr)[rownames(d.smr) == "min.x"] <- "min"
    rownames(d.smr)[rownames(d.smr) == "max.x"] <- "max"
    rownames(d.smr)[rownames(d.smr) == "mean.x"] <- "mean"
    cpt="Summary Stats"
    smr <- knitr::kable(d.smr, format="pandoc", digits=2, caption=cpt,
                        align="r")
    class(smr) <- "out"

    # bin summary stats
    m <- .fmt(m, digits_d)
    midpt <- .fmt(midpt, digits_d)
    d.tbl <- data.frame(rbind(labs, n, midpt, m))
    rownames(d.tbl)[rownames(d.tbl) == "labs"] <- "bin"
    rownames(d.tbl)[rownames(d.tbl) == "m"] <- stat
    names(d.tbl) <- gsub("X", "bin_", names(d.tbl), fixed=TRUE)
    d.tbl <- t(d.tbl)
    row.names(d.tbl) <- 1:nrow(d.tbl)
    cpt <- paste(stat, "of", nm.y, "for levels of", nm.x)
    w <- knitr::kable(d.tbl, format="simple", digits=2, caption=cpt, align="r",
                      row.names=TRUE, escape=FALSE, booktabs=TRUE)
    w <- c("",w)
    class(w) <- "out"

    output <- list(out_b1="", out_stats=smr, out_w=w)
    class(output) <- "out_all"
    print(output)
  }  # end if !quiet

}
