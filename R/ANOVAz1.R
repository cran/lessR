.ANOVAz1 <- 
function(av.out, y.values, x.values, nm, n.obs, jitter_x, digits_d,
         brief, graphics, pdf, width, height) {

  p <- length(unique(na.omit(x.values)))

  # cell stats
  n <- tapply(y.values, x.values, length) 
  m <- tapply(y.values, x.values, mean) 
  s <- tapply(y.values, x.values, sd) 
  mn <- tapply(y.values, x.values, min) 
  mx <- tapply(y.values, x.values, max) 

  # get maximum chars in 1st three columns
  max.lv <- 0; max.n <- 0; max.m <- 0; max.s <- 0; max.mn <- 0; max.mx <- 0
  for (i in 1:p) {
    nch.lv <- nchar(as.character(levels(x.values)[i]))
    nch.n <- nchar(as.character(n[i]))
    nch.m <- nchar(.fmt(m[i],digits_d))
    nch.s <- nchar(.fmt(s[i],digits_d))
    nch.mn <- nchar(.fmt(mn[i],digits_d))
    nch.mx <- nchar(.fmt(mx[i],digits_d))
    if (nch.lv > max.lv) max.lv <- nch.lv
    if (nch.n > max.n) max.n <- nch.n
    if (nch.m > max.m) max.m <- nch.m
    if (nch.s > max.s) max.s <- nch.s
    if (nch.mn > max.mn) max.mn <- nch.mn
    if (nch.mx > max.mx) max.mx <- nch.mx
  }

  title_des <- "\n  DESCRIPTIVE STATISTICS "

  tx <- character(length = 0)

  n.tx <- format("n", width=max.lv+max.n+2, justify="right", sep="")
  m.tx <- format("mean", width=max.m+2, justify="right", sep="")
  s.tx <- format("sd", width=max.s+2, justify="right", sep="")
  mn.tx <- format("min", width=max.mn+2, justify="right", sep="")
  mx.tx <- format("max", width=max.mx+2, justify="right", sep="")
  tx[length(tx)+1] <- paste(n.tx, m.tx, s.tx, mn.tx, mx.tx)

  for (i in 1:p) {
    xval <- paste(format(levels(x.values)[i], width=max.lv, justify="left", sep="")) 
    nn <- format(n[i], width=max.n+1, justify="right", sep="") 
    mm <- format(sprintf("%.*f", digits_d, m[i]), width=max.m+2, justify="right") 
    ss <- format(sprintf("%.*f", digits_d, s[i]), width=max.s+2, justify="right") 
    mnn <- format(sprintf("%.*f", digits_d, mn[i]), width=max.mn+2, justify="right") 
    mxx <- format(sprintf("%.*f", digits_d, mx[i]), width=max.mx+2, justify="right") 
    tx[length(tx)+1] <- paste(xval, nn, mm, ss, mnn, mxx) 
  }

  mg <-  mean(y.values, na.rm=TRUE)
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste("Grand Mean:", round(mg, digits_d+1))

  txdes <- tx


  title_basic <- "\n  ANOVA"

  tx <- character(length = 0)

  if (is.null(options()$knitr.in.progress)) {
    tx[length(tx)+1] <- paste("-- Summary Table")
    tx[length(tx)+1] <- ""
  }

  n.vars <- 2
  smc <- anova(av.out)

  # width of column 1
  max.c1 <- max(nchar("Residuals"), nchar(nm[1]))
  for (i in 1:n.vars) {
    c1 <- nchar(rownames(smc)[i])
    if (c1 > max.c1) max.c1 <- c1 
  }

  # width of data columns
  max.ln <- integer(length=0)
  for (i in 1:4) {
    ln.nm <- nchar(colnames(smc)[i])
    max.ln[i] <- ln.nm + 1
    for (j in 1:nrow(smc)) {
      xjc <- .fmt(smc[j,i], d=digits_d)
      if (nchar(xjc) > max.ln[i]) max.ln[i] <- nchar(xjc)
    }
    max.ln[i] <- max.ln[i] + 1L
    if (max.ln[i] < 9L) max.ln[i] <- 9L
  }

  df.lbl <- .fmtc("     df", max.ln[1]+1)
  SS.lbl <- .fmtc(" Sum Sq", max.ln[2]+1)
  MS.lbl <- .fmtc("Mean Sq", max.ln[3]+1)
  fv.lbl <- .fmtc("F-value", max.ln[4]+1)
  tx[length(tx)+1] <- paste(format("", width=max.c1-4), df.lbl, SS.lbl,
                             MS.lbl, fv.lbl, "   p-value", sep="")

  for (i in 1:n.vars) {
    rlb <- .fmtc(rownames(smc)[i], max.c1, j="left")
    df <- format(sprintf("%i", smc[i,1]), width=max.ln[1]-4, justify="right")
    SS <- format(sprintf("%7.*f", digits_d, smc[i,2]), width=max.ln[2],
                 justify="right")
    MS <- format(sprintf("%7.*f", digits_d, smc[i,3]), width=max.ln[3],
                 justify="right")
    if (i < n.vars) {
      fv <- format(sprintf("%7.*f", digits_d, smc[i,4]), width=9,
                   justify="right")
      pv <- format(sprintf("%6.4f", smc[i,5]), width=9, justify="right")
      tx[length(tx)+1] <- paste(rlb, df, SS, MS, fv, pv)
    }
    else
      tx[length(tx)+1] <- paste(rlb, df, SS, MS)
    }

  sm <- summary(av.out)
  ssb <- sm[[1]][1,2]
  ssw <- sm[[1]][2,2]
  sst <- ssb + ssw
  msw <- sm[[1]][2,3]

  txanv <- tx


  tx <- character(length = 0)

  if (is.null(options()$knitr.in.progress)) {
    tx[length(tx)+1] <- paste("-- Association and Effect Size")
    tx[length(tx)+1] <- ""
  }

  rsq <- ssb / sst
  tx[length(tx)+1] <- paste("R Squared:", .fmt(rsq, 3))
  rsq.adj <-  1 - ( ((n.obs-1)/(n.obs-p)) * (1-rsq) )
  tx[length(tx)+1] <- paste("R Sq Adjusted:", .fmt(rsq.adj, 3))
  omsq <- (ssb - ((p-1)*msw)) / ((ssb + p*(mean(n)-1)*msw) + msw)
  tx[length(tx)+1] <- paste("Omega Squared:", .fmt(omsq, 3))
  if (omsq > 0) {
    tx[length(tx)+1] <- ""
    f.cohen <- sqrt( (omsq/(1-omsq)) )
    tx[length(tx)+1] <- paste("Cohen's f:", .fmt(f.cohen, 3))
  }

  txeft <- tx

  # keep track of the number of plots for 2 windows, see if manage graphics
  if (graphics) {
      plt.i <- 0
      plt.title  <- character(length=0)
      manage.gr <- .graphman()
  }

  txhsd <- ""
  title_tukey <- "\n  TUKEY MULTIPLE COMPARISONS OF MEANS"
  if (!brief) {
    tx <- character(length = 0)

    HSD <- TukeyHSD(av.out)
    HSD <- TukeyHSD(av.out, which=nm[2])
    tx[length(tx)+1] <- paste("Family-wise Confidence Level:",
                              attributes(HSD)$conf.level)
    txHSD <- .prntbl(HSD[[1]], digits_d)
    for (i in 1:length(txHSD)) tx[length(tx)+1] <- txHSD[i]

    txhsd <- tx


    if (graphics) {

      # get left margin size
      mnc <- max(nchar(levels(x.values)))  # 1st highest nchar
      wm.ind <- which.max(nchar(levels(x.values)))
      xv <- levels(x.values)[-wm.ind]
      mnc.xv <- max(nchar(xv))  # 2nd highest nchar
      tot <- mnc + mnc.xv
      lm <- 3.2 + (0.27 * tot)
      if (lm < 2.8) lm <- 2.8

      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))
      par(mar=c(5.1,lm,4.1,1.5))

      if (!pdf) { 
        if (manage.gr) dev.set(which=4) 
      }
      else { 
        pdf_file <- "ANOVA_HSD.pdf"
        pdf(file=pdf_file, width=width, height=height)
      }

      plt.i <- plt.i + 1
      plt.title[plt.i] <- "95% family-wise confidence level"

      plot(HSD, cex.axis=0.8, col.axis="gray30", las=1)

      if (pdf) {
        dev.off()
        .showfile(pdf_file, "Tukey HSD chart")
        tx[length(tx)+1] <- ""
      }
    }  # end graphics
  }  # !brief

  # scatterplot with cell means
  if (graphics) {
    if (!pdf) {
      if (manage.gr) {
        if (!brief) .graphwin(2) else .graphwin(1)
        dev.set(which=3)
      }
    }
    else { 
      pdf_file <- "ANOVA_Means.pdf"
      pdf(file=pdf_file, width=width, height=height)
    }

    plt.i <- plt.i + 1
    plt.title[plt.i] <- "Scatterplot with Cell Means"

    # set margins
    max.width <- strwidth(as.character(max(pretty(y.values))), units="inches")
    margs <- .marg(max.width, y.lab=nm[1], x.lab=nm[2], main=NULL, sub=NULL)
    lm <- margs$lm
    tm <- margs$tm
    rm <- margs$rm
    bm <- margs$bm
    
    par(bg=getOption("window_fill"))
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    par(mai=c(bm, lm, tm, rm))

    # scatter plot
    plot(x.values, y.values, type="n", axes=FALSE, ann=FALSE)

    usr <- par("usr")
    col.bg <- getOption("panel_fill")
    rect(usr[1], usr[3], usr[2], usr[4],
         col=getOption("panel_fill"), border=getOption("panel_color"))

    axT1 <- 1:length(unique(x.values))
    .axes(levels(x.values), NULL, axT1, axTicks(2))
    .grid("v", axT1)
    .grid("h", axTicks(2))

    main.lab <- plt.title[plt.i]  # not used
    x.label <- nm[2]
    y.label <- nm[1]
    .axlabs(x.label, y.label, main.lab=NULL, sub.lab=NULL,
            xlab_adj=0.4, ylab_adj=.05) 

    col_fill <- getOption("pt_fill")
    col_color <- getOption("pt_color")
    xn.values <-  data.frame(  # factor to integer for jitter
        levels(x.values), 1:length(levels(x.values)), row.names = 1)[x.values, 1]
    xn.values <- jitter(xn.values, factor=jitter_x)
    points(xn.values, y.values, pch=21, col=col_color, bg=col_fill, cex=0.7)

    # plot cell means
    pch.avg <- ifelse(getOption("theme")!="gray", 21, 23)
    bck.g <- ifelse(getOption("theme")!="gray", rgb(140,90,70,
                    maxColorValue=255), "gray40")
    if (grepl(".black", getOption("theme"), fixed=TRUE)) bck.g <- "gray85"
    m.lvl <- numeric(length = 0)
    for (i in (1:length(levels(x.values))))
      m.lvl[i] <- mean(y.values[which(x.values==levels(x.values)[i])],
                       na.rm=TRUE)
    abline(h=m.lvl, col="gray50", lwd=.5)
    points(m.lvl, pch=pch.avg, bg=bck.g, col="transparent", cex=1.3)

    if (pdf) {
      dev.off()
      .showfile(pdf_file, "scatterplot and means chart")
    }
  }  # graphics scatterplot


  return(list(
    title_des=title_des, txdes=txdes,
    title_basic=title_basic,
    title_tukey=title_tukey,
    txanv=txanv, txeft=txeft, txhsd=txhsd,
    i=plt.i, ttl=plt.title))

}
