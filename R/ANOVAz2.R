.ANOVAz2 <- 
function(av.out, y.values, x1.values, x2.values, nm, digits_d, brief,
         delim, balance, graphics, pdf, width, height) {

  if (grepl("*", delim, fixed=TRUE)) bet.grp  <- TRUE else bet.grp <- FALSE
  if (grepl("+", delim, fixed=TRUE)) wth.grp  <- TRUE else wth.grp <- FALSE

  p <- length(unique(na.omit(x1.values)))
  if (bet.grp)
    q <- length(unique(na.omit(x2.values)))
  if (wth.grp) {
    n <- length(unique(na.omit(x2.values)))
    q <- 1
  }


  tx <- character(length = 0)

  tx[length(tx)+1] <- "" 
  if (bet.grp)
    tx[length(tx)+1] <- "Two-way Between Groups ANOVA"
  else if (wth.grp) {
    tx[length(tx)+1] <- "Randomized Blocks ANOVA"
    tx[length(tx)+1] <- paste("  Factor of Interest: ", nm[2])
    tx[length(tx)+1] <- paste("  Blocking Factor:    ", nm[3])
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste(
        "Note: For the resulting F statistic for", nm[2], 
        "to be distributed as F,\n",
        "     the population covariances of", nm[1], "must be spherical.")
  }

  txbck2 <- tx


  title_des <- "\n  DESCRIPTIVE STATISTICS "

  txcn <- ""
  txcm <- ""
  if (bet.grp) {

    n <-  tapply(y.values, 
          list(x1.values, x2.values), length)
    n <- as.table(n)

    m <-  tapply(y.values, 
          list(x1.values, x2.values), mean, na.rm=TRUE)
    m <- as.table(m)

    if (!brief) {

      tx <- character(length = 0)
      if (is.null(options()$knitr.in.progress))
        tx[length(tx)+1] <- "-- Cell Sample Sizes"
      tx[length(tx)+1] <- ""
      if (balance) 
        tx[length(tx)+1] <- "Equal cell sizes, so balanced design"
      else {
        tx[length(tx)+1] <- "Unequal cell sizes, so unbalanced design" 
        tx[length(tx)+1] <- "ANOVA based on Type II Sums of Squares" 
      }
      tx[length(tx)+1] <- ""
      tx2 <- .prntbl(t(n), digits_d, cc=" ", v1.nm=nm[2], v2.nm=nm[3])
      for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
      txcn <- tx

      tx <- character(length = 0)
      if (is.null(options()$knitr.in.progress)) {
        tx[length(tx)+1] <- "-- Cell Means"
      }
      tx2 <- .prntbl(t(m), digits_d, cc=" ", v1.nm=nm[2], v2.nm=nm[3])
      for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
      txcm <- tx

    }  # !brief
  }  # bet.grp

  else {  # get means, a 2-d matrix, then table
    m <-  tapply(y.values, 
                 list(x1.values, x2.values), mean, na.rm=TRUE)
    m <- as.table(m)
  }

  # convert table of means to long form
  tx <- character(length = 0)
  if (is.null(options()$knitr.in.progress)) {
    tx[length(tx)+1] <- "-- Marginal Means"
    tx[length(tx)+1] <- ""
  }

  m <- data.frame(m)
  names(m) <- c(nm[2], nm[3], nm[1])

  tx[length(tx)+1] <- nm[2]
  m1 <- tapply(y.values, x1.values, mean, na.rm=TRUE)
  m1 <- data.frame(t(m1))
  tx2 <- .prntbl(m1, digits_d, cc= " ")  # 1st treatment horizontal dimension
  for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- nm[3]
  m2 <-  tapply(y.values, x2.values, mean, na.rm=TRUE)
  m2 <- data.frame(t(m2))
  tx2 <- .prntbl(m2, digits_d, cc= " ")  # 2nd treatment horizontal dimension
  for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
  txmm <- tx

  tx <- character(length = 0)
  mg <- mean(y.values, na.rm=TRUE)
  if (is.null(options()$knitr.in.progress)) {
    tx[length(tx)+1] <- paste("-- Grand Mean:", round(mg, digits_d+1))
  }
  txgm <- tx

  tx <- character(length = 0)
  if (is.null(options()$knitr.in.progress)) {
    tx[length(tx)+1] <- "-- Cell Standard Deviations"
  }

  txcs <- ""
  if (bet.grp) {
    s <-  tapply(y.values, 
                 list(x1.values, x2.values), sd, na.rm=TRUE)
    s <- as.table(s)
    tx2 <- .prntbl(t(s), digits_d, cc=" ", v1.nm=nm[2], v2.nm=nm[3])
    for (i in 1:length(tx2)) tx[length(tx)+1] <- tx2[i]
    txcs <- tx
  }  # end between groups



  title_basic <- "\n  ANOVA"

  if(bet.grp) n.vars <- 4
  if(wth.grp) n.vars <- 3
  smc <- anova(av.out)

  tx <- character(length = 0)

  if (is.null(options()$knitr.in.progress)) {
    tx[length(tx)+1] <- paste("-- Summary Table")

    if (!balance) {
       tx[length(tx)] <-  paste(tx[length(tx)],
                                "from Type II Sums of Squares")
       av2.out <- aov(y.values ~ x2.values * x1.values)
       smc[1,] <- anova(av2.out)[2,]
    }
  }
  tx[length(tx)+1] <- ""

  buf <- 0 
  for (i in 1:n.vars) {
    lng.lbl <- nchar(rownames(smc)[i])
    if (lng.lbl > buf) buf <- lng.lbl 
   }
  max.num <- integer(length=0)
  for (icol in 1:3) {
    max.num[icol] <- 0 
    for (i in 1:n.vars) {
      ln.nm <- nchar(as.character(trunc(smc[i,icol]))) + digits_d + 2
      if (ln.nm > max.num[icol]) max.num[icol] <- ln.nm
    }
    if (icol != 1) if (max.num[icol] < 9L) max.num[icol] <- 9L 
  }

  df.lbl <- .fmtc("     df", max.num[1]+2)
  SS.lbl <- .fmtc(" Sum Sq", max.num[2]+1)
  MS.lbl <- .fmtc("Mean Sq", max.num[3]+1)
  if (bet.grp) 
    max.nm <- max(nchar(nm[2])+nchar(nm[3])+1, nchar("Residuals"))
  else
    max.nm <- max(nchar(nm[2]), nchar(nm[3]), nchar("Residuals"))
    
  tx[length(tx)+1] <- paste(format("", width=max.nm-5), df.lbl, SS.lbl, MS.lbl,
                                     "   F-value", "   p-value", sep="")
  for (i in 1:(n.vars)) {
    rlb <- .fmtc(rownames(smc)[i], buf)
    df <- format(sprintf("%i", smc[i,1]), width=max.num[1]-4, justify="right")
    SS <- format(sprintf("%7.*f", digits_d, smc[i,2]), width=max.num[2],
                 justify="right")
    MS <- format(sprintf("%7.*f", digits_d, smc[i,3]), width=max.num[3],
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

  txanv <- tx


  tx <- character(length = 0)

  if (is.null(options()$knitr.in.progress)) {
    tx[length(tx)+1] <- paste("-- Association and Effect Size")
    tx[length(tx)+1] <- ""
  }

  sm <- summary(av.out)
  msA <- sm[[1]][1,3]
  msB <- sm[[1]][2,3]
  msAB <- sm[[1]][3,3]
  msE <- sm[[1]][4,3]
  FA <- sm[[1]][1,4]
  FB <- sm[[1]][2,4]
  if (bet.grp) {
    FAB <- sm[[1]][3,4]
    n <- n[1,1]
  }

  omsq.A <- ((p-1)*(FA-1)) / ((p-1)*(FA-1) + n*p*q)
  if (bet.grp) omsq.B <- ((q-1)*(FB-1)) / ((q-1)*(FB-1) + n*p*q)
  if (wth.grp) intra.B <- (FB-1) / ((p-1)+(FB))
  if (bet.grp) omsq.AB <- ((p-1)*(q-1)*(FAB-1)) / ((p-1)*(q-1)*(FAB-1) + n*p*q)

  tx[length(tx)+1] <- paste("Partial Omega Squared for ", nm[2],
      ": ", .fmt(omsq.A, 3), sep="")
  if (bet.grp)
    tx[length(tx)+1] <- paste("Partial Omega Squared for ", nm[3],
        ": ", .fmt(omsq.B, 3), sep="")
  if (wth.grp)
    tx[length(tx)+1] <- paste("Partial Intraclass Correlation for ", nm[3],
        ": ", .fmt(intra.B, 3), sep="")
  if (bet.grp)
    tx[length(tx)+1] <- paste("Partial Omega Squared for ", nm[2], " & ", nm[3],
        ": ", .fmt(omsq.AB, 3), sep="")
  tx[length(tx)+1] <- ""

  if (omsq.A > 0) {
    fA.cohen <- sqrt( (omsq.A/(1-omsq.A)) )
    tx[length(tx)+1] <- paste("Cohen's f for ", nm[2], ": ",
                              .fmt(fA.cohen, 3), sep="")
  }
  if (bet.grp) {
    if(omsq.B > 0) {
      fB.cohen <- sqrt( (omsq.B/(1-omsq.B)) )
      tx[length(tx)+1] <- paste("Cohen's f for ", nm[3], ": ",
                                .fmt(fB.cohen, 3), sep="")
    }
    if (omsq.AB > 0) {
      fAB.cohen <- sqrt( (omsq.AB/(1-omsq.AB)) )
      tx[length(tx)+1] <- paste("Cohen's f for ", nm[2], "_&_", nm[3], ": ",
                                 .fmt(fAB.cohen, 3), sep="")
    }
  }
  if (wth.grp) {
    if (intra.B > 0) {
      fB.cohen <- sqrt( (intra.B/(1-intra.B)) )
      tx[length(tx)+1] <- paste("Cohen's f for ", nm[3], ": ",
                                .fmt(fB.cohen, 3), sep="")
    }
  }

  txeft <- tx


  txhsd <- ""
  title_tukey <- "\n TUKEY MULTIPLE COMPARISONS OF MEANS"
  if (!brief) {
    tx <- character(length = 0)

    HSD <- TukeyHSD(av.out)
    tx[length(tx)+1] <- paste("Family-wise Confidence Level:", 
                              attributes(HSD)$conf.level)
    tx[length(tx)+1] <- paste("\nFactor:", nm[2])

    txHSD <- .prntbl(HSD[[1]], digits_d)
    for (i in 1:length(txHSD)) tx[length(tx)+1] <- txHSD[i]

    if (bet.grp) {
      tx[length(tx)+1] <- paste("\nFactor:", nm[3])
      txHSD <- .prntbl(HSD[[2]], digits_d)  # second factor
      for (i in 1:length(txHSD)) tx[length(tx)+1] <- txHSD[i]
      tx[length(tx)+1] <- paste("\nCell Means")
      txHSD <- .prntbl(HSD[[3]], digits_d)  # interaction
      for (i in 1:length(txHSD)) tx[length(tx)+1] <- txHSD[i]
    }

    txhsd <- tx
  }


  # ------------------------------------
  # visualizations

  # keep track of the number of plots, see if manage graphics
  plt.i <- 0
  plt.title  <- character(length=0)
  if (graphics) {
    manage.gr <- .graphman()

    if (bet.grp) {

      # interaction plots
      if (!pdf) {
        if (manage.gr) {
          if (bet.grp) .graphwin(1)
          dev.set(which=2)
        }
      }
      else { 
        pdf_file <- "ANOVA_Interaction.pdf"
        pdf(file=pdf_file, width=width, height=height)
      }

      plt.i <- plt.i + 1
      plt.title[plt.i] <- "Interaction Plot"

      # p is num of levels on x-axis
      # q is num of levels for legend, each curve
      options(byname = nm[3])
      theme <- getOption("theme")
#     qual_pal <- ifelse (theme %in% c("gray", "white"), "grays", "hues")
#     pt_fill <- getColors(qual_pal, n=q, output=FALSE)
      pt_fill <- .color_range(.get_fill(), q)  # see if range
      txt <- paste("Cell Means of", nm[1])
      .plt.main(m[,1,drop=FALSE], m[,3,drop=FALSE], by=m[,2], 
                fill=pt_fill, color=pt_fill, segments=TRUE,
                col.segment=pt_fill, cat.x=TRUE, xlab=nm[2], ylab=txt, size=1.5)

      if (pdf) {
        dev.off()
        .showfile(pdf_file, "interaction plot")
      }
    }  # end bet.grp

    # fitted plots
    if (wth.grp) {

      fit.out <- data.frame(av.out$fitted.values)
      names(fit.out) <- "Fitted"
      m <- cbind(av.out$model, fit.out)
      nm <- names(m)
      options(byname = nm[3])
      options(yname = nm[1])

      # -----------

  # if manage, set up graphics system for 2 windows default
  if (!pdf) {
    if (manage.gr) {
#     .graphwin(2)
      dev.set(which=2)
    }
  }
  else { 
    pdf_file <- "ANOVA_Data.pdf"
    pdf(file=pdf_file, width=width, height=height)
  }

      plt.i <- plt.i + 1
      plt.title[plt.i] <- "Data Values"

      # .plt.main replacement for interaction.plot of data not quite working
      interaction.plot(x1.values, x2.values, m[,1],
                       xlab=nm[2], ylab=nm[1], trace.label=nm[3])
#     .plt.main(m[,2,drop=FALSE], m[,1,drop=FALSE], by=m[,3], segments=TRUE,
#               size=0, cat.x=TRUE, xlab=nm[2], ylab=nm[1])

      if (pdf) {
        dev.off()
        .showfile(pdf_file, "Data values plot")
      }

      # -----------

  if (!pdf) {
    if (manage.gr) {
#     .graphwin(2)
      dev.set(which=3)
    }
  }
  else { 
    pdf_file <- "ANOVA_Fitted.pdf"
    pdf(file=pdf_file, width=width, height=height)
  }

      plt.i <- plt.i + 1
      plt.title[plt.i] <- "Fitted Values"

      .plt.main(m[,2,drop=FALSE], m[,4,drop=FALSE], by=m[,3], segments=TRUE,
              cat.x=TRUE, xlab=nm[2], ylab=nm[4])

      # pdf
      if (pdf) {
        dev.off()
        .showfile(pdf_file, "Fitted values plot")
      }

    }  # end within group

  }  # end grahpics

  return(list(
    txbck2=txbck2, 
    title_des=title_des, txcn=txcn, txcm=txcm, txmm=txmm, txgm=txgm, txcs=txcs,
    title_basic=title_basic,
    title_tukey=title_tukey,
    txanv=txanv, txeft=txeft, txhsd=txhsd,
    i=plt.i, ttl=plt.title))

} 
