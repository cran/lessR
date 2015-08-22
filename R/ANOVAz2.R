.ANOVAz2 <- 
function(av.out, y.values, x1.values, x2.values, nm, digits.d, brief,
         delim, rb.points, pdf, pdf.width, pdf.height) {

  if (grepl("*", delim, fixed=TRUE)) bet.grp  <- TRUE else bet.grp <- FALSE
  if (grepl("+", delim, fixed=TRUE)) wth.grp  <- TRUE else wth.grp <- FALSE

  cat("\n")
  if (bet.grp)
    cat("Two-way Between Groups ANOVA\n")
  else if (wth.grp) {
    cat("Randomized Blocks ANOVA\n")
    cat("  Factor of Interest: ", nm[2], "\n")
    cat("  Blocking Factor:    ", nm[3], "\n")
    cat("\n")
    cat("Note: For the resulting F statistic for", nm[2], "to be distributed as F,\n",
        "     the population covariances of", nm[1], "must be spherical.\n")
  }

  p <- length(unique(na.omit(x1.values)))
  if (bet.grp)
    q <- length(unique(na.omit(x2.values)))
  if (wth.grp) {
    n <- length(unique(na.omit(x2.values)))
    q <- 1
  }

  if (bet.grp) {
    l <-  tapply(y.values, 
          list(x1.values, x2.values), length)
    l <- as.table(l)

    if (!brief) {
      cat("\nCell Sample Size:", l[1,1], "\n")

      cat("\n\nCell Means\n")
      .dash(10)
      m <-  tapply(y.values, 
            list(x1.values, x2.values ), mean, na.rm=TRUE)
      m <- as.table(m)
      names(dimnames(m)) <- c(nm[2], nm[3])
      print(round(t(m), digits.d))  # first treatment horizontal dimension
    }
  }

    cat("\n\nMarginal Means\n")
    .dash(14)
    cat(nm[2], "\n")
    m1 <-  tapply(y.values, x1.values, mean, na.rm=TRUE)
    print(round(m1, digits.d))  # first treatment horizontal dimension
    cat("\n")
    cat(nm[3], "\n")
    m2 <-  tapply(y.values, x2.values, mean, na.rm=TRUE)
    print(round(m2, digits.d))  # first treatment horizontal dimension

    cat("\n\nGrand Mean\n")
    .dash(10)
    mg <-  mean(y.values, na.rm=TRUE)
    cat(round(mg, digits.d+1), "\n")

  if (bet.grp) {
    cat("\n")
    cat("\nCell Standard Deviations\n")
    .dash(24)
    s <-  tapply(y.values, 
          list(x1.values, x2.values ), sd, na.rm=TRUE)
    s <- as.table(s)
    names(dimnames(s)) <- c(nm[2], nm[3])
    print(round(t(s), digits.d))
  }  # end between groups


  cat("\n\n\n")
  cat("Analysis of Variance\n")
  .dash(20)
  if(bet.grp) n.vars <- 4
  if(wth.grp) n.vars <- 3
  smc <- anova(av.out)
  buf <- 0 
  for (i in 1:n.vars) {
    lng.lbl <- nchar(rownames(smc)[i])
    if (lng.lbl > buf) buf <- lng.lbl 
   }
  max.num <- integer(length=0)
  for (icol in 1:3) {
    max.num[icol] <- 0 
    for (i in 1:n.vars) {
      ln.nm <- nchar(as.character(trunc(smc[i,icol]))) + digits.d + 2
      if (ln.nm > max.num[icol]) max.num[icol] <- ln.nm
    }
    if (icol != 1) if (max.num[icol] < 9) max.num[icol] <- 9 
  }
  df.lbl <- .fmtc("     df", max.num[1]+2)
  SS.lbl <- .fmtc(" Sum Sq", max.num[2]+1)
  MS.lbl <- .fmtc("Mean Sq", max.num[3]+1)
  cat(rep(" ", buf-5), df.lbl, SS.lbl, MS.lbl, "   F-value", "   p-value", sep="", "\n")
  for (i in 1:(n.vars)) {
    rlb <- .fmtc(rownames(smc)[i], buf)
    df <- format(sprintf("%i", smc[i,1]), width=max.num[1]-4, justify="right")
    SS <- format(sprintf("%7.*f", digits.d, smc[i,2]), width=max.num[2], justify="right")
    MS <- format(sprintf("%7.*f", digits.d, smc[i,3]), width=max.num[3], justify="right")
    if (i < n.vars) {
      fv <- format(sprintf("%7.*f", digits.d, smc[i,4]), width=9, justify="right")
      pv <- format(sprintf("%6.4f", smc[i,5]), width=9, justify="right")
      cat(rlb, df, SS, MS, fv, pv, "\n") 
    }
    else
      cat(rlb, df, SS, MS, "\n") 
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
    n <- l[1,1]
  }

  omsq.A <- ((p-1)*(FA-1)) / ((p-1)*(FA-1) + n*p*q)
  if (bet.grp) omsq.B <- ((q-1)*(FB-1)) / ((q-1)*(FB-1) + n*p*q)
  if (wth.grp) intra.B <- (FB-1) / ((p-1)+(FB))
  if (bet.grp) omsq.AB <- ((p-1)*(q-1)*(FAB-1)) / ((p-1)*(q-1)*(FAB-1) + n*p*q)

  cat("\n\n\n")
  cat("Effect Size\n")
  .dash(11)
  cat("Partial Omega Squared for ", nm[2],
      ": ", .fmt(omsq.A, 2), sep="", "\n")
  if (bet.grp)
    cat("Partial Omega Squared for ", nm[3],
        ": ", .fmt(omsq.B, 2), sep="", "\n")
  if (wth.grp)
    cat("Partial Intraclass Correlation for ", nm[3],
        ": ", .fmt(intra.B, 2), sep="", "\n")
  if (bet.grp)
    cat("Partial Omega Squared for ", nm[2], "_&_", nm[3],
        ": ", .fmt(omsq.AB, 2), sep="", "\n")
  cat("\n")

  if (omsq.A > 0) {
    fA.cohen <- sqrt( (omsq.A/(1-omsq.A)) )
    cat("Cohen's f for ", nm[2], ": ", .fmt(fA.cohen, 2), sep="", "\n")
  }
  if (bet.grp) {
    if(omsq.B > 0) {
      fB.cohen <- sqrt( (omsq.B/(1-omsq.B)) )
      cat("Cohen's f for ", nm[3], ": ", .fmt(fB.cohen, 2), sep="", "\n")
    }
    if (omsq.AB > 0) {
      fAB.cohen <- sqrt( (omsq.AB/(1-omsq.AB)) )
      cat("Cohen's f for ", nm[2], "_&_", nm[3], ": ", .fmt(fAB.cohen, 2), sep="", "\n")
    }
  }
  if (wth.grp) {
    if (intra.B > 0) {
      fB.cohen <- sqrt( (intra.B/(1-intra.B)) )
      cat("Cohen's f for ", nm[3], ": ", .fmt(fB.cohen, 2), sep="", "\n")
    }
  }

  if (!brief) {
    cat("\n\n\n")
    HSD <- TukeyHSD(av.out)
    cat("Tukey Multiple Comparisons of Means\n")
    cat("Family-wise Confidence Level:", attr(HSD, which="conf.level"), "\n")
    .dash(35)
    cat("\nFactor:", nm[2], "\n")
    .dash(11)
    print(round(HSD[[1]], digits.d))  # first factor
    if (bet.grp) {
      cat("\nFactor:", nm[3], "\n")
      .dash(11)
      print(round(HSD[[2]], digits.d))  # second factor
      cat("\nCell Means\n")
      .dash(10)
      print(round(HSD[[3]], digits.d))  # interaction
    }
  }

  # ------------------------------------
  # graphics
  plt.i <- 0
  plt.title  <- character(length=0)
  manage.gr <- .graphman()

  # interaction plots
  if (!pdf) {
    if (manage.gr) {
      pdf.file <- NULL
      if (bet.grp) .graphwin(1)
      if (wth.grp) .graphwin(2)
      dev.set(which=3)
    }
  }
  else { 
    pdf.file <- "ANOVA_Interaction.pdf"
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }

  plt.i <- plt.i + 1
  plt.title[plt.i] <- "Interaction Plot"

  interaction.plot(x1.values, x2.values, y.values,
                   xlab=nm[2], ylab=nm[1], trace.label=nm[3],
                   main=plt.title[plt.i])

  # pdf
  if (pdf) {
    dev.off()
    .showfile(pdf.file, "interaction plot")
  }

  # fitted interaction plots
  if (wth.grp) {

    if (!pdf) {
      if (manage.gr) {
        dev.set(which=4)
      }
    }
    else { 
      pdf.file <- "ANOVA_FitInter.pdf"
      pdf(file=pdf.file, width=pdf.width, height=pdf.height)
    }

  plt.i <- plt.i + 1
  plt.title[plt.i] <- "Fitted Values"

    mn.y <- min(y.values, av.out$fitted)
    mx.y <- max(y.values, av.out$fitted)
    interaction.plot(x1.values, x2.values, 
             av.out$fitted, main=plt.title[plt.i],
             xlab=nm[2], ylab=nm[1], trace.label=nm[3], ylim=c(mn.y, mx.y))
    if (rb.points) {
      points(x1.values, y.values, pch=21, 
             bg=rgb(.6, .6, .6, alpha=getOption("trans.pts"), maxColorValue = 1))
      segments(as.numeric(x1.values), av.out$fitted, as.numeric(x1.values), y.values)
    }

    # pdf
    if (pdf) {
      dev.off()
      .showfile(pdf.file, "fitted values plot")
    }
  }

  return(list(i=plt.i, ttl=plt.title))
} 

