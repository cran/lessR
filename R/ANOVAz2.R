.ANOVAz2 <- 
function(y.values, x1.values, x2.values, nm, digits.d, brief,
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
          list(x1.values, x2.values ), length)
    l <- as.table(l)

    if (!brief) {
      cat("\n")
      cat("\nCell Sample Sizes\n")
      .dash(17)
      names(dimnames(l)) <- c(nm[2], nm[3])
      print(t(l))  # first treatment horizontal dimension

      cat("\nCell Means\n")
      .dash(10)
      m <-  tapply(y.values, 
            list(x1.values, x2.values ), mean, na.rm=TRUE)
      m <- as.table(m)
      names(dimnames(m)) <- c(nm[2], nm[3])
      print(round(t(m), digits.d))  # first treatment horizontal dimension

      cat("\nMarginal Means\n")
      .dash(14)
      cat(nm[2], "\n")
      m1 <-  tapply(y.values, x1.values, mean, na.rm=TRUE)
      print(round(m1, digits.d))  # first treatment horizontal dimension
      cat("\n")
      cat(nm[3], "\n")
      m2 <-  tapply(y.values, x2.values, mean, na.rm=TRUE)
      print(round(m2, digits.d))  # first treatment horizontal dimension

      cat("\nGrand Mean\n")
      .dash(10)
      mg <-  mean(y.values, na.rm=TRUE)
      cat(round(mg, digits.d+1), "\n")

      cat("\nCell Standard Deviations\n")
      .dash(24)
      s <-  tapply(y.values, 
            list(x1.values, x2.values ), sd, na.rm=TRUE)
      s <- as.table(s)
      names(dimnames(s)) <- c(nm[2], nm[3])
      print(round(t(s), digits.d))
    }
  }  # end between groups


  cat("\n\n\n")
  cat("ANOVA\n")
  .dash(5)
  sm <- summary(av.out)
  print(sm)


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
      cat("\nInteraction\n")
      .dash(11)
      print(round(HSD[[3]], digits.d))  # interaction
    }
  }

  # interaction plots
  if (!pdf) {
    pdf.file <- NULL
    if (bet.grp) .graphwin(1)
    if (wth.grp) .graphwin(2)
    dev.set(which=3)
  }
  else { 
    pdf.file <- "ANOVA_Interaction.pdf"
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }

  interaction.plot(x1.values, x2.values, 
           y.values, main="Interaction Plot",
           xlab=nm[2], ylab=nm[1], trace.label=nm[3])

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "interaction plot")
  }

  # fitted interaction plots
  if (wth.grp) {
    if (!pdf) {
      dev.set(which=4)
    }
    else { 
      pdf.file <- "ANOVA_FitInter.pdf"
      pdf(file=pdf.file, width=pdf.width, height=pdf.height)
    }

    mn.y <- min(y.values, av.out$fitted)
    mx.y <- max(y.values, av.out$fitted)
    interaction.plot(x1.values, x2.values, 
             av.out$fitted, main="Plot of Fitted Values",
             xlab=nm[2], ylab=nm[1], trace.label=nm[3], ylim=c(mn.y, mx.y))
    if (rb.points) {
      points(x1.values, y.values, pch=21, 
             bg=rgb(.6, .6, .6, alpha=getOption("trans.pts"), maxColorValue = 1))
      segments(as.numeric(x1.values), av.out$fitted, as.numeric(x1.values), y.values)
    }
  }

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "interaction plot")
  }

} 
