.ANOVAz1 <- 
function(av.out, y.values, x.values, nm, n.obs, digits.d, brief,
         pdf, pdf.width, pdf.height) {

  p <- length(unique(na.omit(x.values)))

    # cell stats
    n <- tapply(y.values, x.values, length) 
    m <- tapply(y.values, x.values, mean) 
    s <- tapply(y.values, x.values, sd) 
    mn <- tapply(y.values, x.values, min) 
    mx <- tapply(y.values, x.values, max) 

  if (!brief) {
    # get maximum chars in 1st three columns
    max.lv <- 0; max.n <- 0; max.m <- 0; max.s <- 0; max.mn <- 0; max.mx <- 0
    for (i in 1:p) {
      nch.lv <- nchar(as.character(levels(x.values)[i]))
      nch.n <- nchar(as.character(n[i]))
      nch.m <- nchar(.fmt(m[i],digits.d))
      nch.s <- nchar(.fmt(s[i],digits.d))
      nch.mn <- nchar(.fmt(mn[i],digits.d))
      nch.mx <- nchar(.fmt(mx[i],digits.d))
      if (nch.lv > max.lv) max.lv <- nch.lv
      if (nch.n > max.n) max.n <- nch.n
      if (nch.m > max.m) max.m <- nch.m
      if (nch.s > max.s) max.s <- nch.s
      if (nch.mn > max.mn) max.mn <- nch.mn
      if (nch.mx > max.mx) max.mx <- nch.mx
    }

    cat("\n\n\n")
    cat("Descriptive Statistics\n")
    .dash(22)
    cat(format("n", width=max.lv+max.n+2, justify="right", sep=""))
    cat(format("mean", width=max.m+3, justify="right", sep=""))
    cat(format("sd", width=max.s+3, justify="right", sep=""))
    cat(format("min", width=max.mn+3, justify="right", sep=""))
    cat(format("max", width=max.mx+3, justify="right", sep=""))
    cat("\n")
    for (i in 1:p) {
      cat(format(levels(x.values)[i], width=max.lv, justify="left", sep=""), 
        format(n[i], width=max.n+1, justify="right", sep=""), 
        format(sprintf("%.*f", digits.d, m[i]), width=max.m+2, justify="right"), 
        format(sprintf("%.*f", digits.d, s[i]), width=max.s+2, justify="right"), 
        format(sprintf("%.*f", digits.d, mn[i]), width=max.mn+2, justify="right"), 
        format(sprintf("%.*f", digits.d, mx[i]), width=max.mx+2, justify="right"), 
        "\n")
    }

      mg <-  mean(y.values, na.rm=TRUE)
      cat("\nGrand Mean:", round(mg, digits.d+1), "\n")
  }  # end !brief   


  # set up graphics system for 2 windows
  if (!pdf) {
    .graphwin(2)
    dev.set(which=3)
  }
  else { 
    pdf.file <- "ANOVA_Means.pdf"
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }

  .plt.main(x.values, y.values, by=NULL, type="p", n.cat=0,
     col.fill=getOption("col.fill.pt"),
     col.stroke=getOption("col.stroke.pt"),
     col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),
     shape.pts="circle", col.area=NULL, col.box="black",
     cex.axis=.85, col.axis="gray30",
     col.ticks="gray30", xy.ticks=TRUE,
     xlab=nm[2], ylab=nm[1], main="",
     cex=.8, kind="default",
     fit.line="none", col.fit.line="grey55", bubble.size=.25,
     ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE,
     diag=FALSE, col.diag=par("fg"), lines.diag=TRUE, quiet=TRUE)

  if (pdf) {
    dev.off()
    .showfile(pdf.file, "means chart")
  }


  cat("\n\n\n")
  cat("Analysis of Variance\n")
  .dash(20)
  n.vars <- 2
  smc <- anova(av.out)
  buf <- 0 
  for (i in 1:n.vars) {
    lng.lbl <- nchar(rownames(smc)[i])
    if (lng.lbl > buf) buf <- lng.lbl 
   }
  max.num <- integer(length=0)
  for (icol in 1:4) {
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
  fv.lbl <- .fmtc("F-value", max.num[4]+1)
  cat(rep(" ", buf-5), df.lbl, SS.lbl, MS.lbl, fv.lbl, "   p-value", sep="", "\n")
  for (i in 1:(n.vars)) {
    rlb <- .fmtc(rownames(smc)[i], buf)
    df <- format(sprintf("%i", smc[i,1]), width=max.num[1]-4, justify="right")
    SS <- format(sprintf("%7.*f", digits.d, smc[i,2]), width=max.num[2], justify="right")
    MS <- format(sprintf("%7.*f", digits.d, smc[i,3]), width=max.num[3], justify="right")
    fv <- format(sprintf("%7.*f", digits.d, smc[i,4]), width=max.num[4], justify="right")
    pv <- format(sprintf("%6.4f", smc[i,5]), width=9, justify="right")
    if (i < n.vars) cat(rlb, df, SS, MS, fv, pv, "\n") else cat(rlb, df, SS, MS, "\n") 
  }

  sm <- summary(av.out)
  ssb <- sm[[1]][1,2]
  ssw <- sm[[1]][2,2]
  sst <- ssb + ssw
  msw <- sm[[1]][2,3]

  cat("\n\n\n")
  cat("Association and Effect Size\n")
  .dash(27)
  rsq <- ssb / sst
  cat("R Squared:", .fmt(rsq, 2), "\n")
  rsq.adj <-  1 - ( ((n.obs-1)/(n.obs-p)) * (1-rsq) )
  cat("R Sq Adjusted:", .fmt(rsq.adj, 2), "\n")
  omsq <- (ssb - ((p-1)*msw)) / ((ssb + p*(mean(n)-1)*msw) + msw)
  cat("Omega Squared:", .fmt(omsq, 2), "\n")
  if (omsq > 0) {
    cat("\n")
    f.cohen <- sqrt( (omsq/(1-omsq)) )
    cat("Cohen's f:", .fmt(f.cohen, 2), "\n")
  }

  if (!brief) {
    cat("\n\n\n")
    HSD <- TukeyHSD(av.out)
    HSD <- TukeyHSD(av.out, which=nm[2])
    cat("Tukey Multiple Comparisons of Means\n")
    cat("Family-wise Confidence Level:", attr(HSD, which="conf.level"), "\n")
    .dash(35)
    print(round(HSD[[1]], digits.d))

    if (!pdf) 
      dev.set(which=4) 
    else { 
      pdf.file <- "ANOVA_HSD.pdf"
      pdf(file=pdf.file, width=pdf.width, height=pdf.height)
    }
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    par(mar=c(5.1,6.1,4.1,1.5))
    cex.axis <- .8; col.axis <- "gray30"; col.ticks <- "gray30"
    suppressWarnings(plot(HSD, 
      cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks, las=1))


    if (pdf) {
      dev.off()
      .showfile(pdf.file, "Tukey HSD chart")
      cat("\n\n")
    }
  }

}
