.TwoGroup <-
function(YA, YB, n1, n2, m1, m2, s1, s2, from.data,
         Ynm, Xnm, X1nm, X2nm, brief, digits_d,
         conf_level, alternative, mmd, msmd, Edesired, bw1, bw2,
         graph, xlab, line_chart, show_title,
         quiet, pdf_file, width, height, ...)  {        
 
  if ( brief  &&  !quiet  &&  (!is.null(mmd) || !is.null(msmd)) ) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "mmd and msmd do not work with the brief version.\n\n")
  }

  # get lab_x_cex  lab_y_cex
  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_x_cex <- ifelse(is.null(lab_x_cex), lab_cex, lab_x_cex)
  adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex

  # get variable labels if exist plus axes labels
  gl <- .getlabels(xlab=NULL, ylab=xlab, main=NULL, lab_x_cex=lab_x_cex,
                   graph.win=FALSE)  # # graphics window not yet set-up
  x.name <- gl$yn; x.lbl <- gl$yl; x.lab <- gl$yb

  # get variable labels if exist
  options(xname = Xnm)
  options(yname = Ynm)
  gl <- .getlabels(graph.win=FALSE)  # graphics window not yet set-up
  x.lbl <- gl$xl
  y.lbl <- gl$yl
  if (is.null(y.lbl)) y.lbl <- Ynm

  if (!quiet) {
    cat("Compare", Ynm, "across", Xnm, "with levels", X1nm, "and", X2nm, "\n")
  # cat("--------------------------------------------------------------\n\n")

    if (!is.null(x.lbl))
      cat("Grouping Variable:  ", Xnm, ", ", as.character(x.lbl), sep="", "\n")
    else
      cat("Grouping Variable:  ", Xnm, sep="", "\n")
    if (Ynm != as.character(y.lbl))
      cat("Response Variable:  ", Ynm, ", ", as.character(y.lbl), sep="", "\n")
    else 
      cat("Response Variable:  ", Ynm, sep="", "\n")
    cat("\n")

    if (!brief)
       cat("\n------ Describe ------\n\n")
    else
      cat("\n --- Describe ---\n\n")
  }  # end !quiet


  if (from.data) {
    n1 <- sum(!is.na(YA))
    n2 <- sum(!is.na(YB))
    n1.miss <- sum(is.na(YA))
    n2.miss <- sum(is.na(YB))
    YA <- na.omit(YA)
    YB <- na.omit(YB)
    m1 <- mean(YA)
    m2 <- mean(YB)
    s1 <- sd(YA)
    s2 <- sd(YB)
    v1 <- var(YA)
    v2 <- var(YB)
  }
  else {
    v1 <- s1^2
    v2 <- s2^2
  }

  if (from.data) dig.smr.d  <- digits_d  else dig.smr.d <- digits_d - 1

  clpct <- paste(toString(round((conf_level)*100, 2)), "%", sep="")
  Xnmval <- paste(Xnm, X1nm)
  if (!quiet) {
    cat(Ynm, " for ", Xnmval, ":  ", sep="")
    if (from.data) cat("n.miss = ", n1.miss, ",  ", sep="")
    cat("n = ", n1, sep="")
    cat(",  mean = ", .fmt(m1,dig.smr.d), ",  sd = ", .fmt(s1,dig.smr.d),
        sep="", "\n")
  }  # end !quiet
  Xnmval <- paste(Xnm, X2nm)
  if (!quiet) {
    cat(Ynm, " for ", Xnmval, ":  ", sep="")
    if (from.data) cat("n.miss = ", n2.miss, ",  ", sep="")
    cat("n = ", n2, sep="")
    cat(",  mean = ", .fmt(m2,dig.smr.d), ",  sd = ", .fmt(s2,dig.smr.d),
        sep="", "\n")
    cat("\n")
  }  # end !quiet

  # sample mean difference
  mdiff <- m1 - m2
  if (!quiet)
    cat("Mean Difference of ", Ynm, ":  " , .fmt(mdiff), sep="", "\n")
  if (!brief) cat("\n")

# sw
  df1 <- n1 - 1
  df2 <- n2 - 1
  swsq <- (df1*v1 + df2*v2) / (df1 + df2)
  sw <- sqrt(swsq)
  if (!quiet)
    cat("Weighted Average Standard Deviation:  ", .fmt(sw), "\n")

  smd <- mdiff/sw
  if (brief && !quiet) 
    cat("Standardized Mean Difference of ", Ynm, ": ",
        .fmt(smd), sep="", "\n")

  if (!brief) {

    if (!quiet) {
      cat("\n\n------ Assumptions ------\n\n")

      cat("Note: These hypothesis tests can perform poorly, and the", "\n")
      cat("      t-test is typically robust to violations of assumptions.",
                 "\n")
      cat("      Use as heuristic guides instead of interpreting literally.",
          "\n\n")
    }  # end !quiet


    if (from.data) {
      if (!quiet) {

        # Normality
        cat("Null hypothesis, for each group, is a normal distribution of ",
            sep="")
        cat(Ynm, ".", sep="", "\n")
        if (n1 > 30) {
          cat("Group " , X1nm, ": ", sep="")
          cat("Sample mean assumed normal because n > 30, so no test needed.", 
              sep="", "\n")
        }
        else {
          cat("Group", X1nm, " ")
          if (n1 > 2 && n1 < 5000) {
            nrm1 <- shapiro.test(YA)
            W.1 <- nrm1$statistic
            p.val1 <- nrm1$p.value
            cat(nrm1$method, ":  W = ", .fmt(W.1,3), ",  p-value = ",
                .fmt(p.val1,3), sep="", "\n")
          }
        else
        cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
      }  
      if (n2 > 30) {
        cat("Group " , X2nm, ": ", sep="")
        cat("Sample mean assumed normal because n > 30, so no test needed.",
            sep="", "\n")
      }
      else {
        cat("Group", X2nm, " ")
        if (n2 > 2 && n2 < 5000) {
          nrm2 <- shapiro.test(YB)
          W.2 <- nrm2$statistic
          p.val2 <- nrm2$p.value
          cat(nrm2$method, ":  W = ", .fmt(W.2,3), ",  p-value = ",
              .fmt(p.val2,3), sep="", "\n")
        }
        else
          cat("Sample size out of range for Shapiro-Wilk normality test.", "\n")
      }  
      cat("\n")
    }  # end !quiet 
  } 

  # Homogeneity of Variance
  # Var Ratio
  if (v1 >= v2) {
    vratio <- v1/v2
      vr <- paste(.fmt(v1), "/", .fmt(v2), sep="")
      df.num <- df1
      df.den <- df2
  }
  else {
    vratio <- v2/v1
      vr <- paste(.fmt(v2), "/", .fmt(v1), sep="")
      df.num <- df2
      df.den <- df1
  }

  p.var <- pf(vratio, df1=df.num, df2=df.den)
  # adjust for two-sided test, results same as var.test{stats}
  p.var <- 2 * min(p.var, 1-p.var)

  if (!quiet) {
    cat("Null hypothesis is equal variances of ")
    cat(Ynm, ", homogeneous.", sep="", "\n")

    cat("Variance Ratio test:  F = ", vr, " = ", .fmt(vratio),
        ",  df = ", df.num, ";", 
        df.den, ",  p-value = ",  .fmt(p.var,3), sep="", "\n")
  }  # end !quiet

  if (from.data) { # Levene
    YAm <- abs(YA - median(YA))
      YBm <- abs(YB - median(YB))
      t.bf <- t.test(YAm, YBm, var.equal=TRUE)
      tvalue.bf <- t.bf$statistic
      df.bf <- t.bf$parameter
      pvalue.bf <- t.bf$p.value
      if (!quiet) {
        cat("Levene's test, Brown-Forsythe:  t = ", .fmt(tvalue.bf,3),
            ",  df = ", df.bf, sep="")
        cat(",  p-value = ", .fmt(pvalue.bf,3), sep="", "\n")
      }  # end !quiet
  }
}


  if (!quiet) {
    if (!brief)
      cat("\n\n------ Infer ------\n\n")
    else 
      cat("\n --- Infer ---\n\n")
  }  # end !quiet
  

  # t-test
  if (alternative == "two_sided")
    alt <- "two.sided"
  else
    alt <- alternative

  sterr <- sw * sqrt(1/n1 + 1/n2)
  df <- df1 + df2
  if (alternative == "two_sided")
    tcut <- qt((1-conf_level)/2, df=df, lower.tail=FALSE)
  else if (alternative == "less")
    tcut <- qt(1-conf_level, df=df, lower.tail=FALSE)
  else if (alternative == "greater")
    tcut <- qt(1-conf_level, df=df, lower.tail=TRUE)

  if (from.data) {
    ttest <- t.test(YA, YB, var.equal=TRUE, conf_level=conf_level,
                    alternative=alt)
    lb <- ttest$conf[1]
    ub <- ttest$conf[2]
    E <- (ub-lb) / 2
    tvalue <- ttest$statistic
    pvalue <- ttest$p.value
  }
  else {
    sterr <- sw * sqrt(1/n1 + 1/n2)
    E <- tcut*sterr
    lb <- mdiff-E
    ub <- mdiff+E
    tvalue <- mdiff/sterr
    if (alternative == "two_sided")
      pvalue <- 2 * pt(abs(tvalue), df=df, lower.tail=FALSE)
    else if (alternative == "less")
      pvalue <- pt(abs(tvalue), df=df, lower.tail=FALSE)
    else if (alternative == "greater")
      pvalue <- pt(abs(tvalue), df=df, lower.tail=TRUE)
  }

  if (!quiet) {
    if (!brief)
      cat("--- Assume equal population variances of", Ynm, "for each", Xnm,
          "\n\n")
    cat("t-cutoff for 95% range of variation: tcut = ", .fmt(tcut,3), "\n") 
    cat("Standard Error of Mean Difference: SE = ", .fmt(sterr), "\n")
   
    mytitle <- "\nHypothesis Test of 0 Mean Diff:  t-value = "
    if (alt != "two.sided") 
      cat("\nAlternative hypothesis: Population mean difference is", alt, 
          "than 0")
    cat(mytitle, .fmt(tvalue,3), ",  df = ", df, ",  p-value = ", .fmt(pvalue,3),
        sep="", "\n\n")
    cat("Margin of Error for ", clpct, " Confidence Level:  ", .fmt(E), sep="",
        "\n")
    cat(clpct," Confidence Interval for Mean Difference:  ", .fmt(lb), " to ",
        .fmt(ub), sep="", "\n\n")
  }  # end !quiet

  if (!brief) {
    k1 <- v1/n1
    k2 <- v2/n2
    df.ne <- ((k1 + k2)^2) / ((k1^2)/(n1-1) + (k2^2)/(n2-1))
    sterr.ne <- sqrt(k1 + k2)
    if (alternative == "two_sided")
      tcut.ne <- qt((1-conf_level)/2, df=df.ne, lower.tail=FALSE)
    else if (alternative == "less")
      tcut.ne <- qt(1-conf_level, df=df.ne, lower.tail=FALSE)
    else if (alternative == "greater")
      tcut.ne <- qt(1-conf_level, df=df.ne, lower.tail=TRUE)

    if (from.data) {
      ttne <- t.test(YA, YB, var.equal=FALSE, conf_level=conf_level,
                     alternative=alt)
      df.ne <- ttne$parameter
      lb.ne <- ttne$conf[1]
      ub.ne <- ttne$conf[2]
      E.ne <- (ub.ne-lb.ne)/2
      tvalue.ne <- ttne$statistic
      pvalue.ne <- ttne$p.value
    }
    else {
      E.ne <- tcut.ne*sterr.ne
      lb.ne <- mdiff-E.ne
      ub.ne <- mdiff+E.ne
      tvalue.ne <- mdiff / sterr.ne
    if (alternative == "two_sided")
      pvalue.ne <- 2 * pt(abs(tvalue.ne), df=df.ne, lower.tail=FALSE)
    else if (alternative == "less")
      pvalue.ne <- pt(abs(tvalue.ne), df=df.ne, lower.tail=FALSE)
    else if (alternative == "greater")
      pvalue.ne <- pt(abs(tvalue.ne), df=df.ne, lower.tail=TRUE)
    }

    if (!quiet) {
      cat("\n--- Do not assume equal population variances of", Ynm, "for each",
          Xnm, "\n\n")
      cat("t-cutoff: tcut = ", .fmt(tcut.ne,3), "\n") 
      cat("Standard Error of Mean Difference: SE = ", .fmt(sterr.ne), "\n")
      mytitle <- "\nHypothesis Test of 0 Mean Diff:  t = "
      cat(mytitle, .fmt(tvalue.ne,3), ",  df = ", .fmt(df.ne,3), 
          ", p-value = ", .fmt(pvalue.ne,3), sep="", "\n\n")
      cat("Margin of Error for ", clpct, " Confidence Level:  ", .fmt(E.ne),
          sep="", "\n")
      cat(clpct," Confidence Interval for Mean Difference:  ", .fmt(lb.ne),
          " to ", .fmt(ub.ne), sep="", "\n")
    }  # end !quiet

  }

  # mean difference and standardized mean difference
  if (!brief  && !quiet) {
    cat("\n\n------ Effect Size ------\n\n")
    cat("--- Assume equal population variances of", Ynm, "for each", Xnm,
        "\n\n")
    cat("Standardized Mean Difference of ", Ynm, ", ",
        "Cohen's d:  ", .fmt(smd), sep="", "\n")
  }  # end !quiet

  # MBESS function
  #cid <- ci.smd(smd=smd, n.1=n1, n.2=n2, conf_level=conf_level)  
  #deltaL <-cid$Lower.Conf.Limit.smd
  #deltaU <- cid$Upper.Conf.Limit.smd
  #if (!brief) cat("\n")
  #cat(clpct," Confidence Interval for smd:  ",
      #.fmt(deltaL), " to ", .fmt(deltaU), sep="", "\n")

  if (!brief) {
    if ( !is.null(mmd) | !is.null(msmd) ) {
      if (!is.null(mmd)) msmd <- mmd / sw
      if (!is.null(msmd)) mmd <- msmd * sw
    }
  }

  if (!brief  && !quiet) {
    cat("\n\n------ Practical Importance ------\n\n")
    cat("Minimum Mean Difference of practical importance: mmd\n")
    if ( !is.null(mmd) | !is.null(msmd) ) {
      cat("Compare mmd =", .fmt(mmd,digits_d),
          " to the obtained value of md = ", .fmt(mdiff), "\n")
      cat("Compare mmd to the confidence interval for md: ", .fmt(lb), " to ", 
          .fmt(ub), "\n\n")
      cat("Minimum Standardized Mean Difference of practical importance: msmd\n")
      cat("Compare msmd = ", .fmt(msmd,digits_d),
          " to the obtained value of smd = ", .fmt(smd),"\n")
    }
    else {
      cat("Minimum Standardized Mean Difference of practical importance: msmd\n")
      cat("Neither value specified, so no analysis\n")
    }
  }

  # needed sample size from Edesired
  if (!is.null(Edesired)) {
    zcut <- qnorm((1-conf_level)/2)
    ns <- 2*((zcut*sw)/Edesired)^2 
    n.needed <- ceiling(1.099*ns + 4.863) 
    if (!quiet) {
      cat("\n\n------ Needed Sample Size ------\n\n")
      if (Edesired > E) {
        cat("Note: Desired margin of error,", .fmt(Edesired), 
           "is worse than what was obtained,", .fmt(E), "\n\n") 
      }
      cat("Desired Margin of Error: ", .fmt(Edesired), "\n")
      cat("\n")
      cat("For the following sample size there is a 0.9 probability of obtaining\n")
      cat("the desired margin of error for the resulting 95% confidence interval.\n")
      cat("-------\n")
      cat("Needed sample size per group: ", n.needed, "\n")
      cat("\n")
      cat("Additional data values needed Group 1: ", n.needed-n1, "\n")
      cat("Additional data values needed Group 2: ", n.needed-n2, "\n")
    }  # end !quiet
  }

  # graphs
  if (from.data && graph) {

    # keep track of the number of plots in this routine, see if manage graphics
    plt.i <- 0
    plt.title  <- character(length=0)
    manage.gr <- .graphman()

    if (is.null(pdf_file)) {
      if (manage.gr) {
        n.win <- ifelse (!line_chart, 1, 3)
        .graphwin(n.win, width, height)
        i.win <- 2   # start first graphics window on 3
        orig.params <- par(no.readonly=TRUE)
        on.exit(par(orig.params))
     }
    }

    if (line_chart) { 

      if (!is.null(pdf_file))
        pdf(file=paste("LineChart_",X1nm,".pdf",sep=""),
             width=width, height=height)

      if (manage.gr) {
        i.win <- i.win + 1 
        dev.set(which=i.win)
      }

      plt.i <- plt.i + 1
      plt.title[plt.i] <- paste("Sequentially Ordered Data:", paste(Xnm, X1nm))

      YA.df <- data.frame(YA)
      x.df <- data.frame(1:nrow(YA.df))
      .plt.main(x.df, YA.df, segments=TRUE, size=.85,
                center_line="median", fill=getOption("pt_fill"),
                xlab="Index", ylab=paste(Ynm,": ",X1nm, sep=""),
                main=plt.title[plt.i])

      if (!is.null(pdf_file)) {
        dev.off()
        .showfile(paste("LineChart_", X1nm, ".pdf", sep=""),
                  paste("line chart of", X1nm))
      }

      if (manage.gr) {
        i.win <- i.win + 1 
        dev.set(which=i.win)
      }

      plt.i <- plt.i + 1
      plt.title[plt.i] <- paste("Sequentially Ordered Data:", paste(Xnm, X2nm))

      YB.df <- data.frame(YB)
      x.df <- data.frame(1:nrow(YB.df))
      .plt.main(x.df, YB.df, segments=TRUE, size=.85,
                center_line="median", fill=getOption("pt_fill"),
                xlab="Index", ylab=paste(Ynm,": ",X2nm, sep=""),
                main=plt.title[plt.i])
 
      if (!is.null(pdf_file)) {
        dev.off()
        .showfile(paste("LineChart_", X2nm, ".pdf", sep=""),
                  paste("line chart of", X2nm))
      }
    }


    # two density graphs
    # prepare graphics window, dev or pdf

    if (!is.null(pdf_file))
      .opendev(pdf_file, width, height)
    else {
      if (manage.gr) {
        i.win  <- i.win + 1 
        dev.set(which=i.win)
      }
    }

    plt.i <- plt.i + 1
    plt.title[plt.i] <- "Two-Group Plot"

    .TwoGraph(YA, YB, bw1, bw2, Ynm, Xnm, X1nm, X2nm, y.lbl, digits_d, brief,
              n1, m1, s1, n2, m2, s2, df, mdiff, sw, smd, mmd, msmd,
              clpct, tvalue, pvalue, ub, lb, x.lab, alt, show_title, quiet)

    if (!quiet) cat("\n")

    if (!is.null(pdf_file)) {
      dev.off()
      .showfile(pdf_file, paste("density plots of", Ynm, "for both groups"))
    }

    return(list(i=plt.i, ttl=plt.title))
  }
} # End Two Group
