.OneGroup  <-
function(Y, Ynm, mu=NULL, n=NULL, m=NULL, s=NULL, brief, bw1,
         from.data, conf_level, alternative, digits_d, mmd, msmd,
         Edesired, paired, graph, xlab, line_chart, show_title, quiet,
         pdf_file, width, height, ...) { 

  # get lab_x_cex  lab_y_cex
  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_x_cex <- ifelse(is.null(lab_x_cex), lab_cex, lab_x_cex)
  adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex

  # get variable labels if exist plus axes labels
  gl <- .getlabels(xlab=NULL, ylab=xlab, main=NULL, lab_x_cex=lab_x_cex,
                   graph.win=FALSE)  # # graphics window not yet set-up
  x.name <- gl$yn; x.lbl <- gl$yl; x.lab <- gl$yb
  main.lab <- gl$mb
  sub.lab <- gl$sb

  # get variable label if exists (redundant with above
  gl <- .getlabels(graph.win=FALSE)  # graphics window not yet set-up
  y.lbl <- gl$yl
  if ( (!is.null(y.lbl))  && !quiet) {
    cat("Response Variable:  ", Ynm, ", ", as.character(y.lbl), sep="", "\n")
    cat("\n")
  }

  if (!brief && !quiet) cat("\n------ Describe ------\n\n")

  if (from.data) {
    n <- sum(!is.na(Y))
    n.miss <- sum(is.na(Y))
    Y <- na.omit(Y)
    m <- mean(Y)
    s <- sd(Y)
    v <- var(Y)
  }
  else {
    v <- s^2
  }

  clpct <- paste(toString(round((conf_level)*100, 2)), "%", sep="")

  dig.smr.d <- ifelse (from.data, digits_d, digits_d - 1)

  if (!quiet) {
    if (Ynm != "Y") cat(Ynm,  ": ", sep="")
    if (from.data) cat(" n.miss = ", n.miss, ",  ", sep="") 
    cat("n = ", n, ",   mean = ", .fmt(m, dig.smr.d), 
        ",  sd = ", .fmt(s, dig.smr.d), sep="", "\n")

    if
      (brief) cat("\n")
    else  {
      if (from.data) {

        cat("\n\n------ Normality Assumption ------\n\n")
        # Normality
        if (n > 30) {
          cat("Sample mean assumed normal because n > 30, so no test needed.",
              sep="", "\n")
        }
        else {
          cat("Null hypothesis is a normal distribution", sep="")
          if (Ynm != "Y") cat(" of ", Ynm, ".", sep="") else cat(".")
          cat("\n")
          if (n > 2 && n < 5000) {
            nrm1 <- shapiro.test(Y)
            W.1 <- round(nrm1$statistic,min(4,digits_d+1))
            p.val1 <- round(nrm1$p.value,min(4,digits_d))
            cat(nrm1$method, ":  W = ", W.1, ",  p-value = ", p.val1,
                sep="", "\n")
          }
          else
            cat("Sample size out of range for Shapiro-Wilk normality test.",
                "\n")
        }  
      } 
    }

    if (!brief) cat("\n\n------ Infer ------\n\n")
  }  # end !quiet

  # t-test
  if (alternative == "two_sided")
    alt <- "two.sided"
  else
    alt <- alternative


  if (!is.null(mu)) m.dist <- m - mu
  df <- n - 1
  sterr <- s * sqrt(1/n)
  if (alternative == "two_sided")
    tcut <- qt((1-conf_level)/2, df=df, lower.tail=FALSE)
  else if (alternative == "less")
    tcut <- qt(1-conf_level, df=df, lower.tail=FALSE)
  else if (alternative == "greater")
    tcut <- qt(1-conf_level, df=df, lower.tail=TRUE)

  if (from.data) {
    if (!is.null(mu)) mu.null <- mu else mu.null <- 0
    ttest <- t.test(Y, conf.level=conf_level, alternative=alt, mu=mu.null)
    df <- ttest$parameter
    lb <- ttest$conf[1]
    ub <- ttest$conf[2]
    E <- (ub-lb)/2
    if (!is.null(mu)) {
      tvalue <- ttest$statistic
      pvalue <- ttest$p.value
    }
  }  # end from.data
  else {
    E <- tcut * sterr
    lb <- m - E
    ub <- m + E
    if (!is.null(mu)) {
      tvalue <- m.dist/sterr
      if (alternative == "two_sided")
        pvalue <- 2 * pt(abs(tvalue), df=df, lower.tail=FALSE)
      else if (alternative == "less")
        pvalue <- pt(abs(tvalue), df=df, lower.tail=FALSE)
      else if (alternative == "greater")
        pvalue <- pt(abs(tvalue), df=df, lower.tail=TRUE)
    }
  }     
  # difference from mu and standardized mean difference
  if (!is.null(mu)) {
    mdiff <- m - mu
    smd <- abs(mdiff/s)
  }

  if (!quiet) {
    cat("t-cutoff for 95% range of variation: tcut = ", .fmt(tcut,3), "\n") 
    cat("Standard Error of Mean: SE = ", .fmt(sterr), "\n\n")

    if (!is.null(mu)) {
    if (alt != "two.sided") 
      cat("\nAlternative hypothesis: Population mean difference is", alt, 
          "than", mu.null, "\n")
      cat("Hypothesized Value H0: mu =", mu, "\n")
      txt <- "Hypothesis Test of Mean:  t-value = "
      cat(txt, .fmt(tvalue,3), ",  df = ", df, ",  p-value = ", .fmt(pvalue,3),
          sep="", "\n\n")
    }
    cat("Margin of Error for ", clpct, " Confidence Level:  ", .fmt(E),
        sep="", "\n")
    txt <- " Confidence Interval for Mean:  "
    cat(clpct, txt, .fmt(lb), " to ", .fmt(ub), sep="", "\n")

    # difference from mu and standardized mean difference
    if (!is.null(mu)) {
      if (!brief) cat("\n\n------ Effect Size ------\n\n") else cat("\n")
      cat("Distance of sample mean from hypothesized:  " , .fmt(mdiff), "\n",
          "Standardized Distance, Cohen's d:  ", .fmt(smd),
          sep="", "\n")
    }

    # needed sample size from Edesired
    if (!is.null(Edesired)) {
      zcut <- qnorm((1-conf_level)/2)
      ns <- ((zcut*s)/Edesired)^2 
      n.needed <- ceiling(1.132*ns + 7.368) 
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
      cat("Needed sample size: ", n.needed, "\n")
      cat("\n")
      cat("Additional data values needed: ", n.needed-n, "\n")
    }
  }  # end !quiet


  # graphs
  if (graph) {

    # keep track of the number of plots in this routine, see if manage graphics
    plt.i <- 0
    plt.title  <- character(length=0)
    manage.gr <- .graphman()
 
    if (is.null(pdf_file)) {
      if (manage.gr) {
        n.win <- 0
        if (!is.null(mu)) n.win <- n.win + 1
        if (paired) n.win <- n.win + 1
        if (line_chart) n.win <- n.win + 1
        if (n.win > 0) {
          .graphwin(n.win, width, height)
          i.win <- 2   # start first graphics window on 3
          orig.params <- par(no.readonly=TRUE)
          on.exit(par(orig.params))
        }
      }
    }

    if (line_chart) {
      if (!is.null(pdf_file))
        pdf(file=paste("LineChart_",Ynm,".pdf",sep=""), width=width, height=height)

      if (manage.gr) {
        i.win <- i.win + 1 
        dev.set(which=i.win)
      }

      plt.i <- plt.i + 1
      plt.title[plt.i] <- paste("Sequentially Plotted Data for", Ynm)

      Y.df = data.frame(Y)
      x.df <- data.frame(1:nrow(Y.df))
      .plt.main(x.df, Y.df, segments=TRUE, size=.9,
                xlab="Index", ylab=NULL, main=plt.title[plt.i])

      if (!is.null(pdf_file)) {
        dev.off()
        .showfile(paste("LineChart_", Ynm, ".pdf", sep=""),
                  paste("line chart of", Ynm))
      }

    }


    if (!is.null(mu)) {

      if (manage.gr) {
        i.win  <- i.win + 1 
        dev.set(which=i.win)
      }

      plt.i <- plt.i + 1
      plt.title[plt.i] <- "One-Group Plot"

      if (paired) x.lab <- "Difference"

      if (!is.null(pdf_file)) {
        if (!grepl(".pdf", pdf_file))
          pdf_file <- paste(pdf_file, ".pdf", sep="")
        .opendev(pdf_file, width, height)
      }

      .OneGraph(Y, bw1, Ynm, digits_d, brief,
           n, m, mu, mdiff, s, smd, mmd, msmd,
           clpct, tvalue, pvalue, ub, lb, x.lab, alt, show_title, quiet)

      if (!is.null(pdf_file)) {
        dev.off()
        .showfile(pdf_file, paste("density plot of", Ynm))
      }
    }

    return(list(i=plt.i, ttl=plt.title))
  }  # end if graph

}  # End One Group

