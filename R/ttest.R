ttest <-
function(x=NULL, y=NULL, data=mydata, paired=FALSE,

         n=NULL, m=NULL, s=NULL, mu0=NULL, 
         n1=NULL, n2=NULL, m1=NULL, m2=NULL, s1=NULL, s2=NULL, 

         Ynm="Y", Xnm="X", X1nm="Group1", X2nm="Group2", 

         brief=getOption("brief"), digits.d=NULL, conf.level=0.95,
         alternative=c("two.sided", "less", "greater"),
         mmd=NULL, msmd=NULL, Edesired=NULL, 

         show.title=TRUE, bw1="bcv", bw2="bcv",

         graph=TRUE, line.chart=FALSE,
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...)  {       


tt.setup <-
function(x, y=NULL, ...) {

  # keep track of generated graphics from tt.setup
  plot.i <- 0
  plot.title  <- character(length=0)

  cat("\n")
  if (missing(y))
    no.y <- TRUE
  else 
    if (is.null(y)) no.y <- TRUE else no.y  <- FALSE
  if (is.null(n1) && no.y) two.gp <- FALSE else two.gp <- TRUE
  if (is.null(n) && is.null(n1)) from.data <- TRUE else from.data <- FALSE

  if (!from.data) graph <- FALSE

  if (is.null(digits.d)) {
    if (from.data) digits.d <- .max.dd(x)
    else {
      digits.d <- 0
      if (!is.null(m)) if (.max.dd(m) > digits.d) digits.d <- .max.dd(m)   
      if (!is.null(s)) if (.max.dd(s) > digits.d) digits.d <- .max.dd(s)   
      if (!is.null(m1)) if (.max.dd(m1) > digits.d) digits.d <- .max.dd(m1)   
      if (!is.null(m2)) if (.max.dd(m2) > digits.d) digits.d <- .max.dd(m2)   
      if (!is.null(s1)) if (.max.dd(s1) > digits.d) digits.d <- .max.dd(s1)   
      if (!is.null(s2)) if (.max.dd(s2) > digits.d) digits.d <- .max.dd(s2)   
    }
    digits.d <- digits.d + 1
    if (digits.d == 1) digits.d <- 2
  }
  options(digits.d=digits.d)
  if (digits.d > 10) {
    cat("\nThese data contain", digits.d, "significant digits.\n")
    cat("Perhaps specify less digits to display with the  digits.d  parameter.\n\n")
  }

  if (two.gp) {
    if (from.data) { 

      if ( (length(x) < 2) || (length(y) < 2) )  {
       cat("\n"); stop(call.=FALSE, "\n","------\n",
         "Need at least two cases (observations) per sample.\n\n")
      }
     
      if ( !is.null(mmd) && !is.null(msmd) )  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
         "Specify only one of mmd and msmd as one implies the other.\n\n")
      }

      # Always put the group with the largest mean first
      if (mean(x, na.rm=TRUE) > mean(y, na.rm=TRUE))
        plt2 <- .TwoGroup(x, y, n1, n2, m1, m2, s1, s2, from.data,
          Ynm, Xnm, X1nm, X2nm, brief, digits.d, 
          conf.level, alternative, mmd, msmd, Edesired, bw1, bw2, graph,
          line.chart, show.title, pdf.file, pdf.width, pdf.height)
      else {  # switch
        Xtmp <- X2nm
        X2nm <- X1nm
        X1nm <- Xtmp
        plt2 <- .TwoGroup(y, x, n1, n2, m1, m2, s1, s2, from.data,
          Ynm, Xnm, X1nm, X2nm, brief, digits.d, 
          conf.level, alternative, mmd, msmd, Edesired, bw1, bw2, graph,
          line.chart, show.title, pdf.file, pdf.width, pdf.height)
      }

      for (i in (plot.i+1):(plot.i+plt2$i)) plot.title[i] <- plt2$ttl[i-plot.i]
      plot.i <- plot.i + plt2$i


    }  # end from data

    else {  # from stats
      .TwoGroup(y, x,
         n1, n2, m1, m2, s1, s2, from.data,
         Ynm, Xnm, X1nm, X2nm, brief, digits.d, conf.level,
         alternative, mmd, msmd, Edesired, bw1, bw2,
         graph=FALSE, line.chart=FALSE)
    }
  if (!brief) {
    txt <- "Kelley and Lai's MBESS package]"
    cat("\n[smd CI with Ken Kelley's ci.smd function from", txt, "\n") 
  }

  }  # end two group

  else { # one group, including paired
    if (from.data) {
      if (!paired)
        Ynm <- x.name
      else {
        Ynm <- "Difference"
        mu0 <- 0
        options(dname = NULL)
      }

      options(yname = x.name)
      plt1 <- .OneGroup(x, Ynm, mu0, n=NULL, m=NULL, s=NULL, brief, bw1,
         from.data, conf.level, alternative, digits.d, mmd, msmd,
         Edesired, paired, graph, line.chart, show.title,
         pdf.file, pdf.width, pdf.height, ...)

    if (!is.null(plt1$i)) {
        for (i in (plot.i+1):(plot.i+plt1$i)) plot.title[i] <- plt1$ttl[i-plot.i]
        plot.i <- plot.i + plt1$i
      }
    }  # end from data
    else  # from stats
       .OneGroup(x, Ynm, mu0, n, m, s, brief, bw1,
         from.data, conf.level, alternative, digits.d, mmd, msmd,
         Edesired, paired, graph, line.chart, show.title,
         pdf.file, pdf.width, pdf.height, ...)
  }  # end one group

  # return number of plots to main
  if (plot.i > 0) return(list(i=plot.i, ttl=plot.title))

}  # end tt.setup


#-----------------------------------
# BEGIN
#-----------------------------------


  alternative <- match.arg(alternative)
 
  if (missing(x)  &&  missing(n)  &&  missing(n1)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify a variable to analyze, or provide summary stats.\n\n")
  }
        
  if (!is.null(Edesired) && conf.level != 0.95) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Edesired calculation only applies to 95% confidence level.\n\n")
  }

  # keep track of generated graphics
  plot.i <- 0
  plot.title  <- character(length=0)

  # get actual variable name before potential call of data$x, could be NULL
  x.name <- deparse(substitute(x)) 
  if (!missing(y)) y.name <- deparse(substitute(y)) 

  # get data frame name
  dname <- deparse(substitute(data))
 
  if (exists(x.name, where=.GlobalEnv)) {
    if (is.data.frame(x)) {
      nm <- names((eval(substitute(x))))
      txt <- ifelse(length(nm)>1, "one of those variables", "that variable")
      nm2 <- "" 
      for (j in 1:length(nm)) nm2 <- paste(nm2, nm[j])
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The argument to the ttest function you specified, ", dname, ", is\n",
        "a data table, not a variable. A data table contains the data values\n",
        "for one or more variables. This data table references the variables:\n\n",
         "  ", nm2, "\n\n",
        "Perhaps you meant to analyze ", txt, ".\n\n")
    }
  }

  # get conditions and check for data existing
  xs <- .xstatus(x.name, dname)
  is.frml <- xs$ifr
  from.data <- xs$fd
  in.global <- xs$ig 
  if (!missing(y)) .xstatus(y.name, dname)  # just for a message on output 

  # see if the variable exists in the data frame
  if (from.data && !in.global && !is.frml) .xcheck(x.name, dname, data)


  # --------------------------
  # do analysis with  tt.setup
  # plt is the returned number of plots generated 

  if (in.global || is.frml || from.data) {

    if (in.global) {
      if (is.function(x))  # var names that are R functions get assigned to global 
        plt <- tt.setup(eval(substitute(data$x)), Ynm=x.name, ...)  # 1-group
      else {  # not a function name
        if (!missing(y))
           y.l <- length(y)
        else
           y.l <- 0
        if (!paired)
          if (y.l == 0)  # 1-group
            plt <- tt.setup(x, ...)
          else  # 2-group
            plt <- tt.setup(x, y,  ...)
        else {  # paired
          if (length(x)!=length(y))  {
            cat("\n"); stop(call.=FALSE, "\n","------\n",
               "The two columns of data values must be the same size.\n\n")
          }
          diff <- y - x
          plt <- tt.setup(diff, ...)
        }
      }
    }

    else if (is.frml) {
      f <- .tt.formula(x, y, data, ...)  # formula
      x <- f$x;  y <- f$y;  Ynm <- f$Ynm;  Xnm <- f$Xnm
      X1nm <- f$X1nm;  X2nm <- f$X2nm 
      plt <- tt.setup(x, y, ...)
    }

    else if (from.data) {
      if (!missing(y))
         y.l <- length(eval(substitute(data$y)))
      else
         y.l <- 0
      if (!paired) {
        if (y.l == 0)  # 1-group
          plt <- tt.setup(eval(substitute(data$x)), ...)
        else  # 2-group
          plt <- tt.setup(eval(substitute(data$x)), eval(substitute(data$y)),  ...)
      }
      else {   # paired 
        diff <- eval(substitute(data$y)) - eval(substitute(data$x))
        plt <- tt.setup(diff, ...)
      }
    }

    if (!is.null(plt$i)) {
      for (i in (plot.i+1):(plot.i+plt$i)) plot.title[i] <- plt$ttl[i-plot.i]
      plot.i <- plot.i + plt$i
    }
  }  # in.global || is.frml || from.data 

  else
    tt.setup(...)  # analysis from stats


  # --------------------------


  if (paired) {  # scatter plot of both variables, need both vars so do here
    manage.gr <- .graphman()

    if (is.null(pdf.file)) {
      if (manage.gr) {
        if (!line.chart)
          dev.set(which=4)
        else
          dev.set(which=5)
      }
    }
    else
      pdf(file="PairedScatterPlot.pdf", width=pdf.width, height=pdf.height)

    if (in.global) {
      x.values <- x
      y.values <- y
    }
    else {
      x.values <- eval(substitute(data$x))
      y.values <- eval(substitute(data$y))
    }

    plot.i <- plot.i + 1
    plot.title[plot.i] <- "Differences from Equality"

    .plt.main(x.values, y.values,
       by=NULL, type="p", n.cat=getOption("n.cat"),
       col.area=NULL, col.box="black",
       col.fill=getOption("col.fill.pt"),
       col.stroke=getOption("col.stroke.pt"),
       col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),
       shape.pts=21, cex.axis=.85, col.axis="gray30",
       xy.ticks=TRUE,
       xlab=x.name, ylab=y.name, main=plot.title[plot.i],
       cex=.8, kind="default", 
       x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
       fit.line="none", col.fit.line="grey55", center.line=NULL,
       col.bubble=NULL, bubble.size=.25, col.flower=NULL,
       ellipse=FALSE, 
       diag=TRUE, col.diag=par("fg"), lines.diag=TRUE,
       quiet=TRUE)

    if (!is.null(pdf.file)) {
      dev.off()
      .showfile("PairedScatterPlot.pdf", "scatter plot with changes from diagonal")
      cat("\n\n")
    }

  }  # end paired


  if (!paired) cat("\n")

  if (is.null(options()$knitr.in.progress))
    if (plot.i > 1) .plotList(plot.i, plot.title)  # list plots generated

  if (is.frml) {
    if (mean(x, na.rm=TRUE) > mean(y, na.rm=TRUE))
      invisible(list(value1=X1nm, group1=x, value2=X2nm, group2=y))
    else
      invisible(list(value1=X2nm, group1=y, value2=X1nm, group2=x))
  }

}

