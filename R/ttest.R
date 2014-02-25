ttest <-
function(x=NULL, y=NULL, data=mydata, paired=FALSE,

         n=NULL, m=NULL, s=NULL, mu0=NULL, 
         n1=NULL, n2=NULL, m1=NULL, m2=NULL, s1=NULL, s2=NULL, 

         Ynm="Y", Xnm="X", X1nm="Group1", X2nm="Group2", 

         brief=getOption("brief"), digits.d=NULL, conf.level=0.95,
         alternative=c("two.sided", "less", "greater"),
         mmd=NULL, msmd=NULL, 

         show.title=TRUE, bw1="bcv", bw2="bcv",

         graph=TRUE, line.chart=FALSE,
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...)  {       


tt.setup <-
function(x, y=NULL, ...) {

  cat("\n")
  if (missing(y))
    no.y <- TRUE
  else 
    if (is.null(y)) no.y <- TRUE else no.y  <- FALSE
  if (is.null(n1) && no.y) two.gp <- FALSE else two.gp <- TRUE
  if (is.null(n) && is.null(n1)) from.data <- TRUE else from.data <- FALSE

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
        .TwoGroup(x, y, n1, n2, m1, m2, s1, s2, from.data,
          Ynm, Xnm, X1nm, X2nm, brief, digits.d, 
          conf.level, alternative, mmd, msmd, bw1, bw2, graph,
          line.chart, show.title, pdf.file, pdf.width, pdf.height)
      else {  # switch
        Xtmp <- X2nm
        X2nm <- X1nm
        X1nm <- Xtmp
        .TwoGroup(y, x, n1, n2, m1, m2, s1, s2, from.data,
          Ynm, Xnm, X1nm, X2nm, brief, digits.d, 
          conf.level, alternative, mmd, msmd, bw1, bw2, graph,
          line.chart, show.title, pdf.file, pdf.width, pdf.height)
      }

    }

    else {  # from stats
      .TwoGroup(y, x,
         n1, n2, m1, m2, s1, s2, from.data,
         Ynm, Xnm, X1nm, X2nm, brief, digits.d, conf.level,
         alternative, mmd, msmd, bw1, bw2, graph=FALSE, line.chart=FALSE)
    }

  }  # end two.gp 

  else { # one group
    if (from.data) {
      if (!paired)
        Ynm <- x.name
      else {
        Ynm <- "Difference"
        mu0 <- 0
        options(dname = NULL)
      }
      options(yname = x.name)
      .OneGroup(x, Ynm, mu0, brief=brief, bw1=bw1,
           from.data=from.data, conf.level=conf.level,
           alternative=alternative, digits.d=digits.d,
           mmd=mmd, msmd=msmd, paired=paired,
           graph=graph, line.chart=line.chart,
           show.title=show.title, pdf.file=pdf.file,
           pdf.width=pdf.width, pdf.height=pdf.height)
    }
    else  # from stats
      .OneGroup(x, Ynm, mu0, n, m, s, brief=brief, bw1=bw1,
           from.data=from.data, conf.level=conf.level,
           alternative=alternative, digits.d=digits.d,
           mmd=mmd, msmd=msmd, paired=paired,
           graph=graph, line.chart=line.chart,
           show.title=show.title, pdf.file=pdf.file,
           pdf.width=pdf.width, pdf.height=pdf.height, ...)
  }

}  # end tt.setup


#-----------------------------------
# BEGIN
#-----------------------------------

  alternative <- match.arg(alternative)

  # get actual variable name before potential call of data$x, could be NULL
  x.name <- deparse(substitute(x)) 
  if (!missing(y)) y.name <- deparse(substitute(y)) 

  # get data frame name
  dname <- deparse(substitute(data))

  # get conditions and check for data existing
  xs <- .xstatus(x.name, dname)
  is.frml <- xs$ifr
  from.data <- xs$fd
  in.global <- xs$ig 
  if (!missing(y)) .xstatus(y.name, dname)  # just for a message on output 

  # see if the variable exists in the data frame
  if (from.data && !in.global && !is.frml) .xcheck(x.name, dname, data)

  # do analysis with tt.setup
  if (in.global) {
    if (is.function(x))  # var names that are R functions get assigned to global 
      tt.setup(eval(substitute(data$x)), Ynm=x.name, ...)  # 1-group
    else {  # not a function name
      if (!missing(y))
         y.l <- length(y)
      else
         y.l <- 0
      if (!paired)
        if (y.l == 0)  # 1-group
          tt.setup(x, ...)
        else  # 2-group
          tt.setup(x, y,  ...)
      else {  # paired
        if (length(x)!=length(y))  {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
             "The two data vectors must be of the same size.\n\n")
        }
        diff <- x - y
        tt.setup(diff, ...)
      }
    }
  }

  else if (is.frml) {
    f <- .tt.formula(x, y, data, ...)  # formula
    x <- f$x;  y <- f$y;  Ynm <- f$Ynm;  Xnm <- f$Xnm
    X1nm <- f$X1nm;  X2nm <- f$X2nm 
    tt.setup(x, y, ...)
  }

  else if (from.data) {
    if (!missing(y))
       y.l <- length(eval(substitute(data$y)))
    else
       y.l <- 0
    if (!paired) {
      if (y.l == 0)  # 1-group
        tt.setup(eval(substitute(data$x)), ...)
      else  # 2-group
        tt.setup(eval(substitute(data$x)), eval(substitute(data$y)),  ...)
    }
    else {   # paired 
      diff <- eval(substitute(data$x)) - eval(substitute(data$y))
      tt.setup(diff, ...)
    }
  }

  else
    tt.setup(...)  # analysis from stats


  if (paired) {  # scatter plot of both variables, need both vars so do here
    if (is.null(pdf.file))
      if (!line.chart)
        dev.set(which=4)
      else
        dev.set(which=5)
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

    .plt.main(x.values, y.values,
       by=NULL, type="p", n.cat=getOption("n.cat"),
       col.area=NULL, col.box="black",
       col.fill=getOption("col.fill.pt"),
       col.stroke=getOption("col.stroke.pt"),
       col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),
       shape.pts=21, cex.axis=.85, col.axis="gray30",
       col.ticks="gray30", xy.ticks=TRUE,
       xlab=x.name, ylab=y.name, main="",
       cex=.8, kind="default", 
       x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
       fit.line="none", col.fit.line="grey55", center.line=NULL,
       col.bubble=NULL, bubble.size=.25, col.flower=NULL,
       ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE,
       diag=TRUE, col.diag=par("fg"), lines.diag=TRUE,
       quiet=TRUE)

    if (!is.null(pdf.file)) {
      dev.off()
      .showfile("PairedScatterPlot.pdf", "scatter plot with changes from diagonal")
      cat("\n\n")
    }

  }

  txt <- "Kelley and Lai's MBESS package]"
  cat("[smd CI with Ken Kelley's ci.smd function from", txt, "\n") 

  if (!paired) cat("\n")

  if (is.frml) {
    if (mean(x, na.rm=TRUE) > mean(y, na.rm=TRUE))
      invisible(list(value1=X1nm, group1=x, value2=X2nm, group2=y))
    else
      invisible(list(value1=X2nm, group1=y, value2=X1nm, group2=x))
  }

}
