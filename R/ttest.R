ttest <-
function(x=NULL, y=NULL, data=mydata, paired=FALSE,

         n=NULL, m=NULL, s=NULL, mu0=NULL, 
         n1=NULL, n2=NULL, m1=NULL, m2=NULL, s1=NULL, s2=NULL, 

         Ynm="Y", Xnm="X", X1nm="Group1", X2nm="Group2", 

         brief=getOption("brief"), digits.d=NULL, conf.level=0.95,
         alternative=c("two.sided", "less", "greater"),
         mmd=NULL, msmd=NULL, 

         show.title=TRUE, bw1="nrd", bw2="nrd", graph=TRUE,
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
          show.title, pdf.file, pdf.width, pdf.height)
      else {  # switch
        Xtmp <- X2nm
        X2nm <- X1nm
        X1nm <- Xtmp
        .TwoGroup(y, x, n1, n2, m1, m2, s1, s2, from.data,
          Ynm, Xnm, X1nm, X2nm, brief, digits.d, 
          conf.level, alternative, mmd, msmd, bw1, bw2, graph,
          show.title, pdf.file, pdf.width, pdf.height)
      }

    }

    else {  # from stats
      .TwoGroup(y, x,
         n1, n2, m1, m2, s1, s2, from.data,
         Ynm, Xnm, X1nm, X2nm, brief, digits.d, conf.level,
         alternative, mmd, msmd, bw1, bw2, graph=FALSE)
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
           mmd=mmd, msmd=msmd,
           graph=graph, show.title=show.title, pdf.file=pdf.file,
           pdf.width=pdf.width, pdf.height=pdf.height)
    }
    else
      .OneGroup(x, Ynm, mu0, n, m, s, brief=brief, bw1=bw1,
           from.data=from.data, conf.level=conf.level,
           alternative=alternative, digits.d=digits.d,
           mmd=mmd, msmd=msmd,
           graph=graph, show.title=show.title, pdf.file=pdf.file,
           pdf.width=pdf.width, pdf.height=pdf.height, ...)
  }

  cat("\n")


}  # end tt.setup


#-----------------------------------

  alternative <- match.arg(alternative)

  # get actual variable name before potential call of data$x, could be NULL
  x.name <- deparse(substitute(x)) 

  # get data frame name
  dname <- deparse(substitute(data))

  # get conditions and check for data existing
  xs <- .xstatus(x.name, dname)
  is.frml <- xs$ifr
  from.data <- xs$fd
  in.global <- xs$ig 

  # see if the variable exists in the data frame
  if (from.data && !in.global && !is.frml) .xcheck(x.name, dname, data)
    
  # do analysis with tt.setup
  if (in.global) {
    if (is.function(x))  # var names that are R functions get assigned to global 
      tt.setup(eval(substitute(data$x)), Ynm=x.name, ...)  # 1-group
    else {
      if (!paired)
        tt.setup(x, y, ...)  # two vector form must come from global
      else {
        if (paired  &&  length(x)!=length(y))  {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
             "The two data vectors must be of the same size.\n\n")
        }
        diff <- eval(substitute(x)) - eval(substitute(y))
        tt.setup(diff, ...)
      }
    }
  }

  else if (is.frml) {
    f <- .tt.formula(x, y, data, ...)  # formula
    x <- f$x;  y <- f$y;  Ynm <- f$Ynm;  Xnm <- f$Xnm
    X1nm <- f$X1nm;  X2nm <- f$X2nm 
    tt.setup(x, y, ...)
    if (mean(x, na.rm=TRUE) > mean(y, na.rm=TRUE))
      invisible(list(value1=X1nm, group1=x, value2=X2nm, group2=y))
    else
      invisible(list(value1=X2nm, group1=y, value2=X1nm, group2=x))
  }

  else if (from.data)
    if (!paired)
      tt.setup(eval(substitute(data$x)), ...)  # 1-group
    else {
      diff <- eval(substitute(data$x)) - eval(substitute(data$y))
      tt.setup(diff, ...)
  }
  else
    tt.setup(...)  # analysis from stats

}
