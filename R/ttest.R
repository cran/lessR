ttest <-
function(x=NULL, y=NULL, dframe=mydata,
         n = NULL, m = NULL, s = NULL, mu0 = NULL, 
         n1 = NULL, n2 = NULL,  m1 = NULL, m2 = NULL, s1 = NULL, s2 = NULL, 
         Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", 
         brief=FALSE, digits.d = NULL, 
         conf.level = 0.95, mmd = NULL, msmd = NULL, 
         bw1 = "nrd", bw2 = "nrd", ...)  {


tt.default <-
function(x, y=NULL, ...) {


  cat("\n")
  if (missing(y))  no.y <- TRUE
  else if (is.null(y)) no.y <- TRUE else no.y  <- FALSE
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
    cat("Perhaps specify less digits to display with the  digits.d  parameter.\n")
  }

  if (two.gp) {
    if (from.data) { 

      if ( (length(x) < 2) || (length(y) < 2) )  {
       cat("\n"); stop(call.=FALSE, "\n","------\n",
         "Need at least two observations per sample.\n\n")
      }
     
      if ( !is.null(mmd) && !is.null(msmd) )  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
         "Specify only one of mmd and msmd as one implies the other.\n\n")
      }
      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))

      # Always put the group with the largest mean first
      if (mean(x, na.rm=TRUE) > mean(y, na.rm=TRUE))
        .TwoGroup(x, y, n1, n2, m1, m2, s1, s2,
         Ynm, Xnm, X1nm, X2nm, 
         brief, digits.d, 
         conf.level, mmd, msmd, 
         bw1, bw2, from.data)
      else {  # switch
        Xtmp <- X2nm
        X2nm <- X1nm
        X1nm <- Xtmp
        .TwoGroup(y, x, n1, n2, m1, m2, s1, s2,
         Ynm, Xnm, X1nm, X2nm, 
         brief, digits.d, 
         conf.level, mmd, msmd, 
         bw1, bw2, from.data)
      }

    }

    else {  # from stats
      .TwoGroup(y, x,
         n1, n2, m1, m2, s1, s2,
         Ynm, Xnm, X1nm, X2nm, 
         brief, digits.d, 
         conf.level, mmd, msmd, 
         bw1, bw2, from.data)
    }

  }  # end two.gp 

  else { # one group
    if (from.data) {
      Ynm <- x.name
      options(yname = x.name)
      .OneGroup(x, Ynm, mu0, brief=brief,
                from.data=from.data, conf.level=conf.level, digits.d=digits.d)
    }
    else
      .OneGroup(x, Ynm, mu0, n, m, s, brief=brief,
                from.data=from.data, conf.level=conf.level, digits.d=digits.d, ...)
  }

  cat("\n")


}  # end tt.default


#-----------------------------------

  # get actual variable name before potential call of dframe$x, could be NULL
  x.name <- deparse(substitute(x)) 

  # get data frame name
  dframe.name <- deparse(substitute(dframe))

  # get conditions and check for dframe existing
  xs <- .xstatus(x.name, dframe.name)
  is.frml <- xs$ifr
  from.data <- xs$fd
  in.global <- xs$ig 

  # see if the variable exists in the data frame
  if (from.data && !in.global && !is.frml) .xcheck(x.name, dframe.name, dframe)
    
  # do analysis with tt.default
  if (in.global) tt.default(x, y, ...)  # two vector form must come from global
  else
    if (is.frml) {
      f <- .tt.formula(x, y, dframe, ...)  # formula
      x <- f$x;  y <- f$y;  Ynm <- f$Ynm;  Xnm <- f$Xnm;  X1nm <- f$X1nm;  X2nm <- f$X2nm 
      tt.default(x, y, ...)
    }
  else 
    if (from.data) tt.default(eval(substitute(dframe$x)), Ynm=x.name, ...)  # 1-group
  else
    tt.default(...)  # analysis from stats

}
