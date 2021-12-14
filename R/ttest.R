ttest <-
function(x=NULL, y=NULL, data=d, rows=NULL, paired=FALSE,

         n=NULL, m=NULL, s=NULL, mu=NULL, 
         n1=NULL, n2=NULL, m1=NULL, m2=NULL, s1=NULL, s2=NULL, 

         Ynm="Y", Xnm="X", X1nm="Group1", X2nm="Group2", xlab=NULL, 

         brief=getOption("brief"), digits_d=NULL, conf_level=0.95,
         alternative=c("two_sided", "less", "greater"),
         mmd=NULL, msmd=NULL, Edesired=NULL, 

         show_title=TRUE, bw1="bcv", bw2="bcv",

         graph=TRUE, line_chart=FALSE,
         width=5, height=5, pdf_file=NULL, ...)  {       


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

  if (is.null(digits_d)) {
    if (from.data) digits_d <- .max.dd(x)
    else {
      digits_d <- 0
      if (!is.null(m)) if (.max.dd(m) > digits_d) digits_d <- .max.dd(m)   
      if (!is.null(s)) if (.max.dd(s) > digits_d) digits_d <- .max.dd(s)   
      if (!is.null(m1)) if (.max.dd(m1) > digits_d) digits_d <- .max.dd(m1)   
      if (!is.null(m2)) if (.max.dd(m2) > digits_d) digits_d <- .max.dd(m2)   
      if (!is.null(s1)) if (.max.dd(s1) > digits_d) digits_d <- .max.dd(s1)   
      if (!is.null(s2)) if (.max.dd(s2) > digits_d) digits_d <- .max.dd(s2)   
    }
    digits_d <- digits_d + 1
    if (digits_d < 3) digits_d <- 3
  }
  options(digits_d=digits_d)
  if (digits_d > 10) {
    cat("\nThese data contain", digits_d, "significant digits.\n")
    cat("Perhaps specify less digits to display with the  digits_d ",
        " parameter.\n\n")
  }

  if (two.gp) {
    options(yname=Ynm)

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
          Ynm, Xnm, X1nm, X2nm, brief, digits_d, 
          conf_level, alternative, mmd, msmd, Edesired, bw1, bw2,
          graph, xlab,
          line_chart, show_title, pdf_file, width, height)
      else {  # switch
        Xtmp <- X2nm
        X2nm <- X1nm
        X1nm <- Xtmp
        plt2 <- .TwoGroup(y, x, n1, n2, m1, m2, s1, s2, from.data,
          Ynm, Xnm, X1nm, X2nm, brief, digits_d, 
          conf_level, alternative, mmd, msmd, Edesired, bw1, bw2, 
          graph, xlab,
          line_chart, show_title, pdf_file, width, height)
      }

      for (i in (plot.i+1):(plot.i+plt2$i))
        plot.title[i] <- plt2$ttl[i-plot.i]
      plot.i <- plot.i + plt2$i


    }  # end from data

    else {  # from stats
      .TwoGroup(y, x,
         n1, n2, m1, m2, s1, s2, from.data,
         Ynm, Xnm, X1nm, X2nm, brief, digits_d, conf_level,
         alternative, mmd, msmd, Edesired, bw1, bw2,
         graph=FALSE, xlab=NULL, line_chart=FALSE)
    }
  #if (!brief) {
    #txt <- "Kelley and Lai's MBESS package]"
    #cat("\n[smd CI with Ken Kelley's ci.smd function from", txt, "\n") 
  #}

  }  # end two group

  else { # one group, including paired

    if (from.data) {
      if (!paired)
        Ynm <- x.name
      else {
        Ynm <- "Difference"
        mu <- 0
        options(df.name=NULL)
      }

      options(yname=x.name)
      plt1 <- .OneGroup(x, Ynm, mu, n=NULL, m=NULL, s=NULL, brief, bw1,
         from.data, conf_level, alternative, digits_d, mmd, msmd,
         Edesired, paired, graph, xlab, line_chart, show_title,
         pdf_file, width, height, ...)

    if (!is.null(plt1$i)) {
        for (i in (plot.i+1):(plot.i+plt1$i))
          plot.title[i] <- plt1$ttl[i-plot.i]
        plot.i <- plot.i + plt1$i
      }
    }  # end from data

    else  # from stats
       .OneGroup(x, Ynm, mu, n, m, s, brief, bw1,
         from.data, conf_level, alternative, digits_d, mmd, msmd,
         Edesired, paired, graph, xlab, line_chart, show_title,
         pdf_file, width, height, ...)
  }  # end one group

  # return number of plots to main
  if (plot.i > 0) return(list(i=plot.i, ttl=plot.title))

}  # end tt.setup


#-----------------------------------
# BEGIN
#-----------------------------------
  alternative <- match.arg(alternative)

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("digits.d", "conf.level", "show.title", "line.chart",
                "pdf.file")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
    d <- mydata 
    mydata.ok <- TRUE
  }
 
  if (missing(x)  &&  missing(n)  &&  missing(n1)) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Must specify a variable to analyze, or provide summary stats.\n\n")
  }
        
  if (!is.null(Edesired) && conf_level != 0.95) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Edesired calculation only applies to 95% confidence level.\n\n")
  }

  dots <- list(...)
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "mu0")  mu <- dots[[i]]
    }
  }

  # keep track of generated graphics
  plot.i <- 0
  plot.title  <- character(length=0)

  # get actual variable name before potential call of data$x, could be NULL
  if (!missing(x))
    x.name <- deparse(substitute(x)) 
  else
    x.name <- NULL
  if (!missing(y)) y.name <- deparse(substitute(y)) 

  if (!is.null(x.name)) {
    dfs <- .getdfs() 
    mydata.ok <- FALSE
    if (!is.null(dfs)) {
      if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
        d <- mydata
        df.name <- "mydata"
        mydata.ok <- TRUE
        options(dname = df.name)
      }
    }
    if (!mydata.ok) {
      df.name <- deparse(substitute(data))   # get name of data table
      options(dname = df.name)
    }
  }
  else
    df.name <- NULL
 
  if (!is.null(x.name)) if (exists(x.name, where=.GlobalEnv)) {
    if (is.data.frame(x)) {
      nm <- names((eval(substitute(x))))
      txt <- ifelse(length(nm)>1, "one of those variables", "that variable")
      nm2 <- "" 
      for (j in 1:length(nm)) nm2 <- paste(nm2, nm[j])
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "The argument to the ttest function you specified, ", df.name, ", is\n",
        "a data table, not a variable. A data table contains the data values\n",
        "for one or more variables. This data table references the variables:\n\n",
         "  ", nm2, "\n\n",
        "Perhaps you meant to analyze ", txt, ".\n\n")
    }
  }

  if (!missing(rows)) {  # subset rows
    r <- eval(substitute(rows), envir=data, enclos=parent.frame())
    if (!any(r)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No rows of data with the specified value of\n",
        "rows = ", deparse(substitute(rows)), "\n\n")
    }
    r <- r & !is.na(r)  # set missing for a row to FALSE
    data <- data[r,,drop=FALSE]
  }

  # get conditions and check for data existing
  if (!is.null(x.name)) {
 
    # if a tibble convert to data frame
    if (!is.null(dfs)) {
      if (df.name %in% dfs) {  # tibble to df
        if (any(grepl("tbl", class(data), fixed=TRUE))) {
          data <- data.frame(data, stringsAsFactors=FALSE)
        }
      }
    }

    xs <- .xstatus(x.name, df.name)
    is.frml <- xs$ifr
    if (is.frml) if (!mydata.ok) .nodf(df.name)  # check to see if df exists 
    from.data <- xs$fd
    in.style <- xs$ig 
    if (!missing(y)) .xstatus(y.name, df.name)  # just for a message on output 

    # see if the variable exists in the data frame
    if (from.data && !in.style && !is.frml) .xcheck(x.name, df.name, names(data))
  }

  else {
    is.frml <- FALSE
    from.data <- FALSE
    in.style <- FALSE
  }


  # --------------------------
  # do analysis with  tt.setup
  # plt is the returned number of plots generated 

  if (in.style || is.frml || from.data) {

    if (in.style) {
      if (is.function(x))  # var names that are R functions get assigned to style 
        plt <- tt.setup(eval(substitute(data$x)), Ynm=x.name, ...)  # 1-group
      else {  # not a function name
        if (!missing(y))
           y.l <- length(y)
        else
           y.l <- 0
        if (!paired)
          if (y.l == 0) {  # 1-group
            plt <- tt.setup(x, ...)
            data <- data.frame(x, stringsAsFactors=TRUE)
          }
          else  {# 2-group
            plt <- tt.setup(x, y,  ...)
            data <- data.frame(x, y, stringsAsFactors=TRUE)
          }
        else {  # paired
          if (length(x)!=length(y))  {
            cat("\n"); stop(call.=FALSE, "\n","------\n",
               "The two columns of data values must be the same size.\n\n")
          }
          diff <- y - x
          plt <- tt.setup(diff, ...)
          data <- data.frame(x, y, stringsAsFactors=TRUE)
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
      if (!is.numeric((eval(substitute(data$x))))) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "The variable to analyze must be numeric\n\n", 
          "The problem is that ", x.name, " does not have numeric values\n",
          "The first value of  ", x.name, "  is ", data[1,x.name],
          ", which is not numeric\n\n")

      }
      if (!missing(y)) {
        y.l <- length(eval(substitute(data$y)))
        if (!is.numeric((eval(substitute(data$y))))) { 
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Variables separated by a comma must both be numeric\n\n",
            "Perhaps use a tilde, ~, instead of a comma with a\n",
            "  numeric response variable before the tilde and after the\n",
            "  tilde a grouping variable with exactly two unique values\n\n")
        }
      }
      else
         y.l <- 0
      if (!paired) {
        if (y.l == 0)  # 1-group
          plt <- tt.setup(eval(substitute(data$x)), ...)
        else  # 2-group
          plt <- tt.setup(eval(substitute(data$x)), eval(substitute(data$y)),
                          ...)
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
  }  # in.style || is.frml || from.data 

  else
    tt.setup(...)  # analysis from stats

  # --------------------------


  if (paired) {  # scatter plot of both variables, need both vars so do here
    manage.gr <- .graphman()

    if (is.null(pdf_file)) {
      if (manage.gr) {
        if (!line_chart)
          dev.set(which=4)
        else
          dev.set(which=5)
      }
    }
    else
      pdf(file="PairedScatterPlot.pdf", width=width, height=height)

    if (in.style) {
      x.values <- x
      y.values <- y
    }
    else {
      x.values <- eval(substitute(data$x))
      y.values <- eval(substitute(data$y))
    }

    plot.i <- plot.i + 1
    plot.title[plot.i] <- "Differences from Equality"

    # construct x.call
    x.call <- data.frame(x.values, y.values, stringsAsFactors=TRUE)
    names(x.call) <- c(x.name, y.name)
    x.call <- data.matrix(x.call, rownames.force=FALSE)

    # construct y.call, need unique values for Cleveland dot plot
    is.unique <- logical(ncol(data))  # initialed to FALSE
    for(i in 1:ncol(data))
      if ((length(data[,i])==length(unique(data[,i]))) && !is.numeric(data[,i]))
        is.unique[i] <- TRUE
    unq <- which(is.unique)[1]  # choose 1st non-num variable with unique values
    if (!is.na(unq))
      y.call <- factor(data[, unq[1]])
    else { # no non-num var with unique values, so go to row.names
      if (row.names(data)[1] != "1")
        y.call <- factor(row.names(data))
      else
        y.call <- factor(row.names(data), levels=1:nrow(data))  # proper sort
    }

    # Cleveland two-variable dot plot
    theme <- getOption("theme")
    qual_pal <- ifelse (theme %in% c("gray", "white"), "grays", "hues")
    pt_fill <- getColors(qual_pal, n=2, output=FALSE)
    pt_color <- getColors(qual_pal, n=2, output=FALSE)
    .plt.main(data.frame(x.call, stringsAsFactors=TRUE),
              data.frame(y.call, stringsAsFactors=TRUE), cat.y=TRUE,
              fill=pt_fill, color=pt_color,
              shape=21, size=.8, ylab="", segments_y=TRUE)

    if (!is.null(pdf_file)) {
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
      return(invisible(list(value1=X1nm, group1=x, value2=X2nm, group2=y)))
    else
      return(invisible(list(value1=X2nm, group1=y, value2=X1nm, group2=x)))
  }

}

