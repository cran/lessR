Plot <-
function(x, y=NULL, data=d, rows=NULL,
         stat.x=c("data", "count", "proportion", "%"),
         stat.yx=c("data", "sum", "mean", "sd", "dev", "min", "median", "max"),
         n.cat=getOption("n.cat"),

         by=NULL, by1=NULL, by2=NULL,
         n.row=NULL, n.col=NULL, aspect="fill",

         theme=getOption("theme"),
         fill=NULL,
         color=NULL,
         trans=getOption("trans.pt.fill"),

         size=NULL, size.cut=NULL, shape="circle", means=TRUE,
         sort.yx=c("0", "-", "+"), segments.y=FALSE, segments.x=FALSE,
         jitter.x=0, jitter.y=0,

         ID="row.name", ID.size=0.85,
         MD.cut=0, out.cut=0, out.shape="circle", out.size=1,

         vbs.plot="vbs", vbs.size=0.9, bw=NULL, bw.iter=10,
         violin.fill=getOption("violin.fill"), 
         box.fill=getOption("box.fill"), 
         vbs.pt.fill="black",
         vbs.mean=FALSE, fences=FALSE,
         k=1.5, box.adj=FALSE, a=-4, b=3,

         radius=NULL, power=0.5,
         low.fill=NULL, hi.fill=NULL,

         smooth=FALSE, smooth.points=100, smooth.size=1,
         smooth.exp=0.25, smooth.bins=128,

         fit="off", fit.se=0.95, ellipse=0, 

         bin=FALSE, bin.start=NULL, bin.width=NULL, bin.end=NULL,
         breaks="Sturges", cumulate=FALSE, 

         run=FALSE, lwd=2, area.fill="transparent", area.origin=0, 
         center.line=c("default", "mean", "median", "zero", "off"),
         show.runs=FALSE, stack=FALSE,

         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         lab.adj=c(0,0), margin.adj=c(0,0,0,0),

         rotate.x=getOption("rotate.x"), rotate.y=getOption("rotate.y"),
         offset=getOption("offset"),

         xy.ticks=TRUE, value.labels=NULL, label.max=20, origin.x=NULL,
         legend.title=NULL,

         add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

         enhance=FALSE, eval.df=NULL, digits.d=NULL, quiet=getOption("quiet"),
         do.plot=TRUE, width=NULL, height=NULL, pdf.file=NULL, 
         fun.call=NULL, ...) {


# Note: stat is both object (dot plot) and statistic   

  if (is.null(fun.call)) fun.call <- match.call()
   
  # limit actual argument to alternatives, perhaps abbreviated
  stat.yx.miss <- ifelse (missing(stat.yx), TRUE, FALSE)
  sort.yx.miss <- ifelse (missing(sort.yx), TRUE, FALSE)
  sort.yx <- match.arg(sort.yx)

  stat.x <- match.arg(stat.x)
  stat.yx <- match.arg(stat.yx)
  center.line <- match.arg(center.line)
  data.do <- ifelse ((stat.x == "data") && (stat.yx == "data"), TRUE, FALSE)

  proportion <- ifelse (stat.x == "proportion", TRUE, FALSE)   # old signal
  prop <- ifelse (stat.x == "proportion", TRUE, FALSE)   # old signal (Trellis)

  shiny <- ifelse (isNamespaceLoaded("shiny"), TRUE, FALSE) 
  if (is.null(eval.df))  # default values
    eval.df <- ifelse (shiny, FALSE, TRUE) 

  vbs.plot <- tolower(vbs.plot)
  violin <- ifelse (grepl("v", vbs.plot), TRUE, FALSE)
  box <- ifelse (grepl("b", vbs.plot), TRUE, FALSE)

  iter.details <- ifelse (missing(bw.iter), FALSE, TRUE)

  k.iqr <- k   # k is a function name, so do not use internally

  cat.x <- NULL;  num.cat.x <- NULL;  cat.y <- NULL;  num.cat.y <- NULL; 

  pt.fill <- fill
  pt.color <- color
  pt.trans <- trans
  bar.color <- getOption("bar.color")
  bubble.text <- getOption("bubble.text.color")
  segment.color <- getOption("segment.color")
  ID.color <- getOption("ID.color")

  ellipse.fill <- getOption("ellipse.fill")
  ellipse.color <- getOption("ellipse.color")
  ellipse.lwd <- getOption("ellipse.lwd")

  if (fit == "ls") fit <- "lm"  # new value
  fit.color <- getOption("fit.color")
  fit.lwd <- getOption("fit.lwd")
  se.fill <- getOption("se.fill")

  out.fill <- getOption("out.fill")
  out.color <- getOption("out.color")
  out2.fill <- getOption("out2.fill")
  out2.color <- getOption("out2.color")

  panel.fill <- getOption("panel.fill")
  panel.color <- getOption("panel.color") 
  grid.color <- getOption("grid.color")

  lab.cex <- getOption("lab.cex")
  axis.cex <- getOption("axis.cex")
  main.cex <- getOption("main.cex")

  add.cex <- getOption("add.cex")
  add.lwd <- getOption("add.lwd")
  add.lty <- getOption("add.lty")
  add.color <- getOption("add.color")
  add.fill <- getOption("add.fill")
  add.trans <- getOption("add.trans")

  # missing function only reliable if arg not modified, so capture 
  x.miss <- ifelse (missing(x), TRUE, FALSE)
  y.miss <- ifelse (missing(y), TRUE, FALSE)
  by1.miss <- ifelse (missing(by1), TRUE, FALSE)
  by2.miss <- ifelse (missing(by2), TRUE, FALSE)
  by.miss <- ifelse (missing(by), TRUE, FALSE)
  size.miss <- ifelse (missing(size), TRUE, FALSE)
  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  seg.y.miss <- ifelse (missing(segments.y), TRUE, FALSE)  # for Cleveland plot
  seg.x.miss <- ifelse (missing(segments.x), TRUE, FALSE)
  ellipse.miss <- ifelse (missing(ellipse), TRUE, FALSE)
  fit.miss <- ifelse (missing(fit), TRUE, FALSE)
  MD.miss <- ifelse (missing(MD.cut), TRUE, FALSE)
  ID.miss <- ifelse (missing(ID), TRUE, FALSE)
  lwd.miss <- ifelse (missing(lwd), TRUE, FALSE)
  out.shape.miss <- ifelse (missing(out.shape), TRUE, FALSE)
  j.y.miss <- ifelse (missing(jitter.y), TRUE, FALSE)
  j.x.miss <- ifelse (missing(jitter.x), TRUE, FALSE)
  bw.miss <- ifelse (missing(bw), TRUE, FALSE)
  n.col.miss <- ifelse (missing(n.col), TRUE, FALSE)
  n.row.miss <- ifelse (missing(n.row), TRUE, FALSE)
  add.miss <- ifelse (missing(add), TRUE, FALSE)


  stat <- ifelse(y.miss, stat.x, stat.yx) 

  if (!missing(a) || !missing(b)) box.adj <- TRUE

  if (missing(vbs.size)) if (!violin)  # wider box if no violin
    vbs.size <- ifelse (y.miss || by1.miss, vbs.size*3.75, vbs.size*5)
 
  # "off" substitutes for official value of "transparent"
  fill[which(fill == "off")] <- "transparent"
  color[which(color == "off")] <- "transparent"
  ellipse.color[which(ellipse.color == "off")] <- "transparent"
  ellipse.fill[which(ellipse.fill == "off")] <- "transparent"
  add.fill[which(add.fill == "off")] <- "transparent"
  add.color[which(add.color == "off")] <- "transparent"

  if (enhance) {
    if (ellipse.miss) ellipse <- 0.95
    if (MD.miss) MD.cut <- 6
    if (add.miss) add <- "means"
    if (fit.miss) fit <- "lm"
  }

  txdif <- ""  # used to transfer output for diffs of two vars Cleveland sp
  
  xlab.adj <- lab.adj[1];   ylab.adj <- lab.adj[2]
  tm.adj <- margin.adj[1];  rm.adj <- margin.adj[2]
  bm.adj <- margin.adj[3];  lm.adj <- margin.adj[4]
  
  date.ts <- FALSE  # default is not a time series
  freq.poly <- FALSE  # default is not a frequency polygon

  if (show.runs) run <- TRUE

  if (!missing(fit.se)) if (missing(fit))  fit <- "loess"
  if (is.logical(fit))
    fit.ln <- ifelse (fit, "loess", "off")
  if (is.character(fit)) {
    if (!(fit %in% c("loess", "lm", "off", "ls"))) {  # "ls" deprecated
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "fit applies only for  loess  or  lm  (linear model)\n\n")
    }
    fit.ln <- fit  # fit.ln passed to .plt.main
  }

  # any bin parameter activates bins for VBS plot, or freq.poly=TRUE
  if (!missing(breaks)  ||  !missing(bin.start)  ||  !missing(bin.width)  ||
      !missing(bin.end))
    bin <- TRUE

  if (stack) if (fill.miss) fill <- getOption("violin.fill")  # default 

  if (!is.null(pdf.file))
    if (!grepl(".pdf", pdf.file)) pdf.file <- paste(pdf.file, ".pdf", sep="")

  if (!data.do) if (is.null(pt.trans)) pt.trans <- 0  # trans, so dot plot

  # set object 
  if (!missing(radius) || !missing(power)) {
    object <- "bubble"  # any bubble parameter activates a bubble plot
  }
  else
    object <- "default"
  if (run) object <- "both"


  # --------------
  # process shapes

  if (shape[1] == "sunflower")
    object <- "sunflower"

  else {
    bad.shape <- NULL
    shapes <- c("circle", "square", "diamond", "triup", "tridown")
    shapes.all <- c(shapes, c(21:25), letters, LETTERS, 0:9, "+", "*", "#",
                    "%", "!", "=", "-", "&", "$", "?", "|", ">", "<", "@", ".")

    num.flag <- FALSE
    for (i in 1:length(shape)) {
      if (!(shape[i] %in% shapes.all)) {
        bad.shape <- shape[i]
      }
      else if (shape[i] %in% shapes) {
        shape[i] <- which(shape[i] == shapes) + 20
        num.flag <- TRUE
      }
    }
    if (num.flag) shape <- as.numeric(shape)

    num.flag <- FALSE
    if (!(out.shape %in% shapes.all))
      bad.shape <- out.shape 
    else {
      if (out.shape %in% shapes)  # outlier point
        out.shape <- which(out.shape == shapes) + 20
        num.flag <- TRUE
    }
    if (num.flag) out.shape <- as.numeric(out.shape)

    if (!is.null(bad.shape)) {
      message("\nValid shapes")
      message("------------")
      for (j in 1:length(shapes)) message(shapes[j])
      message("all uppercase and lowercase letters")
      message("all digits")
      message("+ * # % ! = - & $ ? | < > @")
      cat("\n")
      stop(call.=FALSE, "\n","------\n",
      "Not a valid shape: ", bad.shape, "\n\n")
    }
  }
    

  # ------
  # see if dated or inconsistent parameter values
  .param.old(...)
  .plt.bad(x.miss, y.miss, stat.yx, breaks, bin.start, n.row, n.col,
           MD.cut, out.cut, fit.se, ...)

  # ---------------------------------
  # get variable and data frame names
  
  data.miss <- ifelse (missing(data), TRUE, FALSE) 


  # let deprecated mydata work as default
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
    df.name <- deparse(substitute(data))  # get name of data table
    options(dname = df.name)
  }
 
  # if a tibble convert to data frame
  if (!is.null(dfs)) {
    if (df.name %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data, stringsAsFactors=FALSE)
      }
    }
  }

  if ((missing(data) && shiny))  # force evaluation (not lazy) if data not specified
    data <- eval(substitute(data), envir=parent.frame())


  x.name <- deparse(substitute(x), width.cutoff = 120L)
  options(xname = x.name)

  x.in.global <- .in.global(x.name)  # see if in global, includes vars lists

  
  #  get data to be analyzed into data.x data frame

  # process row.names if specified
  if (x.name == "row.names") {
    # retain order of row names, otherwise will be alphabetical
    data.x <- data.frame(factor(row.names(data), levels=row.names(data)))
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label
    cat.x <- TRUE
  }

  # x not in global env, in df, specify data= forces to data frame
  else if (!x.in.global) {
    if (eval.df) {
      if (!mydata.ok) .nodf(df.name)  # check to see if df exists 
      .xcheck(x.name, df.name, names(data))  # stop if x an expression
    }
    data.vars <- as.list(seq_along(data))
    names(data.vars) <- names(data)
    x.col <- eval(substitute(x), envir=data.vars)  # col num of each var
    
    if (!missing(rows)) {  # subset rows
      r <- eval(substitute(rows), envir=data, enclos=parent.frame())
      r <- r & !is.na(r)  # set missing for a row to FALSE
      data <- data[r,,drop=FALSE]
    }
    if (!("list" %in% class(data))) {
      data.x <- data[, x.col]
      data.x <- data.frame(data.x)
    }      
    else {  # class of data is "list"
       data.x <- data.frame(data[[x.col]])
    }
    if (is.numeric(x.col))
      names(data.x) <- names(data.vars)[x.col]
    else
      names(data.x) <- x.col  # if x a vector, x.col can return names
    #data.miss <- FALSE  # use d even if not specified (default)
  }  # end x in df

  # x is in the global environment (vector or data frame)
  # can only access x directly if it is not in a data frame
  else if (is.data.frame(x)) { # x a data frame
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need to specify variables for analysis,\n",
        "not a full data frame\n\n")
  }
      
  else if (is.ts(x)) {  # time series in global # just the dates for x var
    data.x <- data.frame(.ts.dates(x))
    names(data.x) <- "date"
    date.ts <- TRUE
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label

    # flip x to y
    nc <- ifelse (is.matrix(x), ncol(x), 1)
    if (nc == 1)
      y.call <- x
    else {
      y.call <- x[,1]
      if (nc > 1) for (i in 2:nc) y.call <- cbind(y.call, x[,i])
    }
    y.call <- data.frame(y.call)
    y.col <- ncol(y.call)
    cat.y <- FALSE
    if (nc == 1) {
      names(y.call) <- x.name
      y.name <- x.name
    }
    else
      names(y.call) <- colnames(x)

    y.name <- deparse(substitute(x))
    options(yname = y.name)
  }
      
  else {  # x a not ts vector in global
    if (!is.function(x))
      data.x <- data.frame(x)  # x is 1 var
    else
      data.x <- data.frame(eval(substitute(data$x)))  # x is 1 var
    names(data.x) <- x.name
  }
    
  n.x_var <- ncol(data.x)  # number of x-variables
  x.call <- data.x
    
  # end get data.x
    

  # get data.x data to be analyzed into x.call, except for BPFM
  BPFM <- FALSE
  spmat <- FALSE

  # just one x variable for now, a vector of cat or num values
  if (n.x_var == 1) {
    if (!is.ts(date.ts)) {
      date.ts <- ifelse (.is.date(x.call[,1]), TRUE, FALSE) 
      if (grepl("POSIX",  class(x.call[,1]), fixed=TRUE)[1])
        x.call[,1] <- as.Date(x.call[,1])
    }
    if (!date.ts) {
      nu <- length(unique(na.omit(x.call[,1])))
      num.cat.x <- .is.num.cat(x.call[,1], n.cat)  # small num of int values
      if (jitter.x > 0) num.cat.x <- FALSE
    }
    else {  # process ts
      nu <- length(unique(x.call[,1]))
      num.cat.x <- FALSE
    }

    
    if (!is.factor(x.call[,1])) {
      eq.int <- TRUE
      d.x <- diff(x.call[,1]) 
      eq.int <- ifelse (any(is.na(x.call[,1])), FALSE, TRUE)
      if (eq.int) {  # no missing allowed
        for (i in 2:(length(d.x)))
          if ((abs(d.x[i-1] - d.x[i]) > 0.0000000001)) eq.int <- FALSE

        if (!num.cat.x && is.integer(x.call[,1]) &&
            !date.ts && nu<= n.cat && eq.int) {
          cat("\n")
          cat(">>> ", x.name, " has only only ", nu, " unique ",
              "integer values, but not equally spaced,\n",
              "      so treat as numerical in this analysis\n",
              "   Maybe convert to an R factor to treat as categorical\n",
              sep="")
        }
      }
    }

    if (num.cat.x && !quiet) .ncat("Plot", x.name, nu, n.cat)
    cat.x <- ifelse (num.cat.x || is.factor(x.call[,1]), TRUE, FALSE)

    if (cat.x  &&  object == "both") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "A run chart applies only to continuous variables\n",
        x.name, " is a categorical variable\n\n")
    }
  }

  # more than one x-variable
  else {
    # no y, see if eligible for BPFM where all selected vars are cat
    # multiple lines of a continuous x-variable also can occur
    if (y.miss  &&  object != "both") {

      is.cat <- logical(length=length(x.col))
      is.nmb <- logical(length=length(x.col))
      for (i in 1:length(x.col)) {  # see if variables are all categorical
        nu <- nrow(unique(na.omit(data.x[,i])))

        num.cat <- .is.num.cat(data.x[,i], n.cat)
        is.string <- is.factor(data.x[,i]) || is.character(data.x[,i])
        is.cat[i] <- ifelse (num.cat || is.string, TRUE, FALSE)

        is.nmb[i] <- ifelse (!num.cat &&  is.numeric(data.x[,i]), TRUE, FALSE)

      }  # end for

      if (all(is.cat)) {
        BPFM <- TRUE
        cat.x <- TRUE
        spmat <- FALSE
      }
      else if (all(is.nmb)) {
        spmat <- TRUE
        cat.x <- FALSE
        BPFM <- FALSE
      }

      else {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Multiple x-variables must all be numeric or all categorical\n\n",
          "A categorical variable is either an R factor variable,\n",
          "  or is numeric with not more than  n.cat  unique values\n\n",
          "Can set  n.cat  locally when calling Plot,\n",
          "  or globally with the function:  global\n\n")
      }
    }  # y is missing and not a line graph
    else
      cat.x <- FALSE
   
  }  # end more than 1 x-variable


   if (!is.factor(x.call[,1])) if (cat.x) x.call[,1] <- factor(x.call[,1])

 
  if (!BPFM)
    nrows <- ifelse (is.matrix(x.call[,1]), nrow(x.call[,1]), length(x.call[,1]))
  else
    nrows <- nrow(data.x)


  if (nrows > 2499) if (missing(smooth)) smooth <- TRUE

  if(date.ts) object <- "both"


  #-----------
  # evaluate y

  if (!y.miss) {
    # get actual variable name before potential call of data$y
    y.name <- deparse(substitute(y))
    options(yname = y.name)
  
    y.in.global <- .in.global(y.name)  # see if in global, includes vars lists

    if (deparse(substitute(y)) == "row.names") {
      # retain order of row names, otherwise will be alphabetical
      y.call <- factor(row.names(data), levels=row.names(data))
      if (is.null(ylab)) ylab <- ""  # unless specified, drop the axis label
      cat.y <- TRUE
      data.y <- data.frame(y.call)
    }
      
    # y not in global env, in df, specify data= forces to data frame
    else if (!y.in.global) {
        if (eval.df) .xcheck(y.name, df.name, names(data))  # var in df?
        data.vars <- as.list(seq_along(data))  # even if only a single var
        names(data.vars) <- names(data)  # all data in data frame
        y.col <- eval(substitute(y), envir=data.vars)  # col num selected vars
        
        if (!("list" %in% class(data))) {
          data.y <- data[, y.col]
          data.y <- data.frame(data.y)
       }      
       else {  # class of data is "list"
          data.y <- data.frame(data[[y.col]])
        }
      if (is.numeric(y.col))
        names(data.y) <- names(data.vars)[y.col]
      else
        names(data.y) <- y.col
      }  # end global y

      else if (is.data.frame(y)){ # y is in the global env (vector or data frame)
          # y a data frame
          data.y <- y
      }
       
      else {  # y a vector in global
        if (!is.function(y)) {
          .xstatus(y.name, df.name, quiet)  # in global note
          data.y <- data.frame(y)  # y is 1 var
        }
        else
          data.y <- data.frame(eval(substitute(data$y)))  # y is 1 var
        names(data.y) <- y.name
      }

      n.y_var <- ncol(data.y)  # number of y-variables
      y.call <- data.y
    # end get data.y
 

      if (ncol(y.call) == 1) { # y is one variable
        # y.call <- data.y[,1]

        nu <- length(unique(na.omit(y.call[,1])))
        num.cat.y <- .is.num.cat(y.call[,1], n.cat)

        if (!num.cat.y && is.integer(y.call[,1]) && nu <= n.cat) {
          cat("\n")
          cat(">>> ", y.name, " has only only ", nu, " unique ",
              "integer values, but not equally spaced,\n",
              "      so treat as numerical in this analysis\n",
              "    Maybe convert to an R factor to treat as categorical\n",
              sep="")
        }

        if (num.cat.y && !quiet) .ncat("Plot", y.name, nu, n.cat)
        cat.y <- ifelse (num.cat.y || is.factor(y.call[,1]), TRUE, FALSE)
      }
      
      else {  # 2 or more y vars
        cat.y <- FALSE  #  multiple y-vars must be numerical
        if (missing(ylab)) ylab <- ""  # use legend instead
        y.call <- data.frame(data.y)
        # y.call <- data.matrix(data.y, rownames.force=FALSE)
      }
  }  # end y not missing
  
  else { # missing y
    if (!date.ts) y.call <- NULL
  }  # end missing y

  if (cat.x  &&  run) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Run chart only applies to a numerical variable\n\n")
  }


  # ---------------------------------
  # ellipse, fit line stop conditions
  if (ellipse[1] > 0) {
    many.y <- FALSE
    if (!y.miss) if (is.matrix(y.call)) many.y <- TRUE
    if ((ncol(data.x)>1 || many.y)  ||  cat.x  ||  cat.y) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "An ellipse only applies to analysis of a single y-variable \n",
        "  which is continuous\n\n")
    }
    if (y.miss) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need a y-variable to compute an ellipse\n\n")
    }
  }
  
  if (y.miss  &&  (fit.ln != "off")  &&  n.x_var == 1  &&  !date.ts) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Fit line only applicable if just one x variable, if y is present\n\n")
  }


  if (!y.miss) if (cat.x && !cat.y) {
    cat("\n>>> Note\n",
        "The integrated Violin/Box/Scatterplot (VBS) for ", y.name, "\n",
        "at each level of ", x.name, " is only obtained if the categorical\n",
        "variable is the variable listed second, that is, the y-variable.\n\n",
        "This ordering with ", x.name, " listed first yields the\n",
        "scatterplot and the associated means, but no VBS plot.\n", sep="")
  }


  # ------------------------------------------
  # ------------------------------------------
  # master control funnel for type of analysis
  # ------------------------------------------

  lx <- length(x.call[,1])
  ly <- length(y.call[,1])
  n.ux <- length(unique(x.call[,1]))
  n.uy <- length(unique(y.call[,1]))
  x.unique <- ifelse (n.ux == lx, TRUE, FALSE)
  y.unique <- ifelse (n.uy == ly, TRUE, FALSE)

  # f replications from two variable cross-tabs, then go bubble plot 
  if (!y.miss) if (n.cat > 0) {  # can set n.cat > 0 to force discrete
    if (n.ux < 12  &&  n.uy < 12)  {
      tbl <- table(x.call[,1], y.call[,1])
      if (max(tbl) > 1) {
        cat.x <- TRUE
        num.cat.x <- TRUE
        cat.y <- TRUE
        num.cat.y <- TRUE
      }
    }
  }

  if (data.do  &&  n.x_var == 1) {
    if (y.miss) {  # y missing
      if (!cat.x) {  # continuous x
        if (!run && !date.ts) {  # not a run chart or ts
        Trellis <- TRUE
        c.type <- "cont"
       }
       else {  # run chart
          if (by1.miss) {  # single panel
            Trellis <- FALSE
          }
          else {  # by1 present
            Trellis <- TRUE
            c.type <- "contcont"  
          }
        }
      }  # end !cat.x
      else {  # single cat variable
        if (by1.miss)
          Trellis <- FALSE
        else {  # by1 activated
          Trellis <- TRUE
          c.type <- "dot"
        }
      }
    }  # end y.miss 

    else {  # y present
      if (!cat.x) {
        if (!cat.y) {  # two continuous vars
          if (by1.miss)
            Trellis <- FALSE
          else {  # by1 is set
            Trellis <- TRUE
            c.type <- "contcont"
          }
        }  # end !cat.y
        else {  # y is cat
          if (y.unique) {  # Cleveland
            if (by1.miss)
              Trellis <- FALSE
            else {
              cat("\n"); stop(call.=FALSE, "\n","------\n",
                "Currently, Trellis plots not available for dot plots \n",
                "with unique values for the categorical variable\n\n")
            }
          }
          else {
            if (box) {
              cat("\n"); stop(call.=FALSE, "\n","------\n",
                "Currently, Trellis box plots not available for a \n",
                "y categorical variable, set to  by1  instead\n\n")
            }
            Trellis <- TRUE  # cat-cont
            c.type <- "contcat"
          }
        }
      }
      else  {  # x is cat
        if (!cat.y) {  # y is num
          if (by1.miss) {
            Trellis <- FALSE
          }
          else {
            cat("\n"); stop(call.=FALSE, "\n","------\n",
                "\n>>> Violin plot does not currently work with this",
                " variable order\n",
                "    Switch variable order to get violin plots\n\n", sep="")
            #Trellis <- TRUE
            #c.type <- "contcont"
            #violin <- FALSE  
          }
        }
        else { # y is cat
          Trellis <- FALSE
        }
      }  # x is cat
    }  # y present
  }  # end: data.do  &&  n.x_var == 1

  else {  # for all analysis of stat transformed data
    if (n.x_var == 1) {
      if (!cat.x) {
        Trellis <- FALSE
        violin <- FALSE
      }
      else {  # is cat.x
        if (by1.miss) {
          Trellis <- FALSE
        }
        else {
          Trellis <- TRUE
          c.type <- "dot"
        }
      }  # end is cat.x
    }  # end n.x_var=1

    else {  # multiple x's
      Trellis <- FALSE
    }
  }
  # end funnel for master control of analysis type

  if (is.logical(ellipse))
    ellipse <- ifelse (ellipse, 0.95, 0.00)
  if (ellipse[1] > 0  &&  !Trellis) {
    txt <- "[Ellipse with Murdoch and Chow's function ellipse"
    cat(txt, "from the ellipse package]\n")
  } 


  # evaluate by
  #------------
  if (!missing(by)) {

    # get actual variable name before potential call of data$x
    by.name <- deparse(substitute(by))
    options(byname = by.name)

    # get conditions and check for data existing
    xs <- .xstatus(by.name, df.name, quiet)
    in.global <- xs$ig

    # see if var exists in data frame, if x not in global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(by.name, df.name, names(data))

    if (!in.global)
      by.call <- eval(substitute(data$by))
    else {  # vars that are function names get assigned to global
      by.call <- by
      if (is.function(by.call)) by.call <- eval(substitute(data$by))
    }

    if (!is.factor(by.call)) by.call <- factor(by.call)
  }

  else
   by.call <- NULL

  
  # evaluate by1
  #-------------
  if (!missing(by1)) {

    # get actual variable name before potential call of data$x
    by1.name <- deparse(substitute(by1))
    options(by1name = by1.name)

    # get conditions and check for data existing
    xs <- .xstatus(by1.name, df.name, quiet)
    in.global <- xs$ig

    # see if var exists in data frame, if x not in global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(by1.name, df.name, names(data))

    if (!in.global)
      by1.call <- eval(substitute(data$by1))
    else {  # vars that are function names get assigned to global
      by1.call <- by1
      if (is.function(by1.call)) by1.call <- eval(substitute(data$by1))
    }

    if (!is.factor(by1.call)) by1.call <- factor(by1.call)
  }

  else
   by1.call <- NULL



  # evaluate by2
  #-------------
  if (!missing(by2)) {

    # get actual variable name before potential call of data$x
    by2.name <- deparse(substitute(by2))
    options(by2name = by2.name)

    # get conditions and check for data existing
    xs <- .xstatus(by2.name, df.name, quiet)
    in.global <- xs$ig

    # see if var exists in data frame, if x not in global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(by2.name, df.name, names(data))

    if (!in.global)
      by2.call <- eval(substitute(data$by2))
    else {  # vars that are function names get assigned to global
      by2.call <- by2
      if (is.function(by2.call)) by2.call <- eval(substitute(data$by2))
    }

    if (!is.factor(by2.call)) by2.call <- factor(by2.call)
  }

  else
   by2.call <- NULL



  # evaluate size (NULL, numeric constant or a variable)
  #--------------
  if (!size.miss) {
    size.name <- deparse(substitute(size))
    in.df <- ifelse (exists(size.name, where=data), TRUE, FALSE)

    if (in.df) {
      size <- eval(substitute(data$size))
      if (!is.numeric(size)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Variable ", size.name, " must be numeric\n\n",
          "Perhaps use: by=", size.name, "\n\n")
      }
      options(sizename = size.name) # for later access
      object <- "bubble"
      if (is.null(size.cut)) size.cut <- ifelse (cat.x, 1, 2)
    }

    else  # size is not a variable
      size.cut <- FALSE
  }
  else
    if (missing(size.cut)) size.cut <- TRUE

  if (!grepl("s", vbs.plot)) size <- 0

  # evaluate ID 
  #------------
  get.ID <- FALSE
  if (y.miss && !cat.x) get.ID <- TRUE  # VBS plot 
  if (!is.null(add)) if (add[1] == "labels") get.ID <- TRUE 
  if (!y.miss) if (!cat.x && !cat.y && (MD.cut>0 || out.cut>0))
    get.ID <- TRUE 
  ID.name <- noquote(deparse(substitute(ID)))  # puts quotes around the name
  ID.name <- gsub("\"", "", ID.name)
  if (get.ID) {
    if (ID.name == "row.name") {
      if (df.name %in% ls(name=.GlobalEnv)  &&  !x.in.global)
        ID.call <- factor(row.names(data), levels=row.names(data))
      else
        ID.call <- 1:nrows
    }

    else {  # allow a specified variable in data table to be the ID
      .xstatus(ID.name, df.name, quiet)  # check for data existing
      .xcheck(ID.name, df.name, names(data))  # var exists in data frame?
      ID.call <- eval(substitute(data$ID))
    }
  }
  else
    ID.call <- NULL


  # -----------  x, y, by and size variables established ------------
  # -----------------------------------------------------------------

    #get n.levels
    n.xcol <- ncol(x.call)
    n.ycol <- ifelse (y.miss, 0, ncol(y.call))
    nn.col <- max(n.xcol, n.ycol)  # n.col goes into lattice, do not change
    if (!is.null(by.call))
      n.by <-  nlevels(by.call)
    else
      n.by <- 0
    n.levels <- max(nn.col, n.by)

    if (!y.miss) if (cat.y) n.levels <- length(unique(na.omit(y.call[,1])))

    if (!missing(violin.fill)) {
      if (length(violin.fill) == 1)
         violin.fill <- .color.range(violin.fill, n.levels)
    }
    if (!missing(box.fill)) {
      if (length(box.fill) == 1)
         box.fill <- .color.range(box.fill, n.levels)
    }
    # end n.levels, only referred to in preceding block

  if (is.null(height)) { 
    if (is.null(y.call)  &&  !BPFM  
        &&  data.do  &&  object != "both"  &&  !.is.date(x.call))
      height <- ifelse (is.null(main), 4, 4.6)  # narrow for 1-D dot plot
    else
      height <- 6

    if (BPFM)  # more than 7 variables, make plot extra long
      height <- height + ((ncol(data.x) - 7) * 0.5)
  }

  if (is.null(width)) width <- 6
  if (!by.miss) width <- width + .85  # wider plot  


  # --------
    # adjust by, manage regular-R or PDF graphics window size
  if (!Trellis && !shiny)
    .opendev(pdf.file, width, height)  # prepare plot window, dev or pdf


  # ------------------------------------------------
  # set object and values where needed

  # prep 1-variable bubble plot to call regular scatter plot function
  # y.call to 0
  if (is.null(y.call)  &&  cat.x  &&  n.x_var == 1  &&  data.do) {
    y.call <- data.frame(rep(0, nrow(x.call)))
    cat.y <- FALSE
    if (object == "default") object <- "bubble"
  }

  # if numeric x is sorted with equal intervals, set as line chart
  # x.call does not exist for BPFM
  if (!cat.x) if (is.numeric(x.call[,1])  &&  nrows > 2) {
    if (object == "default") {
      eq.int <- ifelse (any(is.na(x.call[,1])), FALSE, TRUE)
      if (eq.int) {
        d.x <- diff(x.call[,1])  # only look at first x-variable
        for (i in 2:(length(d.x)))
          if ((abs(d.x[i-1] - d.x[i]) > 0.0000000001)) eq.int <- FALSE
        rm(d.x)
      }  # also no y missing

      if(!is.unsorted(x.call) && eq.int && sum(is.na(y.call))==0) {
        object <- "both"
        if (is.null(size)) size <- 0  # by default, just plot a line w/o points
      }
    }
  }

  # if numeric y is sorted with equal intervals, line chart
  if (is.numeric(y.call)  &&  nrows > 2) {
    if (object == "default") {
      if (sum(is.na(y.call)) > 0)
        eq.int <- FALSE  # missing data in x present
      else {
        eq.int <- TRUE
        if (is.matrix(y.call))
          d.y <- diff(y.call[,1])  # only look at first x-variable
        else
          d.y <- diff(y.call) 
        for (i in 2:(length(d.y)))
          if ((abs(d.y[i-1] - d.y[i]) > 0.0000000001)) eq.int <- FALSE
        rm(d.y)
      }  # also no y missing

      if (!is.unsorted(y.call) && eq.int && sum(is.na(x))==0) object <- "both"
    }
  }

  # set point or bubble plot
  if (object == "default") {  # set default
    if (!y.miss) {
      object <- "point"
      if (data.do) if (cat.x && cat.y) object <- "bubble"
    }
    else if (stat.x %in% c("count", "proportion", "%")) {
      object <- "point"
    }
    else {
      if (data.do) object <- ifelse (cat.x, "bubble", "point")
      if (BPFM) object <- "bubble"  # BPFM
    }
  }

  if (y.miss  &&  !date.ts  &&  object != "bubble") {  

    if (!cat.x) { 

      # run chart prep of x.call and y.call
      if (object == "both"  &&  data.do) {  # run chart
        y.call <- x.call
        cat.y <- cat.x
        options(yname=x.name)
        
        options(xname="Index")
        x.call <- data.frame(1:nrow(x.call))
        names(x.call) <- "Index"
      }

      else if (stat.x %in% c("count", "proportion", "%")) {  # frequency polygon

        ssstuff <- .ss.numeric(x.call[,1], digits.d=digits.d, brief=TRUE)
       
        values <- NULL
        hist.cumul <- ifelse(cumulate, "on", "off")
        reg <- "snow2"  # applies to cumulative histogram
        h <- .hst.main(x.call[,1], pt.fill, pt.color, pt.trans, reg,
           rotate.x, rotate.y, offset,
           breaks, bin.start, bin.width, bin.end,
           proportion, values, hist.cumul,
           xlab, ylab, main, sub, quiet=quiet, do.plot=FALSE,
           fun.call=NULL, ...) 

        n.cat <- 0  # not many midpoints, do not want to trigger num.cat
        x.call <- h$mids
        y.call <- h$counts
        if (stat.x == "count")
          ylab <- paste("Count of", x.name)
        else {
          y.call <- y.call / sum(y.call)
          ylab <- paste("Proportion of", x.name)
        }

        # last assignment of object, now determined
        object <- "both"  # do freq poly as a line chart
        freq.poly <- TRUE  # need to indicate fill possibility
        center.line <- "off"  # not meaningful here
  
        cat.x <- FALSE
        cat.y <- FALSE
        
        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"

          fncl <- .fun.call.deparse(fun.call) 
          fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
          fncl <- gsub(" = ", "=", fncl)

          fc <- ""
          if (!grepl("size", fncl))
            fc <- paste(fc, ", size=0", sep="")
          if (nzchar(fc)) {
            fc <- gsub(" = ", "=", fc)
            fc <- paste(fncl, fc, ")   # just line segments, no points", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          }

          bw.new <- pretty((x.call[2] - x.call[1]) / 1.5)  # arbitrary new bw
          fc <- ""
          if (!grepl("bin.width", fncl))
            fc <- paste(fc, ", bin.width=", as.character(bw.new[1]), sep="")
          if (nzchar(fc)) {
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, fc, sep="")
          }
            
          txsug <- .rm.arg.2(" x=", txsug) 
          txsug <- .rm.arg.2("(x=", txsug) 
        }

        if (!quiet) {
    
          txss <- ssstuff$tx  # stats output before reduce data
        
          txdst <- h$ttx
          if (is.null(txdst)) txdst <- ""

          bx <- .bx.stats(x.call, box.adj, k.iqr, a, b)
          txotl <- bx$txotl

          class(txsug) <- "out"
          class(txss) <- "out"
          class(txdst) <- "out"
          class(txotl) <- "out"
          output <- list(out_suggest=txsug, out_ss=txss, out_freq=txdst,
                         out_outliers=txotl)
          class(output) <- "out_all"
          print(output)
        }

        x.call <- data.frame(x.call)
        y.call <- data.frame(y.call)

      }  # end freq polygon
    }  # end !cat.x

    else {  # cat.x
      # just x variable, so set y.call to plot points for count and prop
      if (stat.x %in% c("count", "proportion", "%")) {
        if (Trellis) {  # follow dot plot format and do horizontal plot
          cat.y <- FALSE
          if (seg.x.miss) segments.x <- TRUE
          ylab <- ifelse (stat.x == "count", "Count of", "Proportion of")
          ylab <- paste(ylab, x.name)
          x.call <- data.frame(x.call)
        }  # end if Trellis

        else {  # not Trellis, so manually flip to match dot plot style
          cat.x <- FALSE
          if (seg.y.miss) segments.y <- TRUE
          if (stat.x == "count")
            xlab <- "Count of"
          else if (stat.x == "proportion")
            xlab <- "Proportion of"
          else 
            xlab <- "Percentage of"
          xlab <- paste(xlab, x.name)
          ylab <- NULL
          frq <- table(x.call)
          if (stat.x == "proportion") frq <- frq / sum(frq)
          if (stat.x == "%") frq <- (frq / sum(frq)) * 100
          if (is.factor(x.call))  # preserve ordering, will lose order attribute
            y.call <- factor(names(frq), levels=levels(x.call))
          else
            y.call <- factor(names(frq))
          cat.y <- TRUE
          num.cat.y <- TRUE
          options(yname=x.name)
          y.call <- data.frame(y.call)
          x.call <- data.frame(as.vector(frq))
        }
      }  # end values in
    }  # end cat.x

  }  # end is null y.call


  # object now determined
  # size of lines for line chart
  if (object == "both") {
    size.ln <- lwd  # size of lines
    if (lwd.miss && Trellis) size.ln <- 1.5  # smaller default for Trellis
  }
  else
    size.ln <- 1  # could be NULL?
  
  # size of fit line
  # windows line too thin at 1, but no increments allowed, and 2 is too thick
  if (fit.ln != "off") if (is.null(fit.lwd)) fit.lwd <- getOption("fit.lwd") 
  #   fit.lwd <- ifelse(.Platform$OS == "windows", 2, 1.5)

  # size of points
  if (size.miss) {  # size.pt not set yet
    scale.pt <- ifelse (.Platform$OS == "windows", 1.00, 0.80)
    size.pt <- scale.pt
  #   if (options("device") == "RStudioGD") size.pt <- size.pt*1.10

if (is.null(out.size)) out.size <- size.pt

  if (getOption("theme") == "gray")
    if (any(size.pt > 0.9)) if (out.shape.miss) out.shape <- 23
  
  if (is.null(pt.fill)) pt.fill <- "transparent"

  }
  else {  # size had been set
    if (length(size) == 1)
      scale.pt <- ifelse (.Platform$OS == "windows", 1.00, 0.80)
    else   # size var
      scale.pt <- 1  # forget Win/Mac scaling for size var, ruins size for Mac
    size.pt <- size * scale.pt
  }

  # object: "both", "point", "bubble", or "sunflower"
  if (fill.miss) {
    if (object == "both"  &&  !stack)   # line chart
      fill[1] <- "transparent"
    else
      fill[1] <- getOption("pt.fill") 
  }
  else if (fill[1] == "on")  # default color
    if (object == "both")   # line chart
      fill[1] <- getOption("violin.fill")
    else
      fill[1] <- getOption("pt.fill") 

  if (size.miss) {
    if (object == "both") {
      size.pt <- 0.77 * size.pt  # default pt size for lines
      if (fill[1] != "transparent")  # default no points if area shown
        size.pt <- 0
      else if (nrows > 50) {
        size.pt <- .9 - 0.002*nrows
        if (size.pt < 0.10) size.pt <- 0
      }
    }
  }


  # ------------------------------------------------
  # analysis
  # ------------------------------------------------
  
  if (getOption("suggest")) {
    # function call for suggestions
    fncl <- .fun.call.deparse(fun.call) 
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)
  }


  if (Trellis && do.plot) {

    if (c.type != "dot") {

      if (!is.null(by1.call)  &&  !is.null(by.call)  &&  is.null(y.call)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Simultaneous conditioned variable (by1) + grouping variable (by)\n",
          "not currently implemented for the VBS plot\n\n")
      }

      # VBS plot only lattice graphics that (currently) does not use Trellis
      txt <- ifelse (!is.null(by1.call), "[Trellis", "[Violin/Box/Scatterplot")
      if (!quiet)
        cat(paste(txt, "graphics from Deepayan Sarkar's lattice package]\n"))

      if (y.miss && !run) {  # VBS plot

        if (vbs.pt.fill == "black") {
          pt.fill <- "black"
          pt.trans <- 0
          pt.color <- "black"
        }
        else if (vbs.pt.fill == "default") {
          pt.fill <- getOption("pt.fill")
          pt.trans <- getOption("pt.fill")
          pt.color <- getOption("pt.color")
        }
        else {
          pt.fill <- vbs.pt.fill
          pt.trans <- getOption("pt.fill")
          pt.color <- getOption("pt.color")
        }

        # get VBS parameters
        VBS <- .plt.VBS(x.call[,1], ID.call, by1.call, by1.miss, by.call,
                by.miss, bw, bw.miss, bw.iter, iter.details,
                lx, n.ux, k.iqr, box.adj, a, b,
                x.name, by1.name, by.name, vbs.plot,
                n.col.miss, n.row.miss,
                size, j.x.miss, jitter.x, j.y.miss, jitter.y,
                bin, breaks, bin.start, bin.width, bin.end, proportion, 
                digits.d, quiet, fun.call, ...)
        size.pt <- VBS$size.pt
        jitter.y <- VBS$jitter.y
        jitter.x <- VBS$jitter.x
        bw <- VBS$bw
        adj.bx.ht <- VBS$adj.bx.ht
 
      }
      else
        adj.bx.ht <- nrows  # this is here just to adjust box height

      if (jitter.x > 0)  # not available in stripplot
        x.call[,1] <- jitter(x.call[,1], factor=jitter.x) 

      # n.col is null for at Plot(x), Plot(x, by=), Plot(x, by1=)
      if (!grepl("s", vbs.plot)) size.pt <- 0  # gets rescaled if earlier
      if (is.null(pt.color)) pt.color <- getOption("pt.color")
      .plt.lattice(x.call[,1], y.call[,1], by1.call, by2.call, by.call,
                   adj.bx.ht, object, n.row, n.col, aspect,
                   fill, pt.color, panel.fill, panel.color,
                   pt.trans, size.pt, size.ln, 
                   xlab, ylab, main, shape, lab.cex, axis.cex,
                   max(ellipse), ellipse.color, ellipse.lwd,
                   fit.ln, fit.color, fit.lwd,
                   area.origin, jitter.y,
                   violin, violin.fill, box, box.fill, 
                   bw, vbs.size, box.adj, a, b, k.iqr, fences, vbs.mean,
                   out.shape, out.size,
                   out.fill, out.color, out2.fill, out2.color,
                   ID.call, out.cut, ID.color, ID.size,
                   rotate.x, rotate.y, width, height, pdf.file,
                   c.type, ...)

    }  # end c.type != "dot"


    else {  # dot plot

      if (seg.x.miss) segments.x <- TRUE
      .bar.lattice(x.call[,1], by1.call, by2=NULL, n.row, n.col, aspect,
                   prop, pt.fill, pt.color,
                   pt.trans, size.pt, xlab, ylab, main,
                   rotate.x, offset,
                   width, height, pdf.file,
                   segments.x, breaks=NULL, c.type, quiet, ...)
    }

  }  # end Trellis && do.plot


  # -----------------------------------
  # bubble plot frequency matrix (BPFM)
  else if (ncol(data.x) > 1  &&  y.miss  &&  object == "bubble") {
    # get labels just for subset data matrix
    l <- attr(data, which="variable.labels")
    nm <- names(data.x)
    mylabs <- character(length=length(nm))
    for (i in 1:length(nm)) {
      if (!(nm[i] %in% names(l)))
        mylabs[i] <- "not available"
      else
        mylabs[i] <- l[which(names(l) == nm[i])]
    }
    if (all(mylabs == "not available")) mylabs <- NULL
    
    l.name <- "l"
    if (l.name %in% ls(name=.GlobalEnv))
      mylabs <- get(l.name, pos=.GlobalEnv)

    if (is.null(xlab)) xlab <- ""  # suppress x-axis label if not specified

    .dpmat.main(data[,x.col], mylabs, sort.yx,
      getOption("bar.fill.ordered"), pt.color, panel.fill,
      pt.trans, shape, panel.color,
      low.fill, hi.fill,
      xy.ticks, xlab, ylab, main, sub, size,
      radius, size.cut, bubble.text, power,
      bm.adj, lm.adj, tm.adj, rm.adj,
      value.labels, rotate.x, rotate.y, offset, quiet, do.plot, fun.call, ...)
  }

  # scatterplot matrix
  else if (spmat && do.plot) {
    bckg <- ifelse(panel.fill=="transparent",
                   getOption("window.fill"), panel.fill)
    nm <- names(data.x)
    .plt.mat(data.x, cor.coef=TRUE, fit=fit,
             col.fill=pt.fill, col.color=pt.color,
             col.fit=fit.color, col.bg=bckg, col.box=panel.color)
  }

  else {  # all the other analyses

    if (stat.yx %in% c("sum", "mean", "sd", "dev", "min", "median", "max")) {

      n.cat <- 0
      means <- FALSE
      if (seg.x.miss) segments.x <- TRUE

      # do stats console output before reducing data
      if (!quiet) {
        if (!missing(y)) {
          if (cat.y) {
            cat("\n"); stop(call.=FALSE, "\n","------\n",
            y.name, " is not numerical, so cannot compute its mean\n\n")
          }
          options(yname = x.name)  # reverse order of x and y for .ss.numeric
          options(xname = y.name)
          stats <- .ss.numeric(y.call[,1], by=x.call[,1],
                               digits.d=digits.d, brief=TRUE, y.name=x.name)
          txout <- stats$tx
          options(xname = x.name)  # reverse back
          options(yname = y.name)
        }
        else  {
          stats <- .ss.factor(x.call, digits.d=digits.d, x.name=x.name,
                              brief=TRUE)
          txout <- stats$counts
        }

        class(txout) <- "out"

        output <- list(out_txt=txout)
        class(output) <- "out_all"
        print(output)
      }

    # set up new x.call and y.call for stats
      if (stat.yx == "sum") {
        ylab <- paste("Sum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], sum, na.rm=TRUE)
      }
      if (stat.yx == "mean") {
        ylab <- paste("Mean of", y.name)
        out <- tapply(y.call[,1], x.call[,1], mean, na.rm=TRUE)
      }
      if (stat.yx == "sd") {
        ylab <- paste("Standard Deviation of", y.name)
        out <- tapply(y.call[,1], x.call[,1], sd, na.rm=TRUE)
      }
      if (stat.yx == "dev") {
        ylab <- paste("Mean Deviations of", y.name)
        out <- tapply(y.call[,1], x.call[,1], mean, na.rm=TRUE)
        out <- out - mean(out, na.rm=TRUE)
      }
      if (stat.yx == "min") {
        ylab <- paste("Minimum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], min, na.rm=TRUE)
      }
      if (stat.yx == "median") {
        ylab <- paste("Median of", y.name)
        out <- tapply(y.call[,1], x.call[,1], median, na.rm=TRUE)
      }
      if (stat.yx == "max") {
        ylab <- paste("Maximum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], max, na.rm=TRUE)
      }

      #if (is.factor(x.call))  # preserve ordering, will lose order attribute
        #x.call <- factor(names(out), levels=levels(x.call))
      #else {
        #if (is.numeric(x.call)) {
          #m1 <- min(sort(unique(x.call[,1])))
          #m2 <- max(sort(unique(x.call[,1])))
          #x.call <- factor(names(out), levels=m1:m2)  # get entire numeric range
        #}
        #else
          x.call <- factor(names(out))
      #}
      y.call <- as.vector(out)

      x.call <- data.frame(x.call)
      y.call <- data.frame(y.call)

    # switch
      temp.call <- x.call
      x.call <- y.call
      cat.x <- FALSE
      y.call <- temp.call
      cat.y <- TRUE
      xlab <- ylab
      ylab <- NULL
      options(yname = x.name)
      if (segments.x) {
        segments.x <- ifelse(segments.y, TRUE, FALSE)
        segments.y <- TRUE
       }
    }  # sum, mean, sd, min, median, max


    # 2-variable scatter plot
    # bubble plot for 1-variable (y.call=0) and 2-variable
    # line chart
    if ((!is.null(y.call) || date.ts) &&
        object %in% c("point", "bubble", "both", "sunflower")) {

      if (object == "point"  &&  data.do){  # for Cleveland dot plot
        if (!cat.x && cat.y && y.unique) {  # no sort.xy option
          if (seg.y.miss) if (y.unique && cat.y) segments.y <- TRUE
          if (seg.x.miss) if (x.unique && cat.x) segments.x <- TRUE
          if (sort.yx.miss) if (n.x_var <= 2) sort.yx <- "+"
        }
      }

     # sort y by x option (intended for Cleveland dot plot)
      tx <- character(length=0)
      if (sort.yx != "0") {
        srt.dwn <- ifelse (sort.yx == "-", TRUE, FALSE)
        if (n.x_var == 1) {  # one x-variable
          ord <- order(x.call, decreasing=srt.dwn)
        }
        else if (n.x_var == 2) {  # two x-vars, sort on diffs
          difs <- x.call[,2] - x.call[,1] 
          ord <- order(difs, decreasing=srt.dwn)
        }  # !quiet
        else {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Sorting not meaningful for more than two x-variables\n\n")
        }

        y.c <- y.call[,1]
        y.c <- factor(y.c, levels=y.c[ord])
        y.call[,1] <- y.c

      }  # end sort.yx
      else {
        if (n.x_var == 2  &&  ncol(x.call) == 2) {  # run==TRUE only 1-col
          ord <- 1:nrow(x.call)
          difs <- x.call[,2] - x.call[,1] 
        }
      }

      # for Cleveland dot plot of two vars, print difference by level
      if (n.x_var == 2  &&  ncol(x.call) == 2  &&  !quiet) {
        dd <- .max.dd(c(x.call[1], x.call[2])) + 1
        if (dd > getOption("digits")) dd <- getOption("digits")
        ny <- nrow(y.call)
        mx.i <- nchar(as.character(ny))
        mx.d <- max(nchar(.fmt(difs, dd)))
        mx.f <- ifelse (is.factor(y.call[,1]), 
           max(nchar(as.character(levels(y.call[,1])))), 5)  #  5 is dummy
        tx[length(tx)+1] <- paste(.fmtc("n",mx.i), " ", 
            .fmtc(" diff", mx.d), "  Row", sep="")
        tx[length(tx)+1] <- .dash2(mx.i + mx.d + mx.f + 2, "-") 
        if (ny <= 20)
          rng <- 1:ny
        else
          rng <- c(1:10, (ny-10):ny)
        for (i in 1:ny) {
          k <- nrow(y.call) - (i - 1)  # reverse order, + diffs first
          if (i %in% rng)
            tx[length(tx)+1] <- paste(.fmti(i, mx.i),
              .fmt(difs[ord[k]], dd, mx.d), levels(y.call[,1])[k])
        }
        txdif <- tx  # a little hack, only display in .plt.txt
      }

      # bigger point for scatterplot of stats (instead of data)
      if (!data.do  &&  object == "point")
        if (is.null(size)) size.pt <- 1.25

      out.ind <- NULL
      txout <- ""
      if (!y.miss && !Trellis) if(n.x_var == 1  &&  n.y_var == 1)
        if (MD.cut > 0  ||  out.cut > 0) {
          otl <- .plt.MD(x.call[,1], y.call[,1], ID.call, MD.cut, out.cut)  
          txout <- otl$tx
          out.ind <- otl$out.ind
      }

      if (do.plot) {

        if (nrow(x.call) != nrow(y.call))  {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "number of elements in x: ", nrow(x.call), "\n",
            "number of elements in y: ", nrow(y.call), "\n\n",
            "The number of elements must be equal, probably\n",
            "  have variables from user workspace so maybe\n",
            "  use the  remove function, e.g., remove(x)\n\n")
        }

  if (run) if (lwd == 0) fill <- getOption("violin.fill")
  if (object == "both"  &&  nn.col > 1) {
     stack <- TRUE  # meaningless otherwise
     if (fill == "on") fill <- getOption("violin.fill")  # change to multi later
  }

 
        .plt.main(x.call, y.call, by.call, n.cat,
          cat.x, num.cat.x, cat.y, num.cat.y,
          object, stat,
          fill, area.fill, pt.color,
          pt.trans, segment.color, 
          xy.ticks, xlab, ylab, main, main.cex,
          sub, value.labels, label.max,
          rotate.x, rotate.y, offset, proportion, origin.x,
          size.pt, shape, means, segments.y, segments.x, size.ln,
          smooth, smooth.points, smooth.size, smooth.exp, smooth.bins,
          radius, power, size.cut, bubble.text, low.fill, hi.fill,
          ID.call, ID.color, ID.size, out.ind,
          out.fill, out.color, out.shape.miss,
          fit.ln, fit.color, fit.lwd, fit.se, se.fill,
          ellipse, ellipse.color, ellipse.fill, ellipse.lwd,
          center.line, show.runs, stack,
          freq.poly, jitter.x, jitter.y,
          xlab.adj, ylab.adj, bm.adj, lm.adj, tm.adj, rm.adj,
          legend.title,
          add, x1, x2, y1, y2, add.cex, add.lwd, add.lty,
          add.color, add.fill, add.trans,
          quiet, ...)
      }

      if (!quiet) {  # text output

        .plt.txt(x.call, y.call, stat, object, n.cat,
          cat.x, num.cat.x, cat.y, num.cat.y,
          xlab, ylab,
          smooth, box.adj, center.line, proportion, size,
          show.runs, radius, digits.d, fun.call, txdif)

       if (!y.miss && !Trellis) if (n.x_var == 1  &&  n.y_var == 1) {
          class(txout) <- "out"  # MD outlier analysis
          output <- list(out_outlier=txout, outlier_indices=out.ind)
          class(output) <- "out_all"
        }
      }

    }  # end 2-variable scatter plot

  }  # end all other analyses


  # terminate pdf graphics system if used
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "plot")
  }

  # reset
  options(xname=NULL)
  options(yname=NULL)
  options(by1name=NULL)
  options(by2name=NULL)
  options(byname=NULL)

  if (!y.miss && !Trellis) if(n.x_var == 1  &&  n.y_var == 1)
    if (!quiet) return(output)
}
