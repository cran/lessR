Plot <-
function(x, y=NULL, data=mydata,
         values=c("data", "count", "prop", "sum", "mean", "sd",
                  "min", "median", "max"),
         n.cat=getOption("n.cat"),

         by=NULL, by1=NULL, by2=NULL,
         n.row=NULL, n.col=NULL, aspect="fill",

         size=NULL, size.cut=NULL, shape="circle", means=TRUE,
         sort.yx=FALSE, segments.y=FALSE, segments.x=FALSE,
         jitter.x=0, jitter.y=0,

         ID="row.name", ID.size=0.75,
         MD.cut=0, out.cut=0, out.shape="circle", out.size=1,

         vbs.plot="vbs", vbs.pt.fill=c("black", "default"), bw=NULL,
         vbs.size=0.9, vbs.mean=FALSE, fences=FALSE,
         k=1.5, box.adj=FALSE, a=-4, b=3,

         radius=0.25, power=0.6,
         low.fill=NULL, hi.fill=NULL, proportion=FALSE,

         smooth=FALSE, smooth.points=100, smooth.trans=0.25,
         smooth.bins=128,

         fit=FALSE, fit.se=0, ellipse=FALSE, 

         bin=FALSE, bin.start=NULL, bin.width=NULL, bin.end=NULL,
         breaks="Sturges", cumul=FALSE, 

         run=FALSE, lwd=2, area=FALSE, area.origin=0, 
         center.line=c("default", "mean", "median", "zero", "off"),
         show.runs=FALSE, stack=FALSE,

         add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,

         xy.ticks=TRUE, value.labels=NULL, label.max=20,
         rotate.x=getOption("rotate.x"),
         rotate.y=getOption("rotate.y"),
         offset=getOption("offset"),
         origin.x=NULL,

         auto=FALSE, digits.d=NULL, quiet=getOption("quiet"),
         do.plot=TRUE, width=NULL, height=NULL, pdf.file=NULL, 
         fun.call=NULL, ...) {


  # ------
  # set parameter values 

  vbs.pt.fill <- match.arg(vbs.pt.fill)
  
  if (is.null(fun.call)) fun.call <- match.call()
   
  # limit actual argument to alternatives, perhaps abbreviated
  values <- match.arg(values)
  center.line <- match.arg(center.line)

  vbs.plot <- tolower(vbs.plot)
  violin <- ifelse (grepl("v", vbs.plot), TRUE, FALSE)
  box <- ifelse (grepl("b", vbs.plot), TRUE, FALSE)

  df.name <- deparse(substitute(data))
  options(dname = df.name)

  # if a tibble convert to data frame
  # data.frame is already #3 in class(data), so no char --> factor conversion 
  if (exists(df.name, where=.GlobalEnv)) if (class(data)[1] == "tbl_df") {
    data <- as.data.frame(data, stringsAsFactors=TRUE)
  }

  k.iqr <- k   # k is a function name, so do not use internally

  cat.x <- NULL;  num.cat.x <- NULL;  cat.y <- NULL;  num.cat.y <- NULL; 

  pt.fill <- getOption("pt.fill")
  pt.color <- getOption("pt.color")
  pt.trans <- getOption("trans.pt.fill")
  bar.color <- getOption("bar.color")
  bubble.text <- getOption("bubble.text.color")
  segment.color <- getOption("segment.color")
  ID.color <- getOption("ID.color")

  ellipse.fill <- getOption("ellipse.fill")
  ellipse.color <- getOption("ellipse.color")
  ellipse.lwd <- getOption("ellipse.lwd")

  fit.color <- getOption("fit.color")
  fit.lwd <- getOption("fit.lwd")

  ID.color <- getOption("ID.color")
  out2.fill <- getOption("out2.fill")
  out.fill <- getOption("out.fill")

  panel.fill <- getOption("panel.fill")
  panel.color <- getOption("panel.color") 
  grid.color <- getOption("grid.color")

  lab.cex <- getOption("lab.cex")
  axis.cex <- getOption("axis.cex")

  add.fill <- getOption("add.fill")
  add.trans <- getOption("add.trans")
  add.color <- getOption("add.color")
  add.cex <- getOption("add.cex")
  add.lwd <- getOption("add.lwd")
  add.lty <- getOption("add.lty")

  # missing function only reliable if arg not modified, so capture 
  x.miss <- ifelse (missing(x), TRUE, FALSE)
  y.miss <- ifelse (missing(y), TRUE, FALSE)
  by1.miss <- ifelse (missing(by1), TRUE, FALSE)
  by2.miss <- ifelse (missing(by2), TRUE, FALSE)
  by.miss <- ifelse (missing(by), TRUE, FALSE)
  values.miss <- ifelse (missing(values), TRUE, FALSE)
  size.miss <- ifelse (missing(size), TRUE, FALSE)
  seg.y.miss <- ifelse (missing(segments.y), TRUE, FALSE)  # for Cleveland plot
  seg.x.miss <- ifelse (missing(segments.x), TRUE, FALSE)
  sort.yx.miss <- ifelse (missing(sort.yx), TRUE, FALSE)
  area.miss <- ifelse (missing(area), TRUE, FALSE)
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
  data.miss <- ifelse (missing(data), TRUE, FALSE)
  add.miss <- ifelse (missing(add), TRUE, FALSE)

  if (!missing(a) || !missing(b)) box.adj <- TRUE

  if (missing(vbs.size)) if (!violin)  # wider box if no violin
    vbs.size <- ifelse (y.miss || by1.miss, vbs.size*3.75, vbs.size*5)
 
  # "off" substitutes for official value of "transparent"
  ellipse.color[which(ellipse.color == "off")] <- "transparent"
  ellipse.fill[which(ellipse.fill == "off")] <- "transparent"
  add.fill[which(add.fill == "off")] <- "transparent"
  add.color[which(add.color == "off")] <- "transparent"

  if (auto) {
    if (ellipse.miss) ellipse <- 0.95
    if (MD.miss) MD.cut <- 6
    if (add.miss) add <- "means"
    if (fit.miss) fit <- "ls"
  }

  date.ts <- FALSE  # default is not a time series
  freq.poly <- FALSE  # default is not a frequency polygon

  #grid.x.color <- getOption("grid.x.color") # for Cleveland

  if (show.runs) run <- TRUE

  # fit.ln
  if (fit.se[1] > 0) if (fit.miss) fit <- TRUE

  if (is.logical(fit))
    fit.ln <- ifelse (fit, "loess", "off")
  if (is.character(fit)) {
    if (!(fit %in% c("loess", "ls", "off"))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "fit applies only for  loess  or  ls  (least squares)\n\n")
    }
    fit.ln <- fit  # fit.ln passed to .plt.main
  }

  # any bin parameter activates bins for VBS plot, or freq.poly=TRUE
  if (!missing(breaks)  ||  !missing(bin.start)  ||  !missing(bin.width)  ||
      !missing(bin.end))
    bin <- TRUE

  # area
  if (stack) if (area.miss) area <- TRUE  # stack default
  if (area) if (missing(stack)) stack <- TRUE
  area.fill <- ifelse (area, getOption("area.fill"), "transparent")

  if (!is.null(pdf.file))
    if (!grepl(".pdf", pdf.file)) pdf.file <- paste(pdf.file, ".pdf", sep="")

  if (values != "data") if (is.null(pt.trans)) pt.trans <- 0

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
  .plt.bad(x.miss, y.miss, values, breaks, bin.start, n.row, n.col,
           MD.cut, out.cut, ...)


  # ----------
  # evaluate x
  
  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x), width.cutoff = 120L)
  options(xname = x.name)

  # get data to be analyzed into data.x data frame

  # process row.names if specified
  if (x.name == "row.names") {
    # retain order of row names, otherwise will be alphabetical
    data.x <- data.frame(factor(row.names(data), levels=row.names(data)))
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label
    cat.x <- TRUE
  }

  else if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists
      .xcheck(x.name, df.name, data)  # var in df?, vars lists not checked
      all.vars <- as.list(seq_along(data))  # even if only a single var
      names(all.vars) <- names(data)  # all data in data frame
      x.col <- eval(substitute(x), envir=all.vars)  # col num of selected vars
      if (!("list" %in% class(data))) {
        data.x <- data[, x.col]
        data.x <- data.frame(data.x)
     }      
     else {  # class of data is "list"
        data.x <- data.frame(data[[x.col]])
      }
      if (is.numeric(x.col))
        names(data.x) <- names(all.vars)[x.col]
      else
        names(data.x) <- x.col  # if x a vector, x.col can return names
      data.miss <- FALSE  # use mydata even if not specified (default)
    }  # end x not in global

    # x is in the global environment (vector or data frame)
    # can only access x directly if it is not in a data frame
    else if (is.data.frame(x)) { # x a data frame
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Need to specify variables for analysis,\n",
          "not a full data frame\n\n")
    }
      
    else if (is.ts(x)) {  # time series in global
      # just the dates for x var
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
      if (!is.function(x)) {
        data.x <- data.frame(x)  # x is 1 var
        cat(">>> ", x.name, "in the global environment, not a data frame\n\n")
      }
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

  #if (!y.miss) if (deparse(substitute(y)) %in% c("count", "prop")) {
    #if (deparse(substitute(y)) == "count") values <- "count"
    #if (deparse(substitute(y)) == "prop") values <- "prop"
    #y.miss <- TRUE
  #}

  if (!y.miss) {
    # get actual variable name before potential call of data$y
    y.name <- deparse(substitute(y))
    options(yname = y.name)

    if (deparse(substitute(y)) == "row.names") {
      # retain order of row names, otherwise will be alphabetical
      y.call <- factor(row.names(data), levels=row.names(data))
      if (is.null(ylab)) ylab <- ""  # unless specified, drop the axis label
      cat.y <- TRUE
      data.y <- data.frame(y.call)
    }
      
    else if (!exists(y.name, where=.GlobalEnv)) {  # y not in global env, in df
        .nodf(df.name)  # check to see if data frame container exists
        .xcheck(y.name, df.name, data)  # var in df?, vars lists not checked
        all.vars <- as.list(seq_along(data))  # even if only a single var
        names(all.vars) <- names(data)  # all data in data frame
        y.col <- eval(substitute(y), envir=all.vars)  # col num selected vars
       if (!("list" %in% class(data))) {
          data.y <- data[, y.col]
          data.y <- data.frame(data.y)
       }      
       else {  # class of data is "list"
          data.y <- data.frame(data[[y.col]])
        }
      if (is.numeric(y.col))
        names(data.y) <- names(all.vars)[y.col]
      else
        names(data.y) <- y.col
      }  # end global y

      else if (is.data.frame(y)){ # y is in the global env (vector or data frame)
          # y a data frame
          data.y <- y
       }
       
        else {  # y a vector in global
          if (!is.function(y))
            data.y <- data.frame(y)  # y is 1 var
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

  # ellipse stop conditions
  if (ellipse[1] > 0) {
    many.y <- FALSE
    if (!y.miss) if (is.matrix(y.call)) many.y <- TRUE
    if ((ncol(data.x)>1 || many.y)  ||  cat.x  ||  cat.y) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "An ellipse only applies to analysis of a single x-variable \n",
        "  with a single y-variable, both continuous\n\n")
    }
    if (y.miss) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need a y-variable to compute an ellipse\n\n")
    }
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
  # master control funnel for type of analysis

  lx <- length(x.call[,1])
  ly <- length(y.call[,1])
  n.ux <- length(unique(x.call[,1]))
  n.uy <- length(unique(y.call[,1]))
  x.unique <- ifelse (n.ux == lx, TRUE, FALSE)
  y.unique <- ifelse (n.uy == ly, TRUE, FALSE)

  # replications from two variable cross-tabs? then go bubble plot 
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

#if (!y.miss) cat("cat.y:", cat.y, "\n")
#if (!y.miss) cat("num.cat.y:", num.cat.y, "\n")
  if (values == "data"  &&  n.x_var == 1) {
    if (y.miss) {  # y missing
      if (!cat.x) {  # continuous x
        if (!run && !date.ts) {  # not a run chart or ts
        Trellis <- TRUE
        c.type <- "cont"
       }
       else {  # run/ts chart
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
          if (y.unique)  # Cleveland
            if (by1.miss)
              Trellis <- FALSE
            else {
              cat("\n"); stop(call.=FALSE, "\n","------\n",
                "Currently, Trellis plots not available for dot plots \n",
                "with unique values for the categorical variable\n\n")
            }
          else {
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
  }  # end values is data

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
      .xcheck(by.name, df.name, data)

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
      .xcheck(by1.name, df.name, data)

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
      .xcheck(by2.name, df.name, data)

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

    # get conditions and check for data existing
    xs <- .xstatus(size.name, df.name, quiet)
    in.global <- xs$ig

    # if size.name is not a number, make.num gets NA with a warning
    make.num <- suppressWarnings(as.numeric(size.name))
    is.num <- ifelse (!is.na(make.num), TRUE, FALSE)

    # see if var exists in data frame, if x not in global Env or function call
    if (!is.num) {  # size.name is the name of a variable
      if (!in.global) {
        .xcheck(size.name, df.name, data)
        size <- eval(substitute(data$size))
      }
      if (!is.numeric(size)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Variable ", size.name, " must be numeric\n\n",
          "Perhaps use: by=", size.name, "\n\n")
      }
      options(sizename = size.name) # for later access
      object <- "bubble"
      if (is.null(size.cut)) size.cut <- ifelse (cat.x, 1, 2)
    }
    else  # size is a numerical constant
      size.cut <- FALSE
  }
  else
    if (missing(size.cut)) size.cut <- TRUE

  if (!grepl("s", vbs.plot)) size <-  0


  # evaluate ID 
  #------------
  get.ID <- FALSE
  if (y.miss && !cat.x) get.ID <- TRUE  # VBS plot
  if (!y.miss) if (!cat.x && !cat.y && (MD.cut>0 || out.cut>0))
    get.ID <- TRUE 
  if (get.ID) {

    if (ID == "row.name") {
      if (!data.miss)
        ID.call <- factor(row.names(data), levels=row.names(data))
      else
        ID.call <- 1:nrows
    }

    else {  # allow a specified variable in data table to be the ID
      ID.name <- deparse(substitute(ID))
      .xstatus(ID.name, df.name, quiet)  # check for data existing
      .xcheck(ID.name, df.name, data)  # var exists in data frame?
      ID.call <- eval(substitute(data$ID))
    }
  }
  else
    ID.call <- NULL



  # -----------  x, y, by and size variables established ------------
  # -----------------------------------------------------------------

  if (is.null(height)) { 
    if (is.null(y.call)  &&  !BPFM  
        &&  values == "data" &&  object != "both"  &&  !.is.date(x.call))
      height <- ifelse (is.null(main), 2.5, 3.1)  # narrow for 1-D dot plot
    else
      height <- 4.5

    if (BPFM)  # more than 7 variables, make plot extra long
      height <- height + ((ncol(data.x) - 7) * 0.5)
  }

  if (is.null(width)) width <- 5
  if (!by.miss) width <- width + .85  # wider plot  


  # --------
    # adjust by, manage regular-R or PDF graphics window size
  if (!Trellis)
    .opendev(pdf.file, width, height)  # prepare plot window, dev or pdf


  # ------------------------------------------------
  # set object and values where needed

  # prep 1-variable bubble plot to call regular scatter plot function
  # y.call to 0
  if (is.null(y.call)  &&  cat.x  &&  n.x_var == 1  &&  values == "data") {
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
      if (values == "data") if (cat.x && cat.y) object <- "bubble"
    }
    else if (values %in% c("count", "prop")) {
      object <- "point"
    }
    else {
      if (values == "data") object <- ifelse (cat.x, "bubble", "point")
      if (BPFM) object <- "bubble"  # BPFM
    }
  }


  if (y.miss  &&  !date.ts  &&  object != "bubble") {  

    if (!cat.x) { 

      # run chart prep of x.call and y.call
      if (object == "both"  &&  values == "data") {  # run chart
        y.call <- x.call
        cat.y <- cat.x
        options(yname = x.name)
        
        options(xname = "Index")
        x.call <- data.frame(1:nrow(x.call))
        names(x.call) <- "Index"
      }

      else if (values %in% c("count", "prop")) {  # frequency polygon

        ssstuff <- .ss.numeric(x.call[,1], digits.d=digits.d, brief=TRUE)
       
        hist.counts <- FALSE
        hist.cumul <- ifelse(cumul, "on", "off")
        reg <- "snow2"  # applies to cumulative histogram
        h <- .hst.main(x.call[,1], pt.fill, pt.color, reg,
           lab.cex, axis.cex, rotate.x, rotate.y, offset,
           breaks, bin.start, bin.width, bin.end, proportion, hist.counts,
           hist.cumul, xlab, ylab, main, sub, quiet, fun.call=NULL,
           do.plot=FALSE, ...) 


        n.cat <- 0  # not many midpoints, do not want to trigger num.cat
        x.call <- h$mids
        y.call <- h$counts
        if (values == "count")
          ylab <- paste("Count of", x.name)
        else {
          y.call <- y.call / sum(y.call)
          ylab <- paste("Proportion of", x.name)
        }

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
          if (!grepl("area", fncl))
            fc <- paste(fc, ", area=TRUE", sep="")
          if (nzchar(fc)) {
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          }
            
          fc <- ""
          if (!grepl("size", fncl)  &&  !grepl("area", fncl))
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
            txsug <- paste(txsug, "\n", fc, sep="")
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

          class(txsug) <- "out_piece"
          class(txss) <- "out_piece"
          class(txdst) <- "out_piece"
          class(txotl) <- "out_piece"
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
      if (values %in% c("count", "prop")) {
        if (Trellis) {  # follow dot plot format and do horizontal plot
          cat.y <- FALSE
          if (seg.x.miss) segments.x <- TRUE
          ylab <- ifelse (values=="count", "Count of", "Proportion of")
          ylab <- paste(ylab, x.name)
          x.call <- data.frame(x.call)
        }  # end if Trellis

        else {  # not Trellis, so manually flip to match dot plot style
          cat.x <- FALSE
          if (seg.y.miss) segments.y <- TRUE
          xlab <- ifelse (values=="count", "Count of", "Proportion of")
          xlab <- paste(xlab, x.name)
          if (!Trellis) {  # Trellis needs the full data for a dot plot
            ylab <- NULL
            frq <- table(x.call)
            if (values == "prop") frq <- frq / sum(frq)
            if (is.factor(x.call))  # preserve ordering, will lose order attribute
              y.call <- factor(names(frq), levels=levels(x.call))
            else
              y.call <- factor(names(frq))
            cat.y <- TRUE
            num.cat.y <- TRUE
          }
          options(yname=x.name)
          y.call <- data.frame(y.call)
          x.call <- data.frame(as.vector(frq))
        }
      }  # end values in
    }  # end cat.x

  }  # end is null y.call


  # size of lines for line chart
  if (object == "both") {
    size.ln <- lwd  # size of lines
    if (lwd.miss && Trellis) size.ln <- 1.5  # smaller default for Trellis
  }
  else
    size.ln <- 1  # could be NULL?
  
  # size of fit line
  # windows line too thin at 1, but no increments allowed, and 2 is too thick
  if (fit.ln != "off") if (is.null(fit.lwd)) 
    fit.lwd <- ifelse(.Platform$OS == "windows", 2, 1.5)

  # size of points
  scale.pt <- ifelse (.Platform$OS == "windows", 1.00, 0.80)
  if (size.miss) {  # size.pt not set yet
    size.pt <- scale.pt
    if (options("device") == "RStudioGD")
      size.pt <- size.pt*1.20

    if (object == "both") {
      size.pt <- 0.77 * size.pt  # default pt size for lines
      if (area.fill != "transparent")  # default no points if area shown
        size.pt <- 0
      else if (nrows > 50) {
        size.pt <- .9 - 0.002*nrows
        if (size.pt < 0.10) size.pt <- 0
      }
    }
  }
  else  # size had been set
    size.pt <- size * scale.pt
    if (is.null(out.size)) out.size <- size.pt

  if (getOption("theme") == "gray")
    if (size.pt > 0.9) if (out.shape.miss) out.shape <- 23
  
  if (is.null(pt.fill)) pt.fill <- "transparent"


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

        VBS <- .plt.VBS(x.call[,1], ID.call, by1.call, by1.miss, by.call,
                bw, bw.miss, lx, n.ux, k.iqr, box.adj, a, b,
                x.name, by1.name, by.name,
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

      .plt.lattice(x.call[,1], y.call[,1], by1.call, by2.call, by.call,
                   adj.bx.ht, object, n.row, n.col, aspect,
                   pt.fill, pt.color, panel.fill, panel.color,
                   pt.trans, size.pt, size.ln, 
                   xlab, ylab, main, shape, lab.cex, axis.cex,
                   max(ellipse), ellipse.color, ellipse.lwd,
                   fit.ln, fit.color, fit.lwd,
                   area.fill, area.origin, jitter.y, bw, violin,
                   box, vbs.size, box.adj, a, b, k.iqr, 
                   fences, vbs.mean, out.shape, out.size, out2.fill, out.fill,
                   ID.call, out.cut, ID.color, ID.size,
                   rotate.x, rotate.y, width, height, pdf.file,
                   c.type, ...)

    }  # end c.type != "dot"


    else {  # dot plot
      if (seg.x.miss) segments.x <- TRUE
      .bar.lattice(x.call[,1], by1.call, by2=NULL, n.row, n.col, aspect,
                   prop=FALSE, pt.fill, pt.color, panel.fill, panel.color,
                   pt.trans, size.pt, xlab, ylab, main, lab.cex, axis.cex,
                   rotate.x, rotate.y, width, height, pdf.file,
                   segments.x, breaks=NULL, c.type, ...)
    }

  }  # end Trellis && do.plot



  # -----------------------------------
  # bubble plot frequency matrix (BPFM)
  else if (ncol(data.x) > 1  &&  y.miss  &&  object == "bubble") {
    # get labels just for subset data matrix
    mylabels <- attr(data, which="variable.labels")
    nm <- names(data.x)
    mylabs <- character(length=length(nm))
    for (i in 1:length(nm)) {
      if (!(nm[i] %in% names(mylabels)))
        mylabs[i] <- "not available"
      else
        mylabs[i] <- mylabels[which(names(mylabels) == nm[i])]
    }
    if (all(mylabs == "not available")) mylabs <- NULL

    if (is.null(xlab)) xlab <- ""  # suppress x-axis label if not specified

    .dpmat.main(data[,x.col], mylabs, sort.yx,
      getOption("bar.fill"), pt.color, panel.fill,
      pt.trans, shape, area.fill, panel.color,
      low.fill, hi.fill,
      xy.ticks, xlab, ylab, main, sub, size,
      radius, size.cut, bubble.text, power,
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
    

    if (values %in% c("sum", "mean", "sd", "min", "median", "max")) {

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

        class(txout) <- "out_piece"

        output <- list(out_txt=txout)
        class(output) <- "out_all"
        print(output)
      }

    # set up new x.call and y.call for stats
      if (values == "sum") {
        ylab <- paste("Sum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], sum, na.rm=TRUE)
      }
      if (values == "mean") {
        ylab <- paste("Mean of", y.name)
        out <- tapply(y.call[,1], x.call[,1], mean, na.rm=TRUE)
      }
      if (values == "sd") {
        ylab <- paste("Standard Deviation of", y.name)
        out <- tapply(y.call[,1], x.call[,1], sd, na.rm=TRUE)
      }
      if (values == "min") {
        ylab <- paste("Minimum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], min, na.rm=TRUE)
      }
      if (values == "median") {
        ylab <- paste("Median of", y.name)
        out <- tapply(y.call[,1], x.call[,1], median, na.rm=TRUE)
      }
      if (values == "max") {
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

      if (object == "point"  &&  values == "data"){  # for Cleveland dot plot
        if (!cat.x && cat.y && y.unique) {  # no sort.xy option
          if (seg.y.miss) if (y.unique && cat.y) segments.y <- TRUE
          if (seg.x.miss) if (x.unique && cat.x) segments.x <- TRUE
          if (sort.yx.miss) if (n.x_var <= 2) sort.yx <- TRUE
        }
      }

     # sort y by x option (intended for Cleveland dot plot)
      if (sort.yx) {
        if (n.x_var == 1)  # one x-variable
          ord <- order(x.call)
        else
          if (n.x_var == 2)  # two x-vars, sort on diffs
            ord <- order(x.call[,2] - x.call[,1])
          else {
            cat("\n"); stop(call.=FALSE, "\n","------\n",
            "Sorting not meaningful for more than two x-variables\n\n")
          }
        y.c <- y.call[,1]
        y.c <- factor(y.c, levels=y.c[ord])
        y.call[,1] <- y.c
      }
  
      # bigger point for scatterplot of stats (instead of data)
      if (values != "data"  &&  object == "point")
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
        .plt.main(x.call, y.call, by.call, n.cat,
          cat.x, num.cat.x, cat.y, num.cat.y,
          object, values,
          pt.fill, pt.color,
          pt.trans, segment.color, area.fill,
          xy.ticks, xlab, ylab, main, sub, value.labels, label.max,
          rotate.x, rotate.y, offset, proportion, origin.x,
          size.pt, shape, means, sort.yx, segments.y, segments.x, size.ln,
          smooth, smooth.points, smooth.trans, smooth.bins,
          radius, power, size.cut, bubble.text, low.fill, hi.fill,
          ID.call, ID.color, ID.size, out.ind,
          out.fill, out.shape.miss,
          fit.ln, fit.color, fit.lwd, fit.se,
          ellipse, ellipse.color, ellipse.fill, ellipse.lwd,
          center.line, show.runs, stack,
          freq.poly, jitter.x, jitter.y,
          add, x1, x2, y1, y2, add.cex, add.lwd, add.lty,
          add.color, add.fill, add.trans,
          quiet, ...)
      }

      if (!quiet) {  # text output
        .plt.txt(x.call, y.call, values, object, n.cat,
          cat.x, num.cat.x, cat.y, num.cat.y,
          xlab, ylab,
          smooth, box.adj, center.line, proportion, size,
          show.runs, radius, digits.d, fun.call)

       if (!y.miss && !Trellis) if(n.x_var == 1  &&  n.y_var == 1) {
          class(txout) <- "out_piece"  # MD outlier analysis
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
