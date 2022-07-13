Plot <-
function(x, y=NULL, data=d, rows=NULL, enhance=FALSE, 
         stat="data", n_cat=getOption("n_cat"), n_bins=1,

         by=NULL, by1=NULL, by2=NULL,
         n_row=NULL, n_col=NULL, aspect="fill",

         theme=getOption("theme"),
         fill=NULL,
         color=NULL,
         trans=getOption("trans_pt_fill"),

         size=NULL, size_cut=NULL, shape="circle", means=TRUE,
         sort_yx=c("0", "-", "+"), 
         segments=FALSE, segments_y=FALSE, segments_x=FALSE,
         jitter_x=0, jitter_y=0,

         ID="row.name", ID_size=0.60,
         MD_cut=0, out_cut=0, out_shape="circle", out_size=1,

         vbs_plot="vbs", vbs_size=0.9, bw=NULL, bw_iter=10,
         violin_fill=getOption("violin_fill"), 
         box_fill=getOption("box_fill"), 
         vbs_pt_fill="black",
         vbs_mean=FALSE, fences=FALSE,
         k=1.5, box_adj=FALSE, a=-4, b=3,

         radius=NULL, power=0.5, low_fill=NULL, hi_fill=NULL,

         smooth=FALSE, smooth_points=100, smooth_size=1,
         smooth_exp=0.25, smooth_bins=128,

         fit="off", fit_power=1, fit_se=0.95,
         fit_color=getOption("fit_color"),
         plot_errors=FALSE, ellipse=0, 

         bin=FALSE, bin_start=NULL, bin_width=NULL, bin_end=NULL,
         breaks="Sturges", cumulate=FALSE, 

         run=FALSE, lwd=1.5, area_fill="transparent", area_origin=0, 
         center_line=c("off", "mean", "median", "zero"),
         show_runs=FALSE, stack=FALSE,

         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         lab_adj=c(0,0), margin_adj=c(0,0,0,0),

         rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
         offset=getOption("offset"),

         xy_ticks=TRUE, value_labels=NULL, origin_x=NULL,
         scale_x=NULL, scale_y=NULL, pad_x=c(0,0), pad_y=c(0,0),
         legend_title=NULL,

         add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

         eval_df=NULL, digits_d=NULL, quiet=getOption("quiet"),
         do_plot=TRUE, width=NULL, height=NULL, pdf_file=NULL, 
         fun_call=NULL, ...) {


  if (is.null(fun_call)) fun_call <- match.call()

  if (fit == "sqrt") {
    fit <- "quad"  # new value
    message("parameter value  sqrt  replaced with  quad  for consistency\n")
  }
  if (fit == "reciprocal") {
    fit <- "exp"  # new value
    message("parameter value  reciprocal  replaced with  exp\n")
  }
  if (fit == "root") {
    fit <- "power"  # new value
    message("parameter value  root  replaced with  power  for consistency\n")
  }

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "stat_x") stat <- dots[[i]]
      if (names(dots)[i] == "stat_yx") stat <- dots[[i]]
      if (grepl(".", names(dots)[i], fixed=TRUE)) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (!is.null(x1)) if ("mean.x" %in% x1) x1 <- "mean_x"
  if (!is.null(y1)) if ("mean.y" %in% y1) y1 <- "mean_y"
  if (!is.null(add)) if ("h.line" %in% add[1])  add[1] <- "h_line"
  if (!is.null(add)) if ("v.line" %in% add[1])  add[1] <- "v_line"


  # Note: stat is both object (dot plot) and statistic   
   
  # limit actual argument to alternatives, perhaps abbreviated
  sort_yx.miss <- ifelse (missing(sort_yx), TRUE, FALSE)
  sort_yx <- match.arg(sort_yx)

  cl.miss <- ifelse (missing(center_line), TRUE, FALSE)
  center_line <- match.arg(center_line)

  data.do <- ifelse ((stat == "data"), TRUE, FALSE)

  proportion <- FALSE  # old signal, adjusted below if needed
  if (stat == "proportion") proportion <- TRUE

  if (plot_errors)
    if (fit == "off") fit <- "lm"

  if (length(pad_x) == 1) {  # only the first element of pad_x specified
    temp <- pad_x
    pad_x <- double(length=2)
    pad_x[1] <- temp  
    pad_x[2] <- temp
  }
  if (length(pad_y) == 1) {
    temp <- pad_y
    pad_y <- double(length=2)
    pad_y[1] <- temp
    pad_y[2] <- temp
  }

  shiny <- ifelse (isNamespaceLoaded("shiny"), TRUE, FALSE) 
  if (is.null(eval_df))  # default value
    eval_df <- ifelse (shiny, FALSE, TRUE) 

  vbs_plot <- tolower(vbs_plot)
  violin <- ifelse (grepl("v", vbs_plot), TRUE, FALSE)
  box <- ifelse (grepl("b", vbs_plot), TRUE, FALSE)

  iter.details <- ifelse (missing(bw_iter), FALSE, TRUE)

  k.iqr <- k   # k is a function name, so do not use internally

  cat.x <- NULL;  num.cat.x <- NULL;  cat.y <- NULL;  num.cat.y <- NULL; 

  pt.fill <- fill
  pt.color <- color
  pt.trans <- trans
  bar_color <- getOption("bar_color")
  bubble_text <- getOption("bubble_text_color")
  segment_color <- getOption("segment_color")
  ID_color <- getOption("ID_color")

  ellipse_fill <- getOption("ellipse_fill")
  ellipse_color <- getOption("ellipse_color")
  ellipse_lwd <- getOption("ellipse_lwd")

  if (fit == "ls") fit <- "lm"  # new value
  fit_lwd <- getOption("fit_lwd")
  se_fill <- getOption("se_fill")

  out_fill <- getOption("out_fill")
  out_color <- getOption("out_color")
  out2_fill <- getOption("out2_fill")
  out2_color <- getOption("out2_color")

  panel_fill <- getOption("panel_fill")
  panel_color <- getOption("panel_color") 
  grid_color <- getOption("grid_color")

  lab_cex <- getOption("lab_cex")
  axis_cex <- getOption("axis_cex")
  main_cex <- getOption("main_cex")

  add_cex <- getOption("add_cex")
  add_lwd <- getOption("add_lwd")
  add_lty <- getOption("add_lty")
  add_color <- getOption("add_color")
  add_fill <- getOption("add_fill")
  add_trans <- getOption("add_trans")

  if (theme != getOption("theme")) {  # not the default theme
    sty <- style(theme, reset=FALSE)
    trans <- sty$pt$trans_fill
    fill <- sty$pt$fill
    pt.color <- sty$pt$fill  # just solid color points
    ellipse_fill <- sty$ellipse$fill
    violin_fill <- sty$VBS$violin_fill
    box_fill <- sty$VBS$box_fill
    se_fill <- sty$se_fill
  }

  # missing function only reliable if arg not modified, so capture 
  x.miss <- ifelse (missing(x), TRUE, FALSE)
  y.miss <- ifelse (missing(y), TRUE, FALSE)
  by.miss <- ifelse (missing(by), TRUE, FALSE)
  by1.miss <- ifelse (missing(by1), TRUE, FALSE)
  by2.miss <- ifelse (missing(by2), TRUE, FALSE)
  size.miss <- ifelse (missing(size), TRUE, FALSE)
  stat.miss <- ifelse (missing(stat), TRUE, FALSE)
  radius.miss <- ifelse (missing(radius), TRUE, FALSE)
  fill.miss <- ifelse (missing(fill), TRUE, FALSE)
  color.miss <- ifelse (missing(color), TRUE, FALSE)
  trans.miss <- ifelse (missing(trans), TRUE, FALSE)
  box_fill.miss <- ifelse (missing(box_fill), TRUE, FALSE)
  violin_fill.miss <- ifelse (missing(violin_fill), TRUE, FALSE)
  area_fill.miss <- ifelse (missing(area_fill), TRUE, FALSE)
  seg.miss <- ifelse (missing(segments), TRUE, FALSE)
  seg.y.miss <- ifelse (missing(segments_y), TRUE, FALSE)  # for Cleveland plot
  seg.x.miss <- ifelse (missing(segments_x), TRUE, FALSE)
  ellipse.miss <- ifelse (missing(ellipse), TRUE, FALSE)
  fit.miss <- ifelse (missing(fit), TRUE, FALSE)
  fit_se.miss <- ifelse (missing(fit_se), TRUE, FALSE)
  MD.miss <- ifelse (missing(MD_cut), TRUE, FALSE)
  ID.miss <- ifelse (missing(ID), TRUE, FALSE)
  lwd.miss <- ifelse (missing(lwd), TRUE, FALSE)
  out_size.miss <- ifelse (missing(out_size), TRUE, FALSE)
  out_shape.miss <- ifelse (missing(out_shape), TRUE, FALSE)
  j.y.miss <- ifelse (missing(jitter_y), TRUE, FALSE)
  j.x.miss <- ifelse (missing(jitter_x), TRUE, FALSE)
  bw.miss <- ifelse (missing(bw), TRUE, FALSE)
  n_col.miss <- ifelse (missing(n_col), TRUE, FALSE)
  n_row.miss <- ifelse (missing(n_row), TRUE, FALSE)
  add_miss <- ifelse (missing(add), TRUE, FALSE)

  if (!missing(a) || !missing(b)) box_adj <- TRUE

  if (missing(vbs_size)) if (!violin)  # wider box if no violin
    vbs_size <- ifelse (y.miss || by1.miss, vbs_size*3.75, vbs_size*5)

  if (!MD.miss && (!by.miss || !by1.miss)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Outlier analysis works only with no  by  or  by1  groups\n\n")
  }

 
  # "off" substitutes for official value of "transparent"
  fill[which(fill == "off")] <- "transparent"
  color[which(color == "off")] <- "transparent"
  ellipse_color[which(ellipse_color == "off")] <- "transparent"
  ellipse_fill[which(ellipse_fill == "off")] <- "transparent"
  add_fill[which(add_fill == "off")] <- "transparent"
  add_color[which(add_color == "off")] <- "transparent"

  if (enhance) {
    if (ellipse.miss) ellipse <- 0.95
    if (MD.miss) MD_cut <- 6
    if (add_miss) add <- "means"
    if (fit.miss) fit <- "lm"
  }

  if (fill.miss && run) {
    fill <- "gray20"
    color <- "gray60"
  }

  # presume text output to the console, could turn off at end of function
  outp <- TRUE

  txdif <- ""  # used to transfer output for diffs of two vars Cleveland sp
  
  xlab_adj <- lab_adj[1];   ylab_adj <- lab_adj[2]
  tm.adj <- margin_adj[1];  rm.adj <- margin_adj[2]
  bm.adj <- margin_adj[3];  lm.adj <- margin_adj[4]
  
  date.ts <- FALSE  # default is not a time series
  freq.poly <- FALSE  # default is not a frequency polygon

  if (show_runs) run <- TRUE
  if (run) if (cl.miss) center_line <- "default"

  if (!fit_se.miss) if (missing(fit))  fit <- "loess"
  if (fit_se.miss && plot_errors) fit_se <- 0  # default for plot_errors
  if (!by.miss && fit_se.miss && fit!="off") fit_se <- 0  # default for by 

  if (is.logical(fit))
    fit.ln <- ifelse (fit, "loess", "off")
  if (is.character(fit)) {
    if (!(fit %in% c("loess", "lm", "off", "ls", "null", "exp", "quad",
                     "power", "log"))) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "fit only for loess (non-linear), lm (linear), or null (null model),\n",
        "exp (exponential), log (log), quad (quadratic), power (any power)\n\n")
    }
    fit.ln <- fit  # fit.ln passed to .plt.main
  }

  # any bin parameter activates bins for VBS plot, or freq.poly=TRUE
  if (!missing(breaks)  ||  !missing(bin_start)  ||  !missing(bin_width)  ||
      !missing(bin_end))
    bin <- TRUE

  if (!is.null(pdf_file))
    if (!grepl(".pdf", pdf_file)) pdf_file <- paste(pdf_file, ".pdf", sep="")

  if (!data.do) if (is.null(pt.trans)) pt.trans <- 0  # trans, so dot plot

  # set object 
  if (!missing(radius) || !missing(power)) {
    object <- "bubble"  # any bubble parameter activates a bubble plot
  }
  else
    object <- "default"
  if (run) {
    object <- "both"
    n.y_var <- 1 
  }

  # --------------
  # process shapes
  if (shape[1] == "sunflower")
    object <- "sunflower"
  else {  # check for bad shapes, see if modify outlier shape 
    shp <- .plt.shapes(shape, out_shape)
    shape <- shp$shape
    out_shape <- shp$out_shape
  }

  # ------
  # see if dated or inconsistent parameter values
  .param.old(...)
  .plt.bad(x.miss, y.miss, stat, breaks, bin_start, n_row, n_col,
           MD_cut, out_cut, fit_se, ...)

  # ------------------
  # process data frame
  
  data.miss <- ifelse (missing(data), TRUE, FALSE) 

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if (!is.null(dfs)) {
    if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
      d <- mydata
      rm(mydata)
      df.name <- "mydata"
      mydata.ok <- TRUE
      options(dname = df.name)
    }
  }

  # get name of data table
  if (!mydata.ok) {
    df.name <- deparse(substitute(data))
    options(dname = df.name)
  }
 
  # if a tibble convert to data frame
  if (!is.null(dfs)) {
    if (df.name %in% dfs) {  
      if (any(grepl("tbl", class(data), fixed=TRUE))) {  # tibble to df
        data <- data.frame(data)
      }
    }
  }

  # force evaluation (not lazy) if data not specified
  if ((missing(data) && shiny)) 
    data <- eval(substitute(data), envir=parent.frame())

  # ---------------

  x.name <- deparse(substitute(x), width.cutoff = 120L)
  options(xname = x.name)

  x.in.global <- .in.global(x.name, quiet)  # in global?, includes vars lists

  #  get data to be analyzed into data.x data frame

  # process row.names if specified
  if (x.name %in% c("row_names", "row.names")) {
    # retain order of row names, otherwise will be alphabetical
    data.x <- data.frame(factor(row.names(data), levels=row.names(data)))
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label
    cat.x <- TRUE
  }

  # x not in global env, in df, specify data= forces to data frame
  else if (!x.in.global) {
    if (eval_df) {
      if (!mydata.ok) .nodf(df.name)  # check to see if df exists 
      .xcheck(x.name, df.name, names(data))  # stop if x an expression
    }
    data.vars <- as.list(seq_along(data))
    names(data.vars) <- names(data)
    x.col <- eval(substitute(x), envir=data.vars)  # col num of each var
    
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
    if (!("list" %in% class(data))) {
      data.x <- data[, x.col]  # need stringsAsFactors
      data.x <- data.frame(data.x, stringsAsFactors=TRUE)
    }      
    else {  # class of data is "list"
       data.x <- data.frame(data[[x.col]], stringsAsFactors=TRUE)
    }
    if (is.numeric(x.col))
      names(data.x) <- names(data.vars)[x.col]
    else
      names(data.x) <- x.col  # if x a vector, x.col can return names
  }  # end x in df

  # x is in the global environment (vector or data frame)
  # can only access x directly if it is not in a data frame
  else if (is.data.frame(x)) { # x a data frame
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need to specify variables for analysis,\n",
        "not a full data frame\n\n")
  }
      
  # time series in global env, deconstruct to just the dates for x var
  else if (is.ts(x)) { 
    data.x <- data.frame(.ts.dates(x))
    names(data.x) <- "date"
    date.ts <- TRUE
    if (is.null(xlab)) xlab <- ""  # unless specified, drop the axis label

    # flip x to y
    nc <- ifelse (is.matrix(x), ncol(x), 1)
    n.y_var <- nc
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
      data.x <- data.frame(x, stringsAsFactors=TRUE)  # x is 1 var
    else  # x is 1 var
      data.x <- data.frame(eval(substitute(data$x)), stringsAsFactors=TRUE)  
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
      num.cat.x <- .is.num.cat(x.call[,1], n_cat)  # small num of int values
      if (jitter_x > 0) num.cat.x <- FALSE
    }
    else {  # process ts
      nu <- length(unique(x.call[,1]))
      num.cat.x <- FALSE
    }
    
    if (!is.factor(x.call[,1])) {
      eq.int <- TRUE
      d.x <- diff(x.call[,1]) 
      eq.int <- ifelse (any(is.na(x.call[,1])), FALSE, TRUE)
      if (eq.int  &&  length(d.x) > 2) {  # no missing allowed
        for (i in 2:(length(d.x)))
          if ((abs(d.x[i-1] - d.x[i]) > 0.0000000001)) eq.int <- FALSE

        if (!num.cat.x && is.integer(x.call[,1]) &&
            !date.ts && nu <= n_cat && eq.int) {
          cat("\n")
          cat(">>> ", x.name, " has only only ", nu, " unique ",
              "integer values, but not equally spaced,\n",
              "      so treat as numerical in this analysis\n",
              "   Maybe convert to an R factor to treat as categorical\n",
              sep="")
        }
      }
    }

    if (num.cat.x && !quiet) .ncat("Plot", x.name, nu, n_cat)
    cat.x <- ifelse (num.cat.x || is.factor(x.call[,1]), TRUE, FALSE)

    if (!is.numeric(x.call[1,])  &&  object == "both") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "A run chart applies only to continuous variables\n",
        x.name, " is a categorical variable\n\n")
    }
  }  # end n.x_var == 1

  # more than one x-variable
  else {
    # no y, see if eligible for BPFM where all selected vars are cat
    # multiple lines of a continuous x-variable also can occur
    if (y.miss  &&  object != "both") {

      is.cat <- logical(length=length(x.col))
      is.nmb <- logical(length=length(x.col))
      for (i in 1:length(x.col)) {  # see if variables are all categorical
        nu <- nrow(unique(na.omit(data.x[,i])))
        num.cat <- .is.num.cat(data.x[,i], n_cat)
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
          "  or is numeric with not more than  n_cat  unique values\n\n",
          "Can set  n_cat  locally when calling Plot,\n",
          "  or globally with the function:  global\n\n")
      }
    }  # y is missing and not a line graph
    else
      cat.x <- FALSE
   
  }  # end more than 1 x-variable

   if (!is.factor(x.call[,1])) if (cat.x) x.call[,1] <- factor(x.call[,1])
 
  if (!BPFM)
    nrows <- ifelse(is.matrix(x.call[,1]), nrow(x.call[,1]), length(x.call[,1]))
  else
    nrows <- nrow(data.x)

  if (date.ts) object <- "both"


  #-----------
  # evaluate y

  if (!y.miss) {
    # get actual variable name before potential call of data$y
    y.name <- deparse(substitute(y))
    options(yname = y.name)
  
    y.in.global <- .in.global(y.name, quiet)  # in global?, includes vars lists

    # row.names deprecated
    if (deparse(substitute(y)) %in% c("row_names", "row.names")) {
      # retain order of row names, otherwise will be alphabetical
      y.call <- factor(row.names(data), levels=row.names(data))
      if (is.null(ylab)) ylab <- ""  # unless specified, drop the axis label
      cat.y <- TRUE
      data.y <- data.frame(y.call)
      #data.y <- data.frame(y.call, stringsAsFactors=TRUE)
    }
      
    # y not in global env, in df, specify data= forces to data frame
    else if (!y.in.global) {
      if (eval_df) .xcheck(y.name, df.name, names(data))  # var in df?
      data.vars <- as.list(seq_along(data))  # even if only a single var
      names(data.vars) <- names(data)  # all data in data frame
      y.col <- eval(substitute(y), envir=data.vars)  # col num selected vars
      
      if (!("list" %in% class(data))) {
        data.y <- data[, y.col]
        data.y <- data.frame(data.y, stringsAsFactors=TRUE)
     }      
     else {  # class of data is "list"
        data.y <- data.frame(data[[y.col]], stringsAsFactors=TRUE)
      }
      if (is.numeric(y.col))
        names(data.y) <- names(data.vars)[y.col]
      else
        names(data.y) <- y.col
    }  # end global y

    # y is in the global env (vector or data frame)
    else if (is.data.frame(y)) {
        # y a data frame
        data.y <- y
    }
     
    else {  # y a vector in global
      if (!is.function(y)) {
        .xstatus(y.name, df.name, quiet)  # in global note
        data.y <- data.frame(y, stringsAsFactors=TRUE)  # y is 1 var
      }
      else
        # y is 1 var
        data.y <- data.frame(eval(substitute(data$y)), stringsAsFactors=TRUE)
      names(data.y) <- y.name
    }

    n.y_var <- ncol(data.y)  # number of y-variables
    y.call <- data.y

    if (ncol(y.call) == 1) { # y is one variable

      nu <- length(unique(na.omit(y.call[,1])))
      num.cat.y <- .is.num.cat(y.call[,1], n_cat)

      if (!num.cat.y && is.integer(y.call[,1]) && nu <= n_cat) {
        cat("\n")
        cat(">>> ", y.name, " has only only ", nu, " unique ",
            "integer values, but not equally spaced,\n",
            "      so treat as numerical in this analysis\n",
            "    Maybe convert to an R factor to treat as categorical\n",
            sep="")
      }

      if (num.cat.y && !quiet) .ncat("Plot", y.name, nu, n_cat)
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

  if (!is.numeric(x.call[,1]) && run) {
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
        "An ellipse only applies to an analysis of a single,",
        "continuous y-variable\n\n")
    }
    if (y.miss) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Need a y-variable to compute an ellipse\n\n")
    }
  }
  
  if (y.miss  &&  (fit.ln != "off")  &&  n.x_var == 1  &&  (!date.ts && !run)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Fit line only applicable if only one x variable, if y is present\n\n")
  }


  # ---------------------------------------
  # ---------------------------------------
  # master control funnel for analysis type
  # ---------------------------------------

  lx <- length(x.call[,1])
  ly <- length(y.call[,1])
  n.ux <- length(unique(x.call[,1]))
  n.uy <- length(unique(y.call[,1]))
  x.unique <- ifelse (n.ux == lx, TRUE, FALSE)
  y.unique <- ifelse (n.uy == ly, TRUE, FALSE)

  # if replications from two variable cross-tabs, then go bubble plot 
  if (!y.miss) if (n_cat > 0) {  # can set n_cat > 0 to force discrete
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

  # get Trellis and T.type
  fnl <- .plt.funnel(data.do, n.x_var, y.miss, cat.x, cat.y, run, date.ts,
                     by1.miss, y.unique)
  Trellis <- fnl$Trellis 
  T.type <- fnl$T.type  # needed only for Trellis

  if (!data.do  &&  n.x_var == 1)
    if (!cat.x) violin <- FALSE

  # end master control funnel
  # -------------------------


  if (is.logical(ellipse))
    ellipse <- ifelse (ellipse, 0.95, 0.00)
  if (ellipse[1] > 0  &&  !Trellis  &&  !quiet) {
    txt <- "[Ellipse with Murdoch and Chow's function ellipse"
    cat(txt, "from their ellipse package]\n")
  } 


  # evaluate by
  #------------
  if (!by.miss) {

    # get actual variable name before potential call of data$x
    by.name <- deparse(substitute(by))
    options(byname = by.name)

    in.global <- .in.global(by.name, quiet)  # in global?

    # get conditions and check for data existing
    if (eval_df) .xstatus(by.name, df.name, quiet)

    # see if var exists in data frame, if x not in global Env or function call
    if (!missing(x) && !in.global)
      .xcheck(by.name, df.name, names(data))

    if (!in.global) {
      data.vars <- as.list(seq_along(data))  # even if only a single var
      names(data.vars) <- names(data)  # all data in data frame
      by.col <- eval(substitute(by), envir=data.vars)  # col num selected vars
      by.call <- data[, by.col]
    }
    else {  # vars that are function names get assigned to global
      by.call <- by
      if (is.function(by.call)) by.call <- eval(substitute(data$by))
    }

    if (!is.factor(by.call)) by.call <- factor(by.call)  # make a factor
  }

  else
    by.call <- NULL
  n.by <- ifelse (by.miss, 0, nlevels(by.call))

  
  # evaluate by1
  #-------------
  if (!by1.miss) {

    # get actual variable name before potential call of data$x
    by1.name <- deparse(substitute(by1))
    options(by1name = by1.name)

    in.global <- .in.global(by1.name, quiet)  # in global?

    # get conditions and check for data existing
    if (eval_df) xs <- .xstatus(by1.name, df.name, quiet)

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
  if (!by2.miss) {

    # get actual variable name before potential call of data$x
    by2.name <- deparse(substitute(by2))
    options(by2name = by2.name)

    in.global <- .in.global(by2.name, quiet)  # in global?

    # get conditions and check for data existing
    if (eval_df) .xstatus(by2.name, df.name, quiet)

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
    in.df <- FALSE
    if (missing(data)) {
      if (exists("d")) {
        if (!is.null(data)) {  # set data to NULL to make sure read from workspace 
          in.df <- ifelse (exists(size.name, where=data), TRUE, FALSE)
        }
      }
    }

    if (in.df) {
      size <- eval(substitute(data$size))
      if (!is.numeric(size)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Variable ", size.name, " must be numeric\n\n",
          "Perhaps use: by=", size.name, "\n\n")
      }
      options(sizename = size.name) # for later access
      object <- "bubble"
      if (is.null(size_cut)) size_cut <- ifelse (cat.x, 1, 2)
    }

    else  # size is not a variable
      size_cut <- 0
  }
  else
    if (missing(size_cut)) size_cut <- 1

  if (!grepl("s", vbs_plot)) size <- 0


  # evaluate ID 
  #------------
  get.ID <- FALSE
  if (y.miss && !cat.x) get.ID <- TRUE  # VBS plot 
  if (!is.null(add)) if (add[1] == "labels") get.ID <- TRUE 
  if (!y.miss) if (MD_cut>0 || out_cut>0)  # 3.9.4 set TRUE for cat
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

  # If plotting a time plot with a Date, make sure x is sorted
  if (date.ts) {
    b.name <- NULL
    sort_flag <- NULL
    if (!is.null(by.call)) if (!is.null(by.name)) {
      b.name <- by.name
      sort_flag <- ifelse (is.unsorted(by.call), FALSE, TRUE)
    }
    if (!is.null(by1.call)) if (!is.null(by1.name)) {
      b.name <- by1.name
      sort_flag <- ifelse (is.unsorted(by1.call), FALSE, TRUE)
    }

    # is.unsorted() flags sorted but in descending order
    if (is.unsorted(x.call[,1]) && is.null(by.call) && is.null(by1.call)) {
      if (is.null(b.name)) {  # not evaluated for by var
        message(">>> Warning\n",
          "The  Date  variable is not sorted in Increasing Order.\n\n",
          "For a data frame named d, enter: \n    ", 
           paste("d <- Sort(d, ", x.name, ")", sep=""), "\n",
        "Enter  ?Sort  for more information and examples.\n\n")
      }
      else {  # a by variable
        if (!sort_flag) {
          message(">>> Warning\n",
            "The  by  variable is not sorted in Increasing Order.\n\n",
            "For a data frame named d, enter: \n    ", 
            paste("d <- Sort(d, by=c(", b.name, ", ", x.name, "))", 
                sep=""), "\n",
                "Enter  ?Sort  for more information and examples.\n\n")
        }
      } 
    }  # end is.unsorted
  }  # if plotting a Date variable

  if (!run)
    n.xcol <- ifelse (y.miss, 1, ncol(x.call))  # sp matrix means only 1 xcol
  else
    n.xcol <- ncol(x.call)
  n.ycol <- ifelse (y.miss, 0, ncol(y.call))
  nn_col <- max(n.xcol, n.ycol)  # n_col goes into lattice, do not change

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
    .opendev(pdf_file, width, height)  # prepare plot window, dev or pdf


  # ------------------------------------------------
  # set object and values where needed

  # prep 1-variable bubble plot to call regular scatter plot function
  # y.call to 0
  if (is.null(y.call)  &&  cat.x  &&  n.x_var == 1  &&  data.do) {
    y.call <- data.frame(rep(0, nrow(x.call)), stringsAsFactors=TRUE)
    cat.y <- FALSE
    if (object == "default") object <- "bubble"
  }

  # if numeric x is sorted with equal intervals, set as line chart
  #   unless doing a fit line as only want one line
  # x.call does not exist for BPFM
  if (!cat.x && fit.ln=="off") if (is.numeric(x.call[,1])  &&  nrows > 2) {
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
        if (!seg.miss && !segments) object <- "point"
        if (is.null(size)) size <- 0  # by default, just plot line w/o points
      }
    }
  }

  # if numeric y is sorted with equal intervals, line chart
  if (fit.ln == "off") if (is.numeric(y.call)  &&  nrows > 2) {
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

      if (!is.unsorted(y.call) && eq.int && sum(is.na(x))==0)
        object <- "both"
    }
  }

  # set point or bubble plot
  if (object == "default") {  # set default
    if (!y.miss) {
      object <- "point"
      if (data.do) if (cat.x && cat.y) object <- "bubble"
    }
    else if (stat %in% c("count", "proportion", "%")) {
      object <- "point"
      n.y_var <- 1
    }
    else {
      if (data.do) object <- ifelse (cat.x, "bubble", "point")
      if (BPFM) object <- "bubble"  # BPFM
    }
  }

  if (y.miss  &&  !date.ts  &&  object != "bubble") {  

    if (!cat.x) { 

      # run chart prep of x.call and y.call
      if (run) {  # run chart
        y.call <- x.call
        cat.y <- cat.x
        options(yname=x.name)
        
        options(xname="Index")
        x.call <- data.frame(1:nrow(x.call), stringsAsFactors=TRUE)
        names(x.call) <- "Index"
      }

      else if (stat %in% c("count", "proportion", "%")) {  # frequency polygon

        ssstuff <- .ss.numeric(x.call[,1], digits_d=digits_d, brief=TRUE)
       
        values <- NULL
        hist.cumul <- ifelse(cumulate, "on", "off")
        reg <- "snow2"  # applies to cumulative histogram
        h <- .hst.main(x.call[,1], pt.fill, pt.color, pt.trans, reg,
           rotate_x, rotate_y, offset,
           breaks, bin_start, bin_width, bin_end,
           proportion, values, hist.cumul,
           xlab, ylab, main, sub, quiet=quiet, do_plot=FALSE,
           fun_call=NULL, ...) 

        n_cat <- 0  # not many midpoints, do not want to trigger num.cat
        x.call <- h$mids
        y.call <- h$counts
        if (stat == "count")
          ylab <- paste("Count of", x.name)
        else {
          y.call <- y.call / sum(y.call)
          ylab <- paste("Proportion of", x.name)
        }

        # last assignment of object, now determined
        object <- "both"  # do freq poly as a line chart
        freq.poly <- TRUE  # need to indicate fill possibility
        center_line <- "off"  # not meaningful here
  
        cat.x <- FALSE
        cat.y <- FALSE
        
        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"

          fncl <- .fun_call.deparse(fun_call) 
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

          bw_new <- pretty((x.call[2] - x.call[1]) / 1.5)  # arbitrary new bw
          fc <- ""
          if (!grepl("bin_width", fncl))
            fc <- paste(fc, ", bin_width=", as.character(bw_new[1]), sep="")
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

          bx <- .bx.stats(x.call, box_adj, k.iqr, a, b)
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

        x.call <- data.frame(x.call, stringsAsFactors=TRUE)
        y.call <- data.frame(y.call, stringsAsFactors=TRUE)

      }  # end freq polygon
    }  # end !cat.x

    else {  # cat.x
      # just x variable, so set y.call to plot points for count and prop
      if (stat %in% c("count", "proportion", "%")) {

        if (Trellis) {  # follow dot plot format and do horizontal plot
          cat.y <- FALSE
          if (seg.x.miss) segments_x <- TRUE
          ylab <- ifelse (stat == "count", "Count of", "Proportion of")
          ylab <- paste(ylab, x.name)
          x.call <- data.frame(x.call, stringsAsFactors=TRUE)
        }  # end if Trellis

        else {  # not Trellis, so manually flip to match dot plot style
          cat.x <- FALSE
          if (seg.y.miss) segments_y <- TRUE
          if (stat == "count")
            xlab <- "Count of"
          else if (stat == "proportion")
            xlab <- "Proportion of"
          else 
            xlab <- "Percentage of"
          xlab <- paste(xlab, x.name)
          ylab <- NULL
          frq <- table(x.call)
          if (stat == "proportion") frq <- frq / sum(frq)
          if (stat == "%") frq <- (frq / sum(frq)) * 100
          if (is.factor(x.call))  # preserve ordering, will lose order attribute
            y.call <- factor(names(frq), levels=levels(x.call))
          else
            y.call <- factor(names(frq))
          cat.y <- TRUE
          num.cat.y <- TRUE
          options(yname=x.name)
          y.call <- data.frame(y.call, stringsAsFactors=TRUE)
          x.call <- data.frame(as.vector(frq), stringsAsFactors=TRUE)
        }
      }  # end values in
    }  # end cat.x

  }  # end is null y.call


  # object now determined: "both", "point", "bubble", or "sunflower"

  # bubble size
  if (radius.miss) {
    radius <- ifelse (length(size) > 1, .12, .22)
  }

  # size of lines for line chart
  if (object == "both") {
    size.ln <- lwd  # size of lines
    if (lwd.miss && Trellis) size.ln <- 1.5  # smaller default for Trellis
  }
  else
    size.ln <- 1  # could be NULL?
  
  # size of fit line
  # windows line too thin at 1, but no increments allowed, and 2 is too thick
  if (fit.ln != "off") if (is.null(fit_lwd)) fit_lwd <- getOption("fit_lwd") 
  #   fit_lwd <- ifelse(.Platform$OS == "windows", 2, 1.5)

  # size of points
  if (size.miss)  # pt.size not set yet
    pt.size <- ifelse (.Platform$OS == "windows", 1.00, 0.80)
  else {  # size had been set
    if (length(size) == 1)
      scale.pt <- ifelse (.Platform$OS == "windows", 1.00, 0.80)
    else   # size var
      scale.pt <- 1  # forget Win/Mac scaling for size var, ruins size for Mac
    pt.size <- size * scale.pt
  }

  if (size.miss) {  # pt.size is a scaler
    if (object == "both") {
      pt.size <- 0.77 * pt.size  # default pt size for lines
      if (!("transparent" %in% area_fill))  
        pt.size[1] <- 0  # default no points if area shown
      else if (nrows > 50) {
        pt.size <- .9 - 0.002*nrows
        if (pt.size < 0.10) pt.size <- 0
      }
    }  # end both
  }

  # get fill and color
  ord.by.call <- is.ordered(by.call)
  fc <- .plt.colors(object, nn_col, n.by, theme, fill, fill.miss,
            color, color.miss, area_fill, area_fill.miss, trans, stack,
            n.ycol, n.y_var, ord.by.call, run, pt.size)

  pt.fill <- fc$pt_fill
  pt.color <- fc$pt_col
  area_fill <- fc$area_fill


  # ------------------------------------------------
  # analysis
  # ------------------------------------------------
  
  if (getOption("suggest")) {  # already did this at line 1045
    # function call for suggestions
    fncl <- .fun_call.deparse(fun_call) 
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)
  }


  # Trellis plot
  # ------------

  if (Trellis && do_plot) {

    if (T.type %in% c("cont", "cont_cont", "cont_cat")) {

      # VBS plot only lattice graphics that (currently) does not use Trellis
      txt <- ifelse (!is.null(by1.call), "[Trellis", "[Violin/Box/Scatterplot")
      if (!quiet)
        cat(paste(txt, "graphics from Deepayan Sarkar's lattice package]\n"))

      # box_fill and violin_fill
      n.by1 <- length(levels(by1.call))
      n.by2 <- ifelse (by2.miss, 1, length(levels(by2.call)))
      n.lvl <- n.by1 * n.by2
      ord.by.call <- is.ordered(by1.call)

      if (!y.miss) {
        if (xor(cat.x, cat.y)  &&  n.by1 > 1) {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "The way to submit this analysis is to have both categorical\n",
            "  variables be   by1  and  by2  variables, respectively.\n\n")
        }
      }

      box_fill <- .plt.fill(box_fill, box_fill.miss, ord.by.call,
                            n.by1, n.lvl, theme)

      if (!violin_fill.miss || vbs_plot %in% c("v", "vs"))  # otherwise default
        violin_fill <- .plt.fill(violin_fill, violin_fill.miss, ord.by.call,
                                 n.by1, n.lvl, theme)
      else
        for (i in 1:n.lvl) violin_fill[i] <- violin_fill[1]

      # VBS plot, cont_cont is a 2-var scatterplot, not a VBS plot
      if (y.miss  &&  !run  &&  T.type != "cont_cont") {

        if (n.by > 1) {
          if (color.miss) {
            if (getOption("theme") %in% c("gray", "white"))
              pt.color <- getColors("grays", n=n.by)
            else
              pt.color <- getColors("hues", n=n.by, l=40)
          }
          for (i in 1:n.by) {
            if (fill.miss) {
              pt.fill[i] <- pt.color[i]
              pt.fill[i] <- .maketrans(pt.fill[i], (1-trans)*256)
            }
            if (box_fill.miss) box_fill[i] <- .maketrans(box_fill[i], 0.6*256)
          }
        }
        else {  # n.by is 0 or 1
          if ("black" %in% vbs_pt_fill) {
            if (fill.miss) pt.fill <- "black"
            if (trans.miss) pt.trans <- 0
            if (color.miss) pt.color <- "black"
          }
          if ("default" %in% vbs_pt_fill) {
            if (fill.miss) pt.fill <- "black"
            if (trans.miss) pt.trans <- getOption("trans_pt_fill")
            if (color.miss) pt.color <- "black" 
          }
          else {
            if (fill.miss) pt.fill <- vbs_pt_fill
            if (trans.miss) pt.trans <- getOption("trans_pt_fill")
            if (color.miss) pt.color <- getOption("pt_color")
          }
        }  # end n.by is 0 or 1

        # grayscale
        if (theme %in% c("gray", "white"))
          if (any(pt.size > 0.4)) if (out_shape.miss) out_shape <- 23
        if (getOption("sub_theme") == "black") {
          if (fill.miss) pt.fill <- "black"
          if (color.miss) pt.color <- "black"
        } 

        # get some VBS parameters
        VBS <- .param.VBS(x.call[,1], ID.call, by1.call, by1.miss, by.call,
                by.miss, bw, bw.miss, bw_iter, iter.details,
                lx, n.ux, k.iqr, box_adj, a, b,
                x.name, by1.name, by.name, vbs_plot,
                n_col.miss, n_row.miss,
                size, out_size, out_size.miss,
                j.x.miss, jitter_x, j.y.miss, jitter_y,
                bin, breaks, bin_start, bin_width, bin_end, proportion, 
                digits_d, quiet, fun_call, ...)
        pt.size <- VBS$pt.size
        pt.out_size <- VBS$out_size
        jitter_y <- VBS$jitter_y
        jitter_x <- VBS$jitter_x
        bw <- VBS$bw
        adj.bx.ht <- VBS$adj.bx.ht
        output <- VBS$output  # text output
      }  # end VBS plot

      else {  # Trellis but not a VBS plot, such as when there is a y-variable

        # e.g., y.miss=FALSE,  Plot(Years, Salary, by1=Gender)
        adj.bx.ht <- nrows  # just to adjust box height
        if (size.miss) pt.size <- pt.size * .8
      }

      if (jitter_x > 0)  # not available in stripplot
        x.call[,1] <- jitter(x.call[,1], factor=jitter_x) 

      if (!missing(vbs_plot)) 
        if (!grepl("s", vbs_plot)) pt.size <- 0  # gets rescaled if earlier

      # n_col is null for at Plot(x), Plot(x, by=), Plot(x, by1=)
      if (n_col.miss && n_row.miss && !is.null(by1.call))
        n_col <- 1  # default n_col for Trellis

      .plt.lattice(x.call[,1], y.call[,1], by1.call, by2.call, by.call,
                   adj.bx.ht, object, n_row, n_col, aspect,
                   pt.fill, area_fill, pt.color, panel_fill, panel_color,
                   pt.trans, pt.size, size.ln, 
                   xlab, ylab, main, shape, lab_cex, axis_cex,
                   max(ellipse), ellipse_color, ellipse_lwd,
                   fit.ln, fit_power, fit_color, fit_lwd, fit_se,
                   plot_errors, area_origin, jitter_y,
                   violin, violin_fill, box, box_fill, 
                   bw, vbs_size, box_adj, a, b, k.iqr, fences, vbs_mean,
                   out_shape, pt.out_size,
                   out_fill, out_color, out2_fill, out2_color,
                   ID.call, out_cut, ID_color, ID_size,
                   rotate_x, rotate_y, width, height, pdf_file,
                   T.type, ...)

    }  # end T.type != "dot"

    else {  # bar plot

      if (seg.x.miss) segments_x <- TRUE
      .bar.lattice(x.call[,1], by1.call, by2=NULL, n_row, n_col, aspect,
                   proportion, pt.fill, pt.color,
                   pt.trans, pt.size, xlab, ylab, main,
                   rotate_x, offset,
                   width, height, pdf_file,
                   segments_x, breaks=NULL, T.type, quiet, ...)
    }

  }  # end Trellis && do_plot


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

    .dpmat.main(data[,x.col], mylabs, sort_yx,
      getOption("bar_fill_cont"), pt.color, panel_fill,
      pt.trans, shape, panel_color,
      low_fill, hi_fill,
      xy_ticks, xlab, ylab, main, sub, size,
      radius, size_cut, bubble_text, power,
      bm.adj, lm.adj, tm.adj, rm.adj,
      value_labels, rotate_x, rotate_y, offset, quiet, do_plot, fun_call, ...)
  }


  # ------------------
  # scatterplot matrix

  else if (spmat && do_plot) {
    bckg <- ifelse(panel_fill=="transparent",
                   getOption("window_fill"), panel_fill)
    .plt.mat(data.x, fit=fit, col_fill=pt.fill, col_color=pt.color,
                     col.bg=bckg, col_trans=pt.trans,
                     pt.size=pt.size, size.miss=size.miss)
  }
  
  else if (n_bins > 1) {  # bin the x-axis, compute mean or median of y by bin

   if (stat.miss) stat <- "mean"
   if (seg.miss) segments <- TRUE
   if (size.miss) pt.size <- 1.1
   if (is.null(digits_d)) digits_d <- 3
   nm.x <- x.name
   nm.y <- y.name
   # size is NULL if not specified
   .plt.bins(x.call[,1], y.call[,1], nm.x, nm.y, stat, n_bins, 
             segments=segments, size=size, digits_d, scale_x, scale_y,
             fill=pt.fill, color=pt.color, trans=pt.trans,
             quiet=quiet)
  }

  # all the other analyses
  # ----------------------

  else { 

    if (stat %in% c("sum", "mean", "sd", "dev", "min", "median", "max")) {

      n_cat <- 0
      means <- FALSE
      if (seg.x.miss) segments_x <- TRUE

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
                               digits_d=digits_d, brief=TRUE, y.name=x.name)
          txout <- stats$tx
          options(xname = x.name)  # reverse back
          options(yname = y.name)
        }
        else  {
          stats <- .ss.factor(x.call, digits_d=digits_d, x.name=x.name,
                              brief=TRUE)
          txout <- stats$counts
        }

        class(txout) <- "out"

        output <- list(out_txt=txout)
        class(output) <- "out_all"
        print(output)
      }

    # set up new x.call and y.call for stats
    # only does overall stat, so not applicable if a by parameter
      if (stat == "sum") {
        ylab <- paste("Sum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], sum, na.rm=TRUE)
      }
      if (stat == "mean") {
        ylab <- paste("Mean of", y.name)
        out <- tapply(y.call[,1], x.call[,1], mean, na.rm=TRUE)
      }
      if (stat == "sd") {
        ylab <- paste("Standard Deviation of", y.name)
        out <- tapply(y.call[,1], x.call[,1], sd, na.rm=TRUE)
      }
      if (stat == "dev") {
        ylab <- paste("Mean Deviations of", y.name)
        out <- tapply(y.call[,1], x.call[,1], mean, na.rm=TRUE)
        out <- out - mean(out, na.rm=TRUE)
      }
      if (stat == "min") {
        ylab <- paste("Minimum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], min, na.rm=TRUE)
      }
      if (stat == "median") {
        ylab <- paste("Median of", y.name)
        out <- tapply(y.call[,1], x.call[,1], median, na.rm=TRUE)
      }
      if (stat == "max") {
        ylab <- paste("Maximum of", y.name)
        out <- tapply(y.call[,1], x.call[,1], max, na.rm=TRUE)
      }

      x.call <- factor(names(out))
      y.call <- as.vector(out)

      x.call <- data.frame(x.call, stringsAsFactors=TRUE)
      y.call <- data.frame(y.call, stringsAsFactors=TRUE)

    # switch
      temp.call <- x.call
      x.call <- y.call
      cat.x <- FALSE
      y.call <- temp.call
      cat.y <- TRUE
      xlab <- ylab
      ylab <- NULL
      options(yname = x.name)
      if (segments_x) {
        segments_x <- ifelse(segments_y, TRUE, FALSE)
        segments_y <- TRUE
       }
    }  # sum, mean, sd, min, median, max


    # 2-variable scatter plot
    # bubble plot for 1-variable (y.call=0) and 2-variable
    # line chart

    if ((!is.null(y.call) || date.ts) &&
        object %in% c("point", "bubble", "both", "sunflower")) {

      if (object == "point"  &&  data.do){  # for Cleveland dot plot
        if (!cat.x && cat.y && y.unique) {  # no sort_xy option
          if (seg.y.miss) if (y.unique && cat.y) segments_y <- TRUE
          if (seg.x.miss) if (x.unique && cat.x) segments_x <- TRUE
          if (sort_yx.miss) if (n.x_var <= 2) sort_yx <- "+"
        }
      }

     # sort y by x option (intended for Cleveland dot plot)
      tx <- character(length=0)
      if (sort_yx != "0") {
        srt.dwn <- ifelse (sort_yx == "-", FALSE, TRUE)
        if (n.x_var == 1) {  # one x-variable
          ord <- order(x.call[,1], decreasing=srt.dwn)
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

      }  # end sort_yx
      else { # sort_yx==0
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
        if (is.null(size)) pt.size <- 1.25

      out_ind <- NULL
      txout <- ""
      if (!y.miss && !Trellis) if(n.x_var == 1  &&  n.y_var == 1)
        if (MD_cut > 0  ||  out_cut > 0) {
          otl <- .plt.MD(x.call[,1], y.call[,1], ID.call, MD_cut, out_cut)  
          txout <- otl$tx
          out_ind <- otl$out_ind
      }


      if (do_plot) {

        if (nrow(x.call) != nrow(y.call))  {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "number of elements in x: ", nrow(x.call), "\n",
            "number of elements in y: ", nrow(y.call), "\n\n",
            "The number of elements must be equal, probably\n",
            "  have variables from user workspace so maybe\n",
            "  use the  remove function, e.g., remove(x)\n\n")
        }

        if (object == "both"  &&  nn_col > 1) {
           # change to multi later
           if ("on" %in% area_fill) fill <- getOption("violin_fill")
        }

        # by default display center_line only if runs about a mean
        if (run  &&  center_line == "default") {
          y.clean <- y.call[complete.cases(y.call), 1]  # converts df to vector 
          m <- mean(y.clean)
          n.change <- 0
          for (i in 1:(length(y.clean)-1))
            if ((y.clean[i+1] > m) != (y.clean[i] > m)) n.change <- n.change+1
          if (n.change/(length(y.clean)-1) < .15)
            center_line <- "off"
          else
            center_line <- "median"
        }
          
        # default jitter for cat-cont scatterplot
        num.c.x <- FALSE
        num.c.y <- FALSE
        if (pt.size[1]>0 && n.x_var==1 && stat!="mean") {

          if (is.factor(x.call[,1]))
            unq.x <- levels(x.call[,1])
          else
            unq.x <- unique(x.call[,1])
          nx.unq <- length(unq.x)
          if (nx.unq <= 12) num.c.x <- TRUE
          max.dup <- 0
          if ((cat.x || num.c.x) && !cat.y) if (j.x.miss) {
            for (i in 1:nx.unq) {
              wx <- which(x.call[,1] == unq.x[i])
              dps <- sum(duplicated(na.omit(y.call[,1][wx])))
              if (dps > max.dup) max.dup <- dps
            }
            if (max.dup > 0) jitter_x <- 0.20 + (0.014*max.dup)
            if (jitter_x > 1) jitter_x <- 1
          }
                
          if (is.factor(y.call[,1]))
            unq.y <- levels(y.call[,1])
          else
            unq.y <- unique(y.call[,1])
          ny.unq <- length(unq.y)
          if (ny.unq <= 12) num.c.y <- TRUE
          max.dup <- 0
          if (!cat.x && (cat.y || num.c.y)) if (j.y.miss) {
            for (i in 1:ny.unq) {
              wy <- which(y.call[,1] == unq.y[i])
              dps <- sum(duplicated(na.omit(x.call[,1][wy])))
              if (dps > max.dup) max.dup <- dps
            }
            if (max.dup > 0) jitter_y <- 0.20 + (0.014*max.dup)
            if (jitter_y > 1) jitter_y <- 1
          }
        }

        reg <- .plt.main(x.call, y.call, by.call, n_cat,
          cat.x, num.cat.x, cat.y, num.cat.y,
          object, stat,
          pt.fill, area_fill, pt.color,
          pt.trans, segment_color, 
          xy_ticks, xlab, ylab, main, main_cex,
          sub, value_labels,
          rotate_x, rotate_y, offset, proportion, origin_x,
          pt.size, size.ln, shape, means, 
          segments, segments_y, segments_x,
          smooth, smooth_points, smooth_size, smooth_exp, smooth_bins,
          radius, power, size_cut, bubble_text, low_fill, hi_fill,
          ID.call, ID_color, ID_size, out_ind,
          out_fill, out_color, out_shape, out_shape.miss,
          fit.ln, fit_power, fit_color, fit_lwd,
          fit_se, se_fill, plot_errors,
          ellipse, ellipse_color, ellipse_fill, ellipse_lwd,
          run, center_line, show_runs, stack,
          freq.poly, jitter_x, j.x.miss, jitter_y, j.y.miss, 
          xlab_adj, ylab_adj, bm.adj, lm.adj, tm.adj, rm.adj,
          scale_x, scale_y, pad_x, pad_y, legend_title, 
          add, x1, x2, y1, y2, add_cex, add_lwd, add_lty,
          add_color, add_fill, add_trans, quiet, ...)

        if (fit.ln != "off") {
          mse <- reg$mse
          b0 <- reg$b0
          b1 <- reg$b1
          Rsq <- reg$Rsq
          by.cat <- reg$by.cat
        }
        else {
          mse <- NULL
          by.cat <- NULL
        }

        if (outp && !quiet) {  # text output

          txprm <- ""

         # if pt.fill or pt.color are vectors, then a warning message otherwise
         if (length(pt.fill) == 1)
            pnt.f <- pt.fill
          else
            pnt.f <- paste(pt.fill, collapse=" ")
          if (length(pt.color) == 1)
            pnt.c <- pt.color
          else
            pnt.c <- paste(pt.color, collapse=" ")
 
          if (xor(cat.x, cat.y) || xor(num.c.x, num.c.y)) {  # show jitter
            tx <- character(length = 0)
            tx[length(tx)+1] <- "Some Parameter values (can be manually set)"
            tx[length(tx)+1] <- .dash2(55)
            tx[length(tx)+1] <- paste("fill:", pnt.f,
                "  filled color of the points")
            tx[length(tx)+1] <- paste("color:", pnt.c,
                " edge color of the points")
            tx[length(tx)+1] <- paste("size:", .fmt(pt.size,2),
                " size of plotted points")
            tx[length(tx)+1] <- paste("jitter_y:", .fmt(jitter_y,2),
                " random vertical movement of points")
            tx[length(tx)+1] <- paste("jitter_x:", .fmt(jitter_x,2),
                " random horizontal movement of points")
            txprm <- tx
          }

          if (object == "bubble") {  # primarily to radius, power
            tx <- character(length = 0)
            tx[length(tx)+1] <- "Some Parameter values (can be manually set)"
            tx[length(tx)+1] <- .dash2(55)
            tx[length(tx)+1] <- paste("fill:", pt.fill,
                "  filled color of the points")
            tx[length(tx)+1] <- paste("color:", pt.color,
                " edge color of the points")
            tx[length(tx)+1] <- paste("radius:", .fmt(radius,2),
                "       size of largest bubble")
            tx[length(tx)+1] <- paste("power:", .fmt(power,2),
                "    relative bubble sizes")
            txprm <- tx
          }

          if (n_bins == 1) {  # if binning, .plt.bins does its own output
            class(txprm) <- "out"
            .plt.txt(x.call, y.call, stat, object, cat.x, cat.y,
              xlab, ylab, fit, n.by, mse, b0, b1, Rsq, by.cat,
              center_line, run, show_runs,
              proportion, size, radius, digits_d, fun_call, txdif)

            class(txout) <- "out"  # MD outlier analysis
            output <- list(out_outlier=txout, outlier_indices=out_ind,
                           out_parm=txprm)
            class(output) <- "out_all"
          }
        }  # end text output

      }  # end do_plot

    }  # end 2-variable scatter plot, line plot, bubble plot

  }  # end all other analyses
  # -------------------------


  # terminate pdf graphics system if used
  if (!is.null(pdf_file)) {
    dev.off()
    if (!quiet) .showfile(pdf_file, "Plot")
  }

  # reset
  options(xname=NULL)
  options(yname=NULL)
  options(by1name=NULL)
  options(by2name=NULL)
  options(byname=NULL)

  # display text output from Plot() unless turned off
  # outp generally set TRUE when here, but not always
  # T.type is the type of Trellis plot, otherwise is NULL
  if (!quiet && n_bins==1) {
    if (Trellis) {  # only VBS plots have output processed here
      if (!(T.type %in% c("cont", "cont_cat"))) outp <- FALSE
    }
    if (n.x_var > 1  &&  y.miss) outp <- FALSE  # scatterplot matrix
    if (outp) return(output)
  }
}
