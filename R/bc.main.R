.bc.main <-
function(x, y, by, stack100,
         fill, color, col.trans, fill_split, theme,
         horiz, gap, prop, scale_y,
         xlab, ylab, main,
         value_labels, label_max, beside,
         rotate_x, offset, break_x, sort_x,
         values, values_color, values_size, values_digits,
         values_pos, values_cut,
         xlab_adj, ylab_adj, bm.adj, lm.adj, tm.adj, rm.adj,
         pad_y_min, pad_y_max,
         legend_title, legend_position, legend_labels,
         legend_horiz, legend_size, legend_abbrev, legend_adj,
         add, x1, x2, y1, y2, out_size, digits_d, do_plot, quiet, ...) {


  multi <- ifelse (is.data.frame(x), TRUE, FALSE)
  y.given <- ifelse (!is.null(y), TRUE, FALSE)
  is.ord <- ifelse (is.ordered(x) || is.ordered(by), TRUE, FALSE)

  if (stack100) prop <- TRUE
  if (!is.null(by)  &&  prop) stack100 <- TRUE  # prop deprecated

  if (is.null(values_digits)) {
    if (y.given) {
      if (max(abs(y)) > 9999)
        values_digits <- 0
      else {
        if (!is.null(y))
          values_digits <- .getdigits(y,0) - 1
        else
          values_digits <- 0
      }
    }
  }

  # if x is integer, not labeled correctly, set to factor
  if (!is.data.frame(x))
    if (length(unique(x)) != length(x)) if (!is.factor(x)) x <- factor(x)

  if ( (is.table(x) || is.matrix(x)) && is.null(legend_title) ) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Need to specify a value for:  legend_title\n\n")
  }

  # --------------------------------------
  # axis values, axis labels, legend setup

  # get axis_x_cex, axis_y_cex
  axis_x_cex <- ifelse(is.null(getOption("axis_x_cex")),
    getOption("axis_cex"), getOption("axis_x_cex"))
  axis_y_cex <- ifelse(is.null(getOption("axis_y_cex")),
    getOption("axis_cex"), getOption("axis_y_cex"))
# adj <- .RSadj(axis_cex=axis_x_cex); axis_x_cex <- adj$axis_cex
# adj <- .RSadj(axis_cex=axis_y_cex); axis_y_cex <- adj$axis_cex

  # get lab_x_cex  lab_y_cex
  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_y_cex <- getOption("lab_y_cex")
  lab_x_cex <- ifelse(is.null(lab_x_cex), lab_cex, lab_x_cex)
  lab_y_cex <- ifelse(is.null(lab_y_cex), lab_cex, lab_y_cex)
# adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex
# adj <- .RSadj(lab_cex=lab_y_cex); lab_y_cex <- adj$lab_cex
  gl <- .getlabels(xlab, ylab, main, by.nm=TRUE, lab_x_cex=lab_x_cex,
                   lab_y_cex=lab_y_cex, flip=horiz)
  x.name <- gl$xn;  x.lbl <- gl$xl;  y.lbl <- gl$yl
  x.lab <- ifelse (horiz, gl$yb, gl$xb)
  main.lab <- gl$mb
  sub.lab <- gl$sb
  by.name <- y.lbl
  if (!is.null(by) || is.data.frame(x))
    by.name <- getOption("byname")
  ylab_keep <- ifelse (is.null(ylab), FALSE, TRUE)

  if (ylab_keep) {
      y.lab <- ylab
  }
  else {  # First part of y-axis label
    if (!is.vector(x)) {
      txt <- "Proportion"
      if (!is.null(by) && prop && !beside) {
        txt <- paste("Cell % within")
      }
      if (is.null(y))
         if (!prop) ylab <- "Count" else ylab <- txt
    }
    else {
      y.lab <- x.name
      if (length(unique(x)) != length(x)) x.lab <- ""
    }
  }

  if (is.null(ylab))
    done <- FALSE
  else
    done <- ifelse (grepl("of", ylab, fixed=TRUE), TRUE, FALSE)

  if (!ylab_keep) {
    if ((!prop || is.null(by)) && is.null(y) && !done &&
         !is.vector(x))
      y.lab <- paste(ylab, "of", x.name)
    if (!is.null(by)) {
      if (!beside) {
        txt <- paste(ylab, "of", x.name)
        y.lab <- ifelse (!prop,
                         txt, paste(ylab, x.name, "by", by.name))
      }
      else {
        if (!prop)
          y.lab <- "Count"
        else
          y.lab <- "Percentage"
      }
    }
    if (!is.null(y)) y.lab <- getOption("yname")
  }

  if (is.matrix(x)) {  # get the variable names as counts entered directly
    options(xname = x.lab)
    options(byname = legend_title)
  }

  # get legend title, l.lab
  if (!is.null(legend_title))
    l.lab <- legend_title
  else
    if (horiz) {
      if (!is.null(by)) if (exists("x.lbl")) {
        if (length(x.lbl) == 0)
          l.lab <-  by.name
        else
          l.lab <- paste(by.name, ": ", x.lbl, sep="")
      }
    }
    else {
      if (!is.null(by)) if (exists("y.lbl")) {
        if (length(y.lbl) == 0)
          l.lab <-  by.name
        else
          l.lab <- paste(by.name, ": ", y.lbl, sep="")
      }
    }


  # --------------------------
  # get missing, data entered?

  x.miss <- sum(is.na(x))
  by.miss <- NULL
  by.miss <- if (!is.null(by)) sum(is.na(by))

  # entered counts typically integers as entered but stored as type double
  # if names(x) or rownames(x) is null, likely data from sample and c functions
  # y is getting data values directly from a data frame with counts entered
  entered.pre <- FALSE
  if (!is.matrix(x) && !is.null(names(x))) entered.pre <- TRUE
  if (is.matrix(x) && !is.null(rownames(x))) entered.pre <- TRUE
  entered <- ifelse (!is.integer(x) && is.double(x) && entered.pre, TRUE, FALSE)

  if (is.null(by) && beside && !entered) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "beside=TRUE  is not valid for analysis of only one variable.\n\n")
  }


  # -------------------------------------------
  # get y variable, either directly or tabulate

  # y is provided, no tabulation
  if (!is.null(y)) {
    entered <- TRUE

    if (horiz) {
      tmp <- y.lab; y.lab <- x.lab;  x.lab <- tmp
    }

    if (is.null(by)) {  # no by variable
      yn <- getOption("yname")
      if (!is.numeric(y) > 0) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "y-values, those from the 2nd unlabeled variable, ", yn, ",",
          " must be\n", " numeric\n\n",
          "A  by  variable is categorical, with only several unique values\n",
          "It appears that ", yn, " is a by variable\n",
          "As of lessR 3.5.6 to specify a  by  variable in the function call\n",
          "  precede its name with:  by=", "\n\n")
      }

      if (anyDuplicated(names(x)) > 0) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "The data contain duplicated values of variable: ", x.name, "\n\n")
      }

      x.temp <- x
      x <- y
      names(x) <- x.temp
      x <- as.table(x)
      if (prop) {
        x.count <- x  # save table of counts for possible bar display
        x <- x/sum(x)
      }
    }

    else {  # a by variable
      x.temp <- x
      unq.x <- na.omit(unique(x))
      unq.by <- na.omit(unique(by))

      do.row <- ifelse (x[1] == x[2], FALSE, TRUE)  # x is outer loop
      m <- matrix(y, nrow=length(unique(by)), byrow=do.row)
      colnames(m) <- unq.x
      rownames(m) <- unq.by
      m <- as.table(m, dnn=c(by.name, x.name))
      names(dimnames(m)) <- c(by.name, x.name)
      x <- m
      if (stack100) {
        x.count <- x  # save table of counts for possible bar display
        x <- prop.table(x, 2)  #  100% within bar chart
      }
    }
  }  # end y is present


  # x = tabulate for counts

  if (!entered) {  # so tabulate
    if (!is.data.frame(x)) { # a single x variable

      if (is.null(by)) {
        x.temp <- x  # save counts, to be restored for text output
        x <- table(x, dnn=NULL)
        if (prop) {
          x.count <- x  # save table of counts for possible bar display
          x <- x/sum(x)
        }
      }
      else {  # a by variable
        if (length(x) == length(by))
          x.temp <- table(by, x, dnn=c(by.name, x.name))
        else {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
          x.name, " and ", by.name, " must be of the same size\n\n",
          "Size of ", x.name, ": ", length(x), "\n",
          "Size of ", by.name, ": ", length(by), "\n\n", sep="")
        }
        x <- x.temp
        if (stack100) {
            x.count <- x  # save table of counts for possible bar display
            x <-prop.table(x, 2)  #  100% within bar chart
        }
      }
    }   # end single x value

    else {  # x is a data frame, so combine x's
      if (!is.factor(x[,1])) {
        resp <- unique(x[,1])
        for (i in 2:ncol(x)) resp <- union(resp, unique(x[,i]))
        resp <- sort(resp)  # sort removes any NA
      }
      else {  # is factor
        resp <- levels(x[,1])
        for (i in 2:ncol(x)) resp <- union(resp, levels(x[,i]))
      }
      n.resp <-length(resp)
      frq <- matrix(nrow=n.resp, ncol=ncol(x))  # all elements NA
      frq <- apply(frq, c(1, 2), function(x) 0)
      rownames(frq) <- as.character(resp)
      colnames(frq) <- names(x)

      # maybe an x not has a value in full set
      for (i in 1:ncol(x)) {
        tblx.i <- table(x[,i])
        k <- 0
        for (j in 1:n.resp) {
          if (rownames(frq)[j] %in% names(table(x[,i]))) {
            k <- k + 1
            frq[j,i] <- tblx.i[k]
          }
        }
      }  # end col i of x

      x <- as.table(frq)
      if (is.numeric(resp))
        wt <- resp
      else
        wt <- 1:n.resp
      wm <- double(length=ncol(x))
      for (i in 1:ncol(x)) wm[i] <- weighted.mean(wt, x[,i])

      if (sort_x != "0") {
        srt.dwn <- ifelse (sort_x == "-", TRUE, FALSE)
        m.o <- order(wm, decreasing=srt.dwn)
        x <- x[,m.o]
        wm <- wm[m.o]
      }
    }  # x is a data frame
  }  # end !entered so tabulated


  if (prop) {
    if (any(is.nan(x))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Some cells (frequencies) are zero\n",
        "Division to calculate proportions not possible\n",
        "Run analysis without  proportion  to identify the 0 cells\n\n")
    }
  }

  if (!is.null(fill_split)) {
    if (sort_x != "0") {
      srt.dwn <- ifelse (sort_x == "-", TRUE, FALSE)
      x <- x[order(x, decreasing=srt.dwn)]
    }
    fill <- character(length=length(x))  # fill starts over
    f.c <- character(length=2)
    chroma <- ifelse (theme %in% c("gray", "white"), 0, 55)
    hue <- .get.h(theme)
    f.c[1] <- hcl(hue, chroma, l=30)
    f.c[2] <- hcl(hue, chroma, l=70)
    for (i in 1:length(x))
      fill[i] <- ifelse (x[i] <= fill_split, f.c[1], f.c[2])
  }

  # ------------
  # sort options
  if (sort_x != "0") {
    srt.dwn <- ifelse (sort_x == "-", TRUE, FALSE)
    if (!is.matrix(x)) {
      x <- x[order(x, decreasing=srt.dwn)]
    }
    else {
      x.df <- as.data.frame.matrix(x, nrow=2)
      x.df <- x.df[order(apply(x.df, 2, sum), decreasing=srt.dwn)]
      x <- as.table(as.matrix(x.df))
    }
  }

  n.levels <- ifelse (is.matrix(x), nrow(x), length(x))


  # -------------
  # preliminaries

  add_top <- 0.05  # old version of pad_y_max, no longer a parameter
  if (length(values_pos > 0))
    if (values_pos == "out") add_top <- add_top + .06
  # a 2-D table is an instance of a matrix, a 1-D table is not
  max.y <- ifelse (is.matrix(x) && !beside, max(colSums(x)), max(x))
  max.y <- max.y + (add_top * max.y)

  if (any(x < 0, na.rm = TRUE)) {
    min.y <- ifelse (is.matrix(x) && !beside, min(colSums(x)), min(x))
    min.y <- min.y - abs(add_top * min.y)
  }
  else
    min.y <- 0

  if (is.null(legend_labels)) legend_labels <- row.names(x)
  for (i in 1:length(legend_labels))
    legend_labels[i] <- gsub("~", " ", legend_labels[i], fixed=TRUE)
  if (beside) legend_horiz <- FALSE
  if (is.matrix(x) && !beside) legend_horiz <- TRUE

  if (is.null(gap)) {  # ifelse does not work here when gap is a vector
    if (!is.null(by) && beside)
      gap <- c(0.1,1)
    else
      gap <- 0.2
  }

  # get the names
  the.names <- integer(length=0)
  if (length(dim(x)) == 0)
    the.names <- names(x)
  else {
    if (is.null(by))  # ifelse does not work
      the.names <- rownames(x)
    else
      the.names <- colnames(x)
  }

  # set las.value for labels horiz or vertical
  las.value <- 1
  if (horiz  &&  max(nchar(the.names), na.rm=TRUE) > 5) las.value <- 0


  # ------------
  # value labels

  # set val.lab as the working value_labels vector
  if (!is.null(value_labels)) {
    val.lab <- value_labels
  }
  else {
    if (is.null(by))
      val.lab <- names(x)
    else
      val.lab <- colnames(x)
    if (length(val.lab) == 0) val.lab <- colnames(x)  # read matrix directly
    if (!is.null(names(y))) val.lab <- names(y)
  }

  # for each value label, partition into mx.x.val.ln lines if (break_x)
  mx.x.val.ln <- 1
  if (!break_x) {
    for (i in seq_along(val.lab)) {
      if (!is.na(val.lab[i])) {
        val.lab[i] <- gsub(" ", "~", val.lab[i])  # ~ , so no \n
      }
    }
  }
  stuff <- .get.val.ln(val.lab, x.name)
  val.lab <- stuff$val.lab
  mx.x.val.ln <- stuff$mx.val.ln

  mx.y.val.ln <- 1

  if (is.null(y)) if (horiz) {  # switch
    temp <- x.lab;  x.lab <- y.lab;  y.lab <- temp
    temp <- mx.x.val.ln;  mx.x.val.ln <- mx.y.val.ln;  mx.y.val.ln <- temp
  }

  # set max.val.width to get max width of y-axis labels for lm adjustment
  lblval.y <- character(length=0)
  if (is.null(scale_y)) {
    prety <- max(pretty(c(min.y, max.y)))
    mx.num <-  ifelse (!prop, as.character(prety), .fmt(prety, 2))
    max.y.width <- max(strwidth(mx.num, cex=axis_y_cex, units="inches"))

  }
  else {  # need scale to be defined to get y.coords, not done till later
    ax.num <- ifelse(horiz, 1, 2)  # location of numerical axis
    y.coords <- axTicks(ax.num, axp=scale_y)
    nd <- 0
    for (i in 1:length(y.coords))
      if (.num.dec(y.coords[i]) > nd) nd <- .num.dec(y.coords[i])
    if (nd > 2) nd <- 2  # only allow a few decimal digits
    for (i in 1: length(y.coords))
      lblval.y[i] <- as.character(.fmt(y.coords[i], nd))
    j <- which(nchar(lblval.y) == max(nchar(lblval.y)))[1]
    max.y.width <- max(strwidth(lblval.y[j], cex=axis_y_cex, units="inches"))
  }

  max.x.width <- NULL
  if (horiz  ||  rotate_x == 90) {  # "y"-axis is categorical (i.e., x-axis)
  val.split <- unlist(strsplit(val.lab, "\n"))  # break into separate words
    if (horiz)
      max.y.width <- max(strwidth(val.split, cex=axis_x_cex, units="inches"))
    else
      max.x.width <- max(strwidth(val.split, cex=axis_x_cex, units="inches"))
    # strwidth not work in R w/o RStudio until barplot, plot.new not enough
    in.RStudio <- ifelse (options("device") == "RStudioGD", TRUE, FALSE)
    if (!in.RStudio) {  # 1st attempt, undoubtedly can be improved
      if (horiz)
        max.y.width <- max(nchar(val.split)) / (11 / axis_x_cex)
      else
        max.x.width <- max(nchar(val.split)) / (11 / axis_x_cex)
    }
  }

  if (do_plot) {

  # ----------------
  # set up plot area

  margs <- .plt.marg(max.y.width, y.lab, x.lab, main.lab, sub.lab,
                rotate_x, mx.x.val.ln, mx.y.val.ln,
                lab_x_cex=lab_x_cex, lab_y_cex=lab_y_cex, max.x.width)
  lm <- margs$lm
  tm <- margs$tm
  rm <- margs$rm
  bm <- margs$bm
  n.lab_x.ln <- margs$n.lab_x.ln
  n.lab_y.ln <- margs$n.lab_y.ln

  if (horiz)
    rm <- rm + 0.1  # sometimes the max axis value goes off the plot
  else
    if (offset > 0.5) bm <- bm + (-0.05 + 0.2 * offset)  # offset kludge

  if (!exists("byname"))
    byname <- getOption("byname")
  if (legend_position == "right_margin"  &&  (is.matrix(x))) {
    if (!is.null(legend_abbrev)) {
      byname  <- abbreviate(byname, legend_abbrev)
      if (!is.null(legend_labels))
        legend_labels <- abbreviate(legend_labels, legend_abbrev)
    }
    mx.ch <- max(c(max(nchar(legend_labels)), nchar(byname)-2))
    exp.coef <- 0.065 + 0.45 * axis_x_cex
    rm <- rm + (.34 + (exp.coef * axis_x_cex))
    rm <- rm + (0.04 + 0.036*mx.ch)  # legend also moves over
    if (mx.ch > 18) rm <- rm + ((mx.ch - 18) * .06)
  }

  if (legend_position == "top")
    tm <- tm + (.25 + (0.446 * lab_cex))  # tm <- tm + .45

  # user manual adjustment
  bm <- bm + bm.adj
  lm <- lm + lm.adj
  tm <- tm + tm.adj
  rm <- rm + rm.adj

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  par(bg=getOption("window_fill"))
  par(mai=c(bm, lm, tm, rm))

#p(par("din")[1]-par("pin")[1])
  # new_scale to control bar width for small number of bars
  # set new_scale
  if ("numeric" %in% class(x)  &&  entered) x <- as.table(x)
  new_scale <- 0
  if (is.null(by)) if (nrow(x) <= 4) new_scale <- nrow(x)
  if (is.matrix(x)  &&  !beside) if (ncol(x) <= 4) new_scale <- ncol(x)
  if ("matrix" %in% class(x)  &&  entered) new_scale <- 0  # turned off for now
  # set width.bars, gap
  if (new_scale == 4) width.bars <- .17
  if (new_scale == 3) width.bars <- .22
  if (new_scale == 2) width.bars <- .28
  if (new_scale == 1) width.bars <- .30  # for only one category
  if (new_scale > 0) gap <- 0.246 + (0.687 * width.bars)

  yp <- pretty(c(min.y, max.y))
  y.adj_min <- pad_y_min * (yp[length(yp)] - yp[1])
  y.adj_max <- pad_y_max * (yp[length(yp)] - yp[1])
  min.y <- min.y - y.adj_min
  max.y <- max.y + y.adj_max

  # barplot run here only to establish usr coordinates, axTick values
  #  otherwise usr is just 0,1 for both axes
  # the barplot itself is not retained
  if (new_scale == 0) {
    if (!horiz)
      barplot(x, col="transparent", border="transparent",
        ylim=c(min.y,max.y), axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, ...)
    else
      barplot(x, col="transparent", border="transparent", horiz=TRUE,
        axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, xlim=c(min.y, max.y), ...)
  }
  else { # new_scale, need (0,1) limit on_cat axis for re-scale to work
    if (!horiz)
      barplot(x, col="transparent", border="transparent",
        ylim=c(min.y,max.y), axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, xlim=c(0,1),
        axes=FALSE, ...)
    else {
      # when x is binary for a BPFM equivalent ylim may need extension
      x.coords <- barplot(x, col="transparent", border="transparent", horiz=TRUE,
        axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, xlim=c(min.y, max.y),
        ylim=c(0,1), axes=FALSE, plot=FALSE, ...)
      up.lim <- ifelse (max(x.coords) > 1, max(x.coords) + .1, 1)
      barplot(x, col="transparent", border="transparent", horiz=TRUE,
        axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, xlim=c(min.y, max.y),
        ylim=c(0,up.lim), axes=FALSE, ...)
    }
  }

  ax.num <- ifelse (horiz, 1, 2)  # location of numerical axis
  y.coords <- axTicks(ax.num, axp=scale_y)


  ## ----
  ## PLOT

   usr <- par("usr")  # used elsewhere as well
  .plt.bck(usr, y.coords, y.coords, do.v=horiz, do.h=!horiz)

  # the bars
  if (new_scale == 0)
    x.coords <- barplot(x, add=TRUE, col=fill, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=color, las=las.value,
          space=gap, axisnames=FALSE, ...)
  else
    x.coords <- barplot(x, add=TRUE, col=fill, beside=beside, horiz=horiz,
          axes=FALSE, ann=FALSE, border=color, las=las.value,
          space=gap, width=width.bars, xlim=c(0,1), axisnames=FALSE, ...)

  # display text labels of values on or above the bars
  # --------------------------------------------------

  # need n.levels for this evaluation
  if (values == "eval.later") {  # values not user-specified
    is.int <- TRUE
    if (!is.null(y)) if (!.is.integer(y)) is.int <- FALSE
    if (n.levels > 14  || !is.int)
      values <- "off"
    else {
      values <- getOption("values")
      if (values != "off") if (y.given) values <- "input"
    }
  }

  if (beside) {
     values_size <- .75 * values_size
     values_cut <- 0.01
  }

    if (values != "off") {
      if (is.null(values_cut)) {
        values_cut <- 0.028
        if ((prop && is.matrix(x)) || multi) values_cut <- 0.040
      }

      if (is.null(values_digits)) {  # if too large for "input", get in bc.main
        if (values == "%") values_digits <- 0
        else if (values == "proportion") values_digits <- 2
        else if (values == "input") values_digits <- 0
      }

      # set type of the values to display, x.txt

      if (!prop) {
        if (!multi)
          x.prop <- x/sum(x)
        else
          x.prop <- x/colSums(x)
        if (values == "input")
          x.txt <- .fmt(x, values_digits)   # as.char not accurate for dec dig
        else if (values == "%")
          x.txt <- paste(.fmt(x.prop * 100, values_digits), "%", sep="")
        else if (values == "proportion")
          x.txt <- .fmt(x.prop, values_digits)

        if (is.matrix(x))
            x.txt <- matrix(x.txt, nrow=nrow(x))

        if (values_pos != "out") {
          if (is.null(by)) {
            if (!y.given) {
              for (i in 1:length(x.prop))
                x.txt[i] <- ifelse (x.prop[i] >= values_cut, x.txt[i], "")
            }
          }
          else {  # by variable
            for (i in 1:nrow(x.prop)) for (j in 1:ncol(x.prop))
              x.txt[i,j] <- ifelse (x.prop[i,j] >= values_cut, x.txt[i,j], "")
          }
        }
      }

      else {  # prop
        if (values == "input")
          x.txt <- as.character(x.count)
        else if (values == "%")
          x.txt <- paste(.fmt(x * 100, values_digits), "%", sep="")
        else if (values == "prop")
          x.txt <- .fmt(x, values_digits)

        if (is.matrix(x))
            x.txt <- matrix(x.txt, nrow=nrow(x))

        if (values_pos != "out") {
          if (is.null(by)) {
            for (i in 1:length(x))
              x.txt[i] <- ifelse (x[i] >= values_cut, x.txt[i], "")
          }
          else {  # by variable
            # as.numeric & as.character convert table to vector: re-convert
            for (i in 1:nrow(x)) for (j in 1:ncol(x))
              x.txt[i,j] <- ifelse (x[i,j] >= values_cut, x.txt[i,j], "")
          }
        }
      }

      # R text function sets cex at 1 if input value is 0
      if (values_size == 0) values_size <- 0.01

      # vertical bars
      if (!horiz) {
        if (!is.matrix(x)) { # 1 variable
          usr.y.inch <- diff(grconvertY(0:1, 'inches', 'user'))
          if (values_pos == "in")
            ycrd <- x/2
          else {
            slope <- 0.0 + (0.15*values_size)
            ycrd <- ifelse(x > 0, x + slope*usr.y.inch, x - slope*usr.y.inch)
          }
          text(x.coords, ycrd, labels=x.txt, col=values_color, cex=values_size)
        }  # no by
        else {  # 2 variables
          if (!beside) {
            for (i in 1:ncol(x)) {
              ycrd <- cumsum(x[,i]) - (x[,i] / 2)
              text(x.coords[i], ycrd, labels=x.txt[,i],
                   col=values_color, cex=values_size)
            }
          }  # end !beside
          else {  # beside
            usr.y.inch <- diff(grconvertY(0:1, 'inches', 'user'))
            if (values_pos == "in")
              ycrd <- x/2
            else  # value.pos == "out"
              ycrd <- x + 0.10*usr.y.inch
            for (i in 1:ncol(x)) {
              text(x.coords[,i], ycrd[,i], labels=x.txt[,i],
                   col=values_color, cex=values_size)
            }
          }  # end beside
        }  # end 2 variables
      }  # end vertical bars

      # horiz bars
      else {
        if (!is.matrix(x)) {  # 1 variable
          usr.x.inch <- diff(grconvertX(0:1, 'inches', 'user'))
          if (values_pos == "in")
            ycrd <- x/2
          else { # out, adjust for label font size
            slope <- 0.024 + (0.192*values_size)
            ycrd <- ifelse(x > 0, x + slope*usr.x.inch, x - slope*usr.x.inch)
          }
          text(ycrd, x.coords, labels=x.txt, col=values_color, cex=values_size)
        }  # end no by
        else {  # by variable
          if (!beside) {  # stacked chart
          for (i in 1:ncol(x)) {  # each level of by
            ycrd <- cumsum(x[,i]) - (x[,i] / 2)
            text(ycrd, x.coords[i], labels=x.txt[,i],
               col=values_color, cex=values_size)
          }
        }
        else {  # beside chart
          usr.x.inch <- diff(grconvertX(0:1, 'inches', 'user'))
          if (values_pos == "in")
            ycrd <- x/2
          else  # value.pos == "out"
            ycrd <- x + 0.17*usr.x.inch
            for (i in 1:ncol(x)) {
              text(ycrd[,i], x.coords[,i], labels=x.txt[,i],
                   col=values_color, cex=values_size)
            }
          }  # end beside
        }  # end by
      }  # end horiz bars

    }  # end display values


    # y-axis is the numerical axis
    if (!horiz) las.value <- 1
    axis_y_color <- ifelse(is.null(getOption("axis_y_color")),
      getOption("axis_color"), getOption("axis_y_color"))
    axis_y_text_color <- ifelse(is.null(getOption("axis_y_text_color")),
      getOption("axis_text_color"), getOption("axis_y_text_color"))
    adj1 <- ifelse (!horiz, 0.5, -1.0)
    if (is.null(scale_y)) {  # if scale_y defined, can evaluate earlier
      if (!prop)
        lblval.y <- as.character(y.coords)
      else
        lblval.y <- .fmt(y.coords,2)
    }
    axis(ax.num, col=axis_y_color,   # maybe split off the text like with x axis?
         col.axis=axis_y_text_color, cex.axis=axis_y_cex, las=las.value,
         tck=-.02, padj=adj1,  at=y.coords, labels=lblval.y, ...)
         #tck=-.03, padj=adj1,  at=axTicks(ax.num, axp=scale_y), ...)

    # x-axis is the category value axis
    axis_x_color <- ifelse(is.null(getOption("axis_x_color")),
      getOption("axis_color"), getOption("axis_x_color"))
    axis_x_text_color <- ifelse(is.null(getOption("axis_x_text_color")),
      getOption("axis_text_color"), getOption("axis_x_text_color"))

    if (beside) x.coords <- apply(x.coords, 2, mean)  # one label per group
    if (!horiz) {
      usr.y.in <- diff(grconvertY(0:1, 'inches', 'user'))  # y user x.coords inch
      ax.value <- 1;  xx <- x.coords;  yy <- par("usr")[3] - (.15 * usr.y.in)
    }
    else {
      usr.x.in <- diff(grconvertX(0:1, 'inches', 'user'))  # x user x.coords inch
      ax.value <- 2;  xx <- par("usr")[1] - (.15 * usr.x.in);  yy <- x.coords
    }

    adj.x <- ifelse(!horiz, 0.5, 1.0)
    adj.y <- ifelse(!horiz, 1.0, 0.5)
    if (rotate_x == 0) {
      axis(ax.value, at=x.coords, labels=FALSE, tck=-.02, col=axis_x_color, ...)
      text(x=xx, y=yy, labels=val.lab, adj=c(adj.x,adj.y),
           xpd=TRUE, cex=axis_x_cex, col=axis_x_text_color, ...)
    }
    else if (rotate_x > 0  && rotate_x < 90) {
      axis(ax.value, at=x.coords, labels=FALSE, tck=-.02, col=axis_x_color, ...)
      text(x=xx, y=yy, labels=val.lab, pos=1,  # pos needed for offset
           xpd=TRUE, cex=axis_x_cex, col=axis_x_text_color,
           srt=rotate_x, offset=offset, ...)
    }
    else if (rotate_x == 90)  # 90 degrees rotate
      axis(ax.value, at=x.coords, labels=val.lab, tck=-.02, col=axis_x_color,
           cex.axis=axis_x_cex, las=2, ...)

    # title
    title(main=main.lab, cex.main=getOption("main_cex"),
          col.main=getOption("main_color"))

    lab_x_color <- ifelse(is.null(getOption("lab_x_color")),
      getOption("lab_color"), getOption("lab_x_color"))

    # xlab positioning
    ln.ht.x <- par('cin')[2] * lab_x_cex * par('lheight')  # line ht inches
    xlab_adj <- xlab_adj / ln.ht.x
    lblx.lns <- par("mar")[1] - 1.3   # par("mar")[1] is bm in lines
    title(xlab=x.lab, line=lblx.lns-xlab_adj, cex.lab=lab_x_cex,
          col.lab=lab_x_color)

    # need sub.lab processing here

    # ylab positioning (based on .axlabs function)
    lab_y_color <- ifelse(is.null(getOption("lab_y_color")),
    getOption("lab_color"), getOption("lab_y_color"))
    ln.ht.y <- par('cin')[2] * lab_y_cex * par('lheight')  # line ht inches
    ylab_adj <- ylab_adj / ln.ht.y
    lm <- par("mar")[2]  # get current left margin in lines
    lbly.lns <- lm - (0.3 + 0.9*n.lab_y.ln)
    title(ylab=y.lab, line=lbly.lns-ylab_adj,
          cex.lab=lab_y_cex, col.lab=lab_y_color)


    # ------------------------------------------------------
    # legend for two variable plot including variable labels

    if ( (!is.null(by) || is.matrix(x)) && !is.null(legend_position)) {

      col.bg <- getOption("panel_fill")
      col.txt <- ifelse (sum(col2rgb(col.bg))/3 > 80, "black", rgb(.97,.97,.97))

      # evaluate in bc.main, under color, when n.levels is known
      if (is.null(legend_size)) {
        if (legend_position == "top")
          legend_size <- getOption("lab_cex")
        else
          legend_size <- axis_x_cex
      }

      # default right_margin option location
      if (legend_position == "right_margin") {

        options(byname = getOption("byname"))
        trans_pts <- .6  # dummy value
        point.size <- 2.5 * axis_x_cex
        .plt.by.legend(legend_labels, color, fill, shp=22, trans_pts,
                       col.bg, usr, pt.size=point.size, pt.lwd=0,
                       legend_size, legend_abbrev, legend_adj, legend_title)

      }  # end right margin

      # top option
      else if (legend_position == "top") {
        ll <- legend("top", legend=legend_labels, plot=FALSE,
               title=l.lab, xpd=NA, x.intersp=.15,
               fill=fill, border="transparent",
               horiz=legend_horiz, cex=legend_size, bty="n", text.col=col.txt)
        fct <- (-0.043 + (0.257 * lab_cex))  # more cushion for large cex
        legend(ll$rect$left, usr[4]+(fct*(usr[4])), legend=legend_labels,
               title=l.lab, xpd=NA, x.intersp=.15, pt.cex=10,
               fill=fill, border="transparent",
               horiz=legend_horiz, cex=legend_size, bty="n", text.col=col.txt)
      }

      # everything else, not so consistent
      else
        legend(legend_position, legend=legend_labels, title=l.lab,
               fill=fill, border="transparent", x.intersp=.15,
               horiz=legend_horiz, cex=legend_size, bty="n", text.col=col.txt)

    } # end legend

    if (!is.null(add)) {

      add_cex <- getOption("add_cex")
      add_lwd <- getOption("add_lwd")
      add_lty <- getOption("add_lty")
      add_color <- getOption("add_color")
      add_fill <- getOption("add_fill")
      add_trans <- getOption("add_trans")

      .plt.add (add, x1, x2, y1, y2,
                add_cex, add_lwd, add_lty, add_color, add_fill, add_trans)
    }

  }  # end do_plot

  # -----------------------------------------------------------------------
  # -----------------------------------------------------------------------
  # text output

  if (prop) {
    if (is.null(y)) {
      if (!is.null(by) || is.matrix(x))
        x  <- x.temp
      else
        x <- table(x.temp)
    }
  }

  dd <- ifelse (is.null(digits_d), 3, digits_d)
  n_dim <- length(dim(x))
  stats <- ""

  if (multi) {

    txsug <- ""

    # display variable labels
    txlbl <- ""
    l.name <- "l"
    if (exists(l.name, where=.GlobalEnv)) {
      mylabs <- get(l.name, pos=.GlobalEnv)
      mylabs <- mylabs[colnames(x),]
      if (!is.null(mylabs)) {
        tx <- character(length = 0)
        for (i in 1:length(colnames(x))) {
          if (is.data.frame(mylabs))
            ml <- mylabs[i,]
          else  # if a different l label frame exists, ml is NA
            ml <- mylabs[i]
          if (!is.na(ml))
            tx[length(tx)+1] <- paste(colnames(x)[i], ": ", ml, sep="")
        }
        tx[length(tx)+1] <- " "
        tx[length(tx)+1] <- "Variable Labels"
        txlbl <- rev(tx)
        blnk <- " "
      }
    }

    # display frequencies and means of each variable
    txttl <- "Frequencies of Responses by Variable"
    tx <- character(length = 0)
    mx.chr <- max(nchar(rownames(x)))
    mx.len <- 8
    if (mx.chr > mx.len) {
      c.nm <- rownames(x)
      rownames(x) <- .abbrev(rownames(x), mx.len)
    }
    x <- t(x)

    txtbl <- .prntbl(x, 0, cc=NULL)
    mx <- max(nchar(round(wm,2)))
    txtbl[1] <- paste(txtbl[1], " ", .fmtc("Mean", w=mx+1))
    n.var <- nrow(x)
    for (i in 1:n.var)
      txtbl[i+1] <- paste(txtbl[i+1], " ", .fmt(wm[i],3,w=mx+1))
     txtbl[2:length(txtbl)] <- rev(txtbl[2:length(txtbl)])

    if (mx.chr > mx.len) {
      txtbl[n.var+2] <- ""
      txtbl[n.var+3] <- ""
      txtbl[n.var+4] <- "Unabbreviated labels"
      txtbl[n.var+5] <- "--------------------"
      txtbl[n.var+6] <- paste(c.nm, sep="", collapse="\n")
    }
    strt <- ifelse (mx.chr > mx.len, 7, 2)
    if (is.null(resp)) {
      txtbl[n.var+strt] <- ""
      txtbl[n.var+strt+1] <- paste("Computation of the mean based on coding",
                                   "response categories from 1 to", n.var)
    }

    class(txsug) <- "out"
    class(txtbl) <- "out"
    class(txttl) <- "out"

    if (nzchar(txsug))
      output <- list(out_suggest=txsug,
                     out_text=txlbl, out_title=txttl, out_text=txtbl)
    else
      output <- list(out_text=txlbl,
                     out_title=txttl, out_text=txtbl)

    class(output) <- "out_all"
    if (!quiet) print(output)

  }  # end multi


  # x is a table, could be real values or integers <0 if y is specified
  # no by variable, dim == 0 if x<-x.temp
  else if (n_dim == 1) {
    if (.is.integer(x)  &&  all(x >= 0)) {  # x is table of count-like values

      txsug <- ""
      if (getOption("suggest")) {
        txsug <- ">>> Suggestions"
        fc <- paste("BarChart(", x.name,
                    ", horiz=TRUE)  # horizontal bar chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("BarChart(", x.name,
                 ", fill=\"reds\")  # red bars of varying lightness", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("PieChart(", x.name, ")  # doughnut (ring) chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("Plot(", x.name, ")  # bubble plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("Plot(", x.name,
                    ", stat=\"count\")  # lollipop plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
      }

      stats <- .ss.factor(x, by=NULL, brief=TRUE, digits_d=dd,
                          x.name, by.name, x.lbl, y.lbl, label_max,
                          x.miss, by.miss, out_size)
      if (!is.null(stats)) {
        txttl <- stats$title
        counts <- stats$count
        miss <- stats$miss
        chi <- stats$chi
        lbl <- stats$lbl

        class(txsug) <- "out"
        class(txttl) <- "out"
        class(counts) <- "out"
        class(miss) <- "out"
        class(chi) <- "out"
        class(lbl) <- "out"
        output <- list(out_suggest=txsug, out_title=txttl, out_miss=miss,
                       out_lbl=lbl, out_counts=counts, out_chi=chi)
        class(output) <- "out_all"
        if (!quiet) print(output)
      }

    # names and order of components per documentation in BarChart.Rd
      stats$n_miss <- x.miss
      stats$p_value <- .fmt(stats$p_value, 3)
      names(stats) <- c("n_dim", "out_title", "out_counts", "out_miss",
                        "out_chi", "out_lbl", "freq", "freq_df", "prop",
                        "p_value", "n_miss")
      stats <- c(stats[2], stats[6], stats[3], stats[5], stats[4],
                 stats[1], stats[10], stats[8], stats[7], stats[9], stats[11])
    }  # end counts or count-like

    else {  # x (table from y) not count-like: not integer or values < 0

      txsug <- ""
      if (getOption("suggest")) {
        y.name <- getOption("yname")
        txsug <- ">>> Suggestions"
        fc <- paste("Plot(", y.name, ", ", x.name, ") # lollipop plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
      }

      stats <- .ss.real(x, by, digits_d=dd,
                   x.name, getOption("yname"), by.name, x.lbl, y.lbl, label_max)
      txtbl <- stats$txtbl
      class(txsug) <- "out"
      class(txtbl) <- "out"
      output <- list(out_suggest=txsug, out_txt=txtbl, values=stats$values)

      class(output) <- "out_all"
      if (!quiet) print(output)
      names(stats) <- c("n_dim", "out_y", "values")
      stats <- c(stats[2], stats[1], stats[3])
    }

  }  # end if (n_dim == 1)

  # -------------
  # a by variable
  else {

    txsug <- ""
    if (getOption("suggest")) {
      txsug <- ">>> Suggestions"
      fc <- paste("Plot(", x.name, ", ", by.name, ")  # bubble plot", sep="")
      txsug <- paste(txsug, "\n", fc, sep="")
      fc <- paste("BarChart(", x.name, ", by=", by.name,
                  ", horiz=TRUE)  # horizontal bar chart", sep="")
      txsug <- paste(txsug, "\n", fc, sep="")
      fc <- paste("BarChart(", x.name,
                  ", fill=\"steelblue\")  # steelblue bars", sep="")
      txsug <- paste(txsug, "\n", fc, sep="")
    }

    if (is.null(y) && .is.integer(x)) {

      # need brief=FALSE for row proportions
      stats <- .ss.factor(x, by, brief=FALSE, digits_d=dd,
                          x.name, by.name, x.lbl, y.lbl, label_max)
      txttl <- stats$txttl
      txfrq <- stats$txfrq
      txXV <- stats$txXV
      txlbl <- stats$txlbl

      class(txsug) <- "out"
      class(txttl) <- "out"
      class(txfrq) <- "out"
      class(txXV) <- "out"
      class(txlbl) <- "out"
      if (!prop)
        output <- list(out_suggest=txsug, out_title=txttl, out_lbl=txlbl,
                       out_text=txfrq, out_XV=txXV)
      else {
        txrow <- stats$txrow
        class(txrow) <- "out"
        output <- list(out_suggest=txsug, out_title=txttl, out_text=txfrq,
                       out_XV=txXV, out_row=txrow)
      }
      class(output) <- "out_all"
      if (!quiet) print(output)

      names(stats) <- c("n_dim", "out_title", "out_lbl", "out_counts",
                        "out_chi", "out_prop", "out_row",
                        "out_col", "freq", "p_value")
      stats <- c(stats[2], stats[3], stats[4], stats[5], stats[6],
                 stats[7], stats[8], stats[1], stats[9], stats[10])
    }  # end is.null(y)

    else {  # y is present
      stats <- .ss.real(x, y, by, digits_d=dd, x.name,
                        getOption("yname"), by.name, x.lbl, y.lbl, label_max)
      txtbl <- stats$txtbl
      class(txtbl) <- "out"
      output <- list(out_txt=txtbl)

      class(output) <- "out_all"
      if (!quiet) print(output)

      names(stats) <- c("n_dim", "out_y")
      stats <- c(stats[2], stats[1])
    }

  }  # end a by variable

  cat("\n")
  return(stats)

}

