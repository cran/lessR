.plt.txt <-
function(x, y, stat, object, cat.x, cat.y, date.var,
       xlab, ylab, fit, n.by, mse.ln, mse.nl, b0, b1, Rsq, 
       fit_new, y.new, by.cat,
       center_line, run, show_runs, prop, size, radius, digits_d,
       fun_call=NULL) {

  if (n.by == 0) n.by <- 1
  if (date.var) center_line <- "off"

  # x and y come across here in their natural state, within each data frame
  # a time series has dates for x and numeric for y, factors are factors, etc

  bubble1 <- ifelse (length(unique(y[,1])) == 1, TRUE, FALSE)  # 1-D bubble plot

  unique.x <- ifelse (length(unique(x[,1])) == length(x[,1]), TRUE, FALSE)
  unique.y <- ifelse (length(unique(y[,1])) == length(y[,1]), TRUE, FALSE)

  # all processing in terms of numeric variables
  # convert factors to numeric, save levels, so x and y are always numeric
  # x will always be a matrix
  x.lvl <- NULL; y.lvl <- NULL  # if remain null, then not factors

  # process x
  nm.x <- names(x)
  if (is.factor(x[,1])) {
    x.lvl <- levels(x[,1])
    x <- as.matrix(as.integer(x[,1]))
  }
  else if (!date.var) {
    x <- as.matrix(x)
    colnames(x) <- nm.x
  }

  # process y
  nm.y <- names(y)
  if (is.factor(y[,1])) {
    y.lvl <- levels(y[,1])
    y <- as.matrix(as.integer(y[,1]))

  }
  else if (!date.var) {
    y <- as.matrix(y)
    colnames(y) <- nm.y
  }

  # dimensions
  n.xcol <- ncol(x)
  n.ycol <- ncol(y)
  n_col <- max(n.xcol, n.ycol)
  nrows <- nrow(x)

  if (date.var) {
    x.val <- x[,1]
    x <- as.matrix(x.val, ncol=1)
  }

  if (is.null(x.lvl) && !is.null(y.lvl) && unique.y ||
      is.null(y.lvl) && !is.null(x.lvl) && unique.x) {
    cleveland <- TRUE
  }
  else
    cleveland <- FALSE

  gl <- .getlabels(xlab, ylab)  # this redoes if already a plot
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
  #by.name <- getOption("byname")

  # decimal digits
  if (is.null(digits_d)) {  # cat vars are only integers
    digits_d <- .max.dd(y[,1]) + 1
    if (!cat.x && cat.y) digits_d <- .max.dd(x[,1]) + 1 
  }
  if (digits_d < 3) digits_d <- 3
  options(digits_d=digits_d)

  size.pt <- ifelse (is.null(size), 1, size)  # dummy non-zero value

  if (n_col > 1) center_line <- "off"   # no center_line for multiple plots

  m.y <- mean(y[,1], na.rm=TRUE)

  if (center_line == "mean") {
    lbl <- " mean"
    lbl.cat <- "mean:"
  }
  else if (center_line == "median"  ||  center_line == "off") {
    m.y <- median(y[,1], na.rm=TRUE)
    lbl <- " medn"
    lbl.cat <- "median:"
  }
  else if (center_line == "zero") {
    m.y <- 0
    lbl <- ""
    lbl.cat <- "zero:"
  }


  # -----------
  # text output
  # -----------

  if (getOption("suggest")) {
    # function call, with last ) removed for suggestions
    fncl <- .fun_call.deparse(fun_call)  # class call to class character
    fncl <- gsub(")$", "", fncl)  # get function call less closing )
    fncl <- gsub(" = ", "=", fncl)
  }

  if (stat == "data") {
      txsug <- ""

    if (date.var) {
      if (getOption("suggest")) {
        txsug <- "\n>>> Suggestions  or  enter: style(suggest=FALSE)"

        if (x.name != y.name)
          fc <- paste("Plot(", x.name, ", ", y.name, sep="")
        else
          fc <- paste("Plot(", x.name, sep="")

        if (!grepl("ts_ahead", fncl)) {
          txt <- ", ts_ahead=4)"
          cmnt <- "  # exponential smoothing forecast 4 time units"
          txsug <- paste(txsug, "\n", fc, txt, cmnt, sep="")
        }

        if (!grepl("ts_unit", fncl)) {
          txt <- ", ts_unit=\"years\")  # aggregate time by yearly sum"
          txsug <- paste(txsug, "\n", fc, txt, sep="")
        }

        if (!grepl("ts_agg", fncl)) {
          txt <- ", ts_unit=\"years\", ts_agg=\"mean\")"
          cmnt <- "  # aggregate by yearly mean"
          txsug <- paste(txsug, "\n", fc, txt, cmnt, sep="")
        }

        if (grepl("ts_ahead", fncl)  &&  !grepl("ts_seasons", fncl)) {
          txt <- ", ts_ahead=4, ts_seasons=FALSE)"
          cmnt <- "  # turn off exponential smoothing seasonal effect"
          txsug <- paste(txsug, "\n", fc, txt, cmnt, sep="")
        }
      }
    }  # end date.var


    if (object == "bubble") {
      if (getOption("suggest")) {
        txsug <- "\n>>> Suggestions  or  enter: style(suggest=FALSE)"
        fc <- ""
        smaller <- as.character(.fmt(radius / 1.5, 2))
        larger <- as.character(.fmt(radius * 1.5, 2))
        if (!grepl("bubble", fncl)) {
          if (!is.null(radius)) {
            if (radius >= 0.22) {
              fc <- paste(fc, ", radius=", smaller, sep="")
              txt <- "# smaller bubbles"
            }
            else {
              fc <- paste(fc, ", radius=", larger, sep="")
              txt <- "# larger bubbles"
            }
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              fc <- paste(fc, txt, sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
#               class(txsug) <- "out"
#               return(list(out_suggest=txsug))
            }
          }
        }
      }  # if suggest
    }  # end bubble

    if (object == "both") {  # line, run chart (object is "both")

      if (getOption("suggest")) {
        if (txsug == "")
          txsug <- "\n>>> Suggestions  or  enter: style(suggest=FALSE)"

        fc <- ""
        if (!grepl("size", fncl)  &&  size.pt > 0)
          fc <- paste(fc, ", size=0", sep="")
        if (nzchar(fc)) {
          fc <- gsub(" = ", "=", fc)
          fc <- paste(fncl, fc, ")   # just line segments, no points", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }

        fc <- ""
        if (!grepl("lwd", fncl))
          fc <- paste(fc, ", lwd=0", sep="")
        if (nzchar(fc)) {
          fc <- gsub(" = ", "=", fc)
          if (size.pt > 0)
            txt <- "just points, no line segments"
          else {
            fc <- paste(fc, ", fill=\"on\"", sep="")
            txt <- "just area"
          }
          fc <- paste(fncl, fc, ")   # ", txt, sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }

        fc <- ""
        if (!grepl("area_fill", fncl)  &&  (!grepl("stack", fncl)))
          fc <- paste(fc, ", area_fill=\"on\"", sep="")
        if (nzchar(fc)) {
          fc <- gsub(" = ", "=", fc)
          fc <- paste(fncl, fc, ")   # default color fill", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }

        txsug <- .rm.arg.2(" x=", txsug)
        txsug <- .rm.arg.2("(x=", txsug)
        txsug <- .rm.arg.2(" y=", txsug)
#for(i in 1:length(txsug)) cat(txsug[i], "\n")  # even here blank lines at end

        class(txsug) <- "out"
        output <- list(out_suggest=txsug)
        class(output) <- "out_all"
        print(output)
      }  # end getOption suggest

      # analyze runs if a singly y
      if (run && n.ycol==1) {

        txss <- ""
        ssstuff <- .ss.numeric(y, digits_d=digits_d, x.name="*NONE*",
                               brief=TRUE)
        txss <- ssstuff$tx
        class(txss) <- "out"
        output <- list(out_ss=txss)
        class(output) <- "out_all"
        print(output)

        .dash(12); cat("Run Analysis\n"); .dash(12)
        run <- integer(length=0)  # length of ith run in run[i]
        n.runs <- 1  # total number of runs
        run[n.runs] <- 1
        line.out <- "    1"
        for (i in 2:length(y)) {  # find the runs
          if (y[i] != m.y) {  # throw out values that equal m.y
            if (sign(y[i]-m.y) != sign(y[i-1]-m.y)) {  # new run
              if (show_runs) {
                if (i == 2) cat("\n")
                buf <- ifelse (n.runs < 10,  "  ", " ")
                  if (run[n.runs] > 1)  # print only if run of size 2 or more
                    cat("size=", run[n.runs], "  Run", buf, n.runs, ":",
                      line.out, "\n", sep="")
              }
              line.out <- ""
              n.runs <- n.runs + 1
              run[n.runs] <- 0
            }
          }
          run[n.runs] <- run[n.runs] + 1
          buf <- ifelse (i < 10, "  ", " ")
          line.out <- paste(line.out, buf, i)
        }  # end find the runs
        if (run[n.runs] > 1)  # print only if run has at least 2 elements
          if (show_runs)
            cat("size=", run[n.runs], "  Run", buf, n.runs, ":", line.out,
                "\n", sep="")
        eq.ctr <- which(y==m.y)
        cat("\nTotal number of runs:", n.runs, "\n")
        txt <- "Total number of values that do not equal the "
        cat(txt, lbl.cat, " ", length(y)-length(eq.ctr), "\n", sep="")
        if (length(eq.ctr) != 0) {
          if (show_runs) {
            cat("\nValues ignored that equal the", lbl.cat, "\n")
            for (i in 1:length(eq.ctr))
              cat("    #", eq.ctr[i], " ", y[eq.ctr[i]], sep="", "\n")
            cat("Total number of values ignored:", length(eq.ctr), "\n")
          }
        }
        else {
          cat("Total number of values ignored that equal the", lbl.cat,
              length(eq.ctr), "\n")
        }

      }  # end analyze runs

      return()  # already did print(output) and cat()
    }  # end object is both


    # ---------------------------
    # contcont 2-way scatter plot
    # ---------------------------

    if (!cat.x  &&  !cat.y  &&
      object %in% c("point", "both")  && !run) {

      # suggestions
      # -----------
      txsug <- ""
      if (getOption("suggest")) {
        txsug <- "\n>>> Suggestions  or  enter: style(suggest=FALSE)"

        fc <- paste("Plot(", x.name, ", ", y.name, sep="")

        if (!grepl("enhance", fncl)) {
          txt <- ", enhance=TRUE)  # many options"
          txsug <- paste(txsug, "\n", fc, txt, sep="")
        }

        if (runif(1) > 0.5) {
          if (!grepl("fill", fncl)) {
            txt <- ", fill=\"skyblue\")  # interior fill color of points"
            txsug <- paste(txsug, "\n", fc, txt, sep="")
          }
        }
        else {
          if (!grepl("color", fncl)) {
            txt <- ", color=\"red\")  # exterior edge color of points"
            txsug <- paste(txsug, "\n", fc, txt, sep="")
          }
        }

        if (!grepl("fit", fncl)) {
          txt <- ", fit=\"lm\", fit_se=c(.90,.99))  # fit line, stnd errors"
          txsug <- paste(txsug, "\n", fc, txt, sep="")
        }

        if (runif(1) > 0.5) {
          if (!grepl("out_cut", fncl)) {
            txt <- ", out_cut=.10)  # label top 10% from center as outliers"
            txsug <- paste(txsug, "\n", fc, txt, sep="")
          }
        }
        else {
          if (!grepl("MD_cut", fncl)) {
            txt <- ", MD_cut=6)"
            cmt <- "  # Mahalanobis distance from center > 6 is an outlier"
            txsug <- paste(txsug, "\n", fc, txt, cmt, sep="")
          }
        }

#        if (!grepl("ellipse", fncl)) {
#          txt <- ", ellipse=0.95, add=\"means\")  # 0.95 ellipse with means"
#          txsug <- paste(txsug, "\n", fc, txt, sep="")
#        }

#         if (!grepl("smooth", fncl)) {
#           txt <- ", shape=\"diamond\")  # change plot character"
#           txsug <- paste(txsug, "\n", fc, txt, sep="")
#         }
      }  # end suggest

      blank <- ""
      class(blank) <- "out"  # a blank line when needed

      txreg <- ""
      txcor <- ""

      # output cor info if no fit line or lm fit only
      # ---------------------------------------------

      # Linear correlation for a linear analysis
      if (fit %in% c("off", "lm") && !date.var) {

        for (i in 1:n_col) {
          class(txsug) <- "out"

          #  no output correlation if a by variable
          if (n.by == 1) {
            if (n.xcol > 1) {
              x.nm <- colnames(x)[i]
              x.nm <- paste("\nVariable:", x.nm, "with",  colnames(y)[1])
              class(x.nm) <- "out"
              if (exists("output"))
                output <- c(output, list(out_name=x.nm))
              else
                output <- list(out_name=x.nm)
              options(xname = colnames(x)[i])
              stuff <- .cr.main(x[,i], y[,1], brief=TRUE)
            }
            else {
              options(yname = colnames(y)[i])
              stuff <- .cr.main(x[,1], y[,i], brief=TRUE)
            }

            txbck <- stuff$txb
            txdsc <- stuff$txd
            txinf <- stuff$txi

            # txcor contains the basic correlational text output
            txcor <- c(txbck, txdsc, " ", txinf, " ")
          }  # end n.by is 1

        }  # end for i through n_col
      }  # end output cor info



      # output mse.ln, triggered by a non-lm fit line
      # ------------------------------------------

      # Create a y.new data frame with formatted numbers
      if (!is.null(y.new)) {
        if (.is.integer(fit_new))
          x1 <- format(fit_new, trim=TRUE) 
        else
          x1 <- format(fit_new, nsmall=digits_d, trim=TRUE) 
        x2 <- format(y.new, nsmall=digits_d, trim=TRUE)
        mx.x <- max(x[,1], na.rm=TRUE)
        txt <- "Prediction from beyond the data range"
        x3 <- ifelse(fit_new > mx.x, txt, " ")   
        tx <- data.frame(x1,x2,x3)
        names(tx) <- c(nm.x, paste(nm.y, "_Fit", sep=""), " ")
        dfnew <- tx
      }
      else
       dfnew <- NULL

      if (!is.null(mse.ln)  &&  n.xcol == 1) {  # mse.ln not reported for all

        if (fit == "lm") {
          op1 <- ""
          op2 <- ""
        }

        if (fit == "quad") {
          op1 <- "sqrt()"
          op2 <- "square"

        }
        if (fit == "power") {
          op1 <- "the root of the\n   reciprocal of the power"
          op2 <- "of the power"
        }

        if (fit == "exp") {
          op1 <- "log()"
          op2 <- "exp()"
        }

        if (fit == "log") {
          op1 <- "exp()"
          op2 <- "log()"
        }

        if (fit %in% c("quad", "power", "exp", "log")) {
          msg <- paste("Regressed linearized data of transformed",
                      "data values of", nm.y, "with", op1, "\n")
        }
        else
          msg <- ""

        tx <- character(length=n.by)
        for (i in 1:n.by) {
          by.name <- getOption("byname")
          if (i > 1) msg <- ""  # display title only at beginning
          if (n.by > 1) {
            if (fit %in% c("quad", "power", "exp", "log")) {
              msg <- paste("Regressed linearized data of transformed",
                          "data values of", nm.y, "with", op1, "\n")
            }
            tx[i] <- paste(msg, by.name, ": ", by.cat[i], "  ", sep="")
          }
          else
            tx[i] <- paste(msg, by.cat[i], "  ", sep="")
          mse.out <- .fmt_cm(mse.ln[i], d=digits_d)
          b0.pn <- .fmt(b0[i], digits_d)
          b1.pn <- .fmt(b1[i], digits_d)
          Rsq.pn <- .fmt(Rsq[i], 3)
          md.type <- ifelse (fit=="loess", "Loess", "Linear")
          if (!is.na(b1[i])) {  # linear function
            tx[i] <- paste(tx[i],
              "Line: b0 = ", b0.pn, "    b1 = ", b1.pn,
              "    ", md.type,  " Model MSE = ", mse.out, sep="")
              rsqu <- ifelse (is.na(Rsq[i]), "", paste("   Rsq =", Rsq.pn))
              tx[i] <- paste(tx[i], rsqu, "\n", sep="")
          }
          else {
            tx[i] <- paste(tx[i],
              " ", md.type, " Model MSE = ", mse.out, "\n", sep="")
          }

          if (!(fit %in% c("loess", "lm", "null"))) {
            msg <- paste("\nFit to the data with back transform",
                         op2, "of linear regression model")
            mse.out <- .fmt_pn(mse.nl[i], d=digits_d)
            tx[i] <- paste(tx[i], msg, "\n", "Model MSE =", mse.out, "\n\n")
          }

          # kludge, if removing outliers reg line info not correct,remove
          if (b0[i]==0 && b1[i]==0 && mse.ln[i]==0) tx <- ""
        }  # end for n.by

        txreg <- tx
      }  # end !is.null(mse.ln)

      class(txsug) <- "out"
      class(txcor) <- "out"
      class(txreg) <- "out"
      return(list(tipe="contcont", out_suggest=txsug,
                  out_stats=txcor, out_y.new=dfnew, out_reg=txreg))

    }  # end traditional 2-way scatter plot



      # --------------------------------
      # categorical var with numeric var for means plot or bubble-1D plot

      else if ((cat.x && !cat.y && !unique.x) ||
               (!cat.x && cat.y && !unique.y)) {

        if (!bubble1) {  # means plot

          txsug <- ""
          if (getOption("suggest")) {
            txsug <- "\n>>> Suggestions  or  enter: style(suggest=FALSE)"

            fc <- ""
            if (!grepl("means", fncl))
              fc <- paste(fc, ", means=FALSE", sep="")
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              txsug <- paste(txsug, "\n", fc, "  # do not plot means", sep="")
            }

            fc <- ""
            if (!grepl("stat", fncl)) {
              fc <- paste(fc, ", stat=\"mean\"", sep="")
              if (grepl("means", fncl)) fncl <- .rm.arg.l("means", fncl)
            }
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              txsug <- paste(txsug, "\n", fc, "  # only plot means", sep="")
            }

            if (cat.x) {
              rv <- y.name
              pv <- x.name
              n.lvl <- length(unique(x))
            }
            else {
              rv <- x.name
              pv <- y.name
              n.lvl <- length(unique(y))
            }
            fnct <- ifelse(n.lvl == 2, "ttest", "ANOVA")
            fc <- paste("\n", fnct, "(", rv, " ~ ", pv,
                        ")  # inferential analysis", sep="")
            txsug <- paste(txsug, fc, sep="")

            txsug <- .rm.arg.2(" x=", txsug)
            txsug <- .rm.arg.2("(x=", txsug)
            txsug <- .rm.arg.2(" y=", txsug)

          }  # end suggest

          # get stats
          if (cat.x && !cat.y) {
            if (!is.null(x.lvl))  # convert back to a factor if was one
              x.by <- factor(x, levels=1:length(x.lvl), labels=x.lvl)
            else
              x.by <- x
            options(yname = x.name)  # reverse order x and y for .ss.numeric()
            options(xname = y.name)
           stats <- .ss.numeric(y, by=x.by, digits_d=digits_d,
                                 brief=TRUE, y.name=x.name)
          }
          else if (!cat.x && cat.y) {
            if (!is.null(y.lvl))  # convert back to a factor if was one
              y.by <- factor(y, levels=1:length(y.lvl), labels=y.lvl)
            else
              y.by <- y
            stats <- .ss.numeric(x, by=y.by, digits_d=digits_d, brief=TRUE)
          }

          class(stats$tx) <- "out"
          return(list(tipe="catcont", out_stats=stats$tx, out_suggest=txsug))
        }  # !bubble_1

        else {  # 1-D bubble plot of a factor var, y just a constant

          txsug <- ""
          if (getOption("suggest")) {
            txsug <- "\n>>> Suggestions  or  enter: style(suggest=FALSE)"

            fc <- ""
            if (!grepl("color_low", fncl))
              fc <- paste(fc, ", color_low=\"lemonchiffon2\"", sep="")
            if (!grepl("color_hi", fncl))
              fc <- paste(fc, ", color_hi=\"maroon3\"", sep="")
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
            }

            fc <- paste("Plot(", x.name,
                   ", stat=\"count\")  # scatter plot of counts", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")

            txsug <- .rm.arg.2(" x=", txsug)
            txsug <- .rm.arg.2("(x=", txsug)
          }  # end suggest

          if (!is.null(x.lvl))
            x.by <- factor(x, levels=1:length(x.lvl), labels=x.lvl)
          else
            x.by <- factor(x)

          stats <- .ss.factor(x.by, by=NULL, brief=TRUE, digits_d=NULL,
                              x.name, y.name, x.lbl, y.lbl)
          txttl <- stats$title
          counts <- stats$count
          chi <- stats$chi

          class(txsug) <- "out"
          class(txttl) <- "out"
          class(counts) <- "out"
          class(chi) <- "out"
          output <- list(out_suggest=txsug, out_title=txttl,
                         out_counts=counts, out_chi=chi)
          class(output) <- "out_all"
          print(output)
        }  # else
      }  # end catcont


      # Cleveland dot plot
      else if (cleveland) {
        txsug <- ""
        if (getOption("suggest")) {
          txsug <- "\n>>> Suggestions  or  enter: style(suggest=FALSE)"
          fc <- ""
          if (!grepl("sort_yx", fncl)) {
            cmt <- "  # do not sort y-axis variable by x-axis variable"
            fc <- paste(fncl, ", sort_yx=\"0\"", ")", cmt, "\n", sep="")
          }
          if (!grepl("segments_y", fncl)) {
            cmt <- "  # drop the line segments"
            fc <- paste(fc, fncl, ", segments_y=FALSE", ")", cmt, "\n", sep="")
          }
          if (!grepl("fill", fncl)) {
            cmt <- "  # red point interiors"
            fc <- paste(fc, fncl, ", fill=\"red\"", ")", cmt, "\n", sep="")
          }
          if (nzchar(fc)) {
            fncl <- .fun_call.deparse(fun_call)
            fncl <- gsub(")$", "", fncl)  # get function call less closing
            fncl <- gsub(" = ", "=", fncl)
            txsug <- paste(txsug, "\n", fc, sep="")

            txsug <- .rm.arg.2(" x=", txsug)
            txsug <- .rm.arg.2(" y=", txsug)
            txsug <- .rm.arg.2("(x=", txsug)
          }

          class(txsug) <- "out"
          return(list(tipe="ts", out_suggest=txsug))
        }  # end suggest

        if (!is.null(y.lvl))
          # convert back to a factor if was one originally
          y.by <- factor(y, levels=1:length(y.lvl), labels=y.lvl)
        else
          y.by <- y

        tx <- ""
        for (i in 1:n.xcol) {
          stats <- .ss.numeric(x[,i], digits_d=digits_d, brief=TRUE)
          tx[length(tx)+1] <- paste("---", colnames(x)[i], "---")
          for (j in 2:length(stats$tx)) tx[length(tx)+1] <- stats$tx[j]
          if (i < n.xcol) {
            tx[length(tx)+1] <- ""
            tx[length(tx)+1] <- ""
          }
        }
        txstats <- tx
        txotl <- ""
        txotl <- .bx.stats(x)$txotl
        if (txotl[1] == "") txotl <- "No (Box plot) outliers"

        class(txsug) <- "out"
        class(txstats) <- "out"
        class(txotl) <- "out"

        return(list(out_suggest=txsug, out_stats=txstats, out_outliers=txotl))
      }  # end Cleveland


      # ------------------------
      # categorical x and y vars
      # ------------------------

      else if (cat.x && cat.y) {
        txsug <- ""
        if (getOption("suggest")) {
          txsug <- "\n>>> Suggestions  or  enter: style(suggest=FALSE)"

          fc <- ""
          if (!grepl("size_cut", fncl))
            fc <- paste(fc, ", size_cut=FALSE", sep="")
          if (nzchar(fc)) {
            fncl <- .fun_call.deparse(fun_call)
            fncl <- gsub(")$", "", fncl)  # get function call less closing )
            fncl <- gsub(" = ", "=", fncl)
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
          }

          fc <- ""
          if (!grepl("trans", fncl))
            fc <- paste(fc, ", trans=.8", sep="")
          if (!grepl("bg", fncl))
            fc <- paste(fc, ", bg=\"off\"", sep="")
          if (!grepl("grid", fncl))
            fc <- paste(fc, ", grid=\"off\"", sep="")
          if (nzchar(fc)) {
            fncl <- .fun_call.deparse(fun_call)
            fncl <- gsub(")$", "", fncl)  # get function call less closing )
            fncl <- gsub(" = ", "=", fncl)
            fc <- paste(fncl, fc, ") ", sep="")
            fc <- sub(",,", ",", fc, fixed=TRUE)  # hack
            txsug <- paste(txsug, "\n", fc, sep="")

          }

          fc <- paste("\nSummaryStats(", x.name, ", ", y.name,
                      ")  # or ss", sep="")

          txsug <- paste(txsug, fc, sep="")
          txsug <- .rm.arg.2(" x=", txsug)
          txsug <- .rm.arg.2("(x=", txsug)
          txsug <- .rm.arg.2(" y=", txsug)

        }

        if (!is.null(x.lvl))
          x.fac <- factor(x, levels=1:length(x.lvl), labels=x.lvl)
        else
          x.fac <- x[,1]
        if (!is.null(y.lvl))
          y.fac <- factor(y, levels=1:length(y.lvl), labels=y.lvl)
        else
          y.fac <- y

        stats <- .ss.factor(x.fac, y.fac, digits_d=3, brief=FALSE,
                            x.name, y.name, x.lbl, y.lbl)

        txttl <- stats$txttl
        txfrq <- stats$txfrq
        txXV <- stats$txXV

        class(txsug) <- "out"
        class(txttl) <- "out"
        class(txfrq) <- "out"
        class(txXV) <- "out"
        return(list(tipe="catcat",
                    out_title=txttl, out_suggest=txsug,
                    out_stats=txfrq, out_XV=txXV))
      }  # end catcat
#   }  # end object != "line"  &&  !run
  }  # end stat is data


  else {  # stat not data
    if (cat.x  &&  !cat.y  &&  object %in% c("point", "bubble")) {
      txsug <- ""
      if (getOption("suggest")) {
        txsug <- "\n>>> Suggestions  or  enter: style(suggest=FALSE)"

        fc <- ""
        if (!grepl("segments_x", fncl))
          fc <- paste(fc, ", segments_x=FALSE", sep="")
        if (nzchar(fc)) {
          fc <- paste(fncl, fc, ")  # just points", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }

        txsug <- .rm.arg.2(" x=", txsug)
        txsug <- .rm.arg.2("(x=", txsug)
        txsug <- .rm.arg.2(" y=", txsug)

        class(txsug) <- "out"
        if (nzchar(txsug))
          return(list(out_suggest=txsug))
      }  # end option suggest
    }
  }  # end stat not data
}
