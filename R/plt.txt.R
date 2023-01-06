.plt.txt <- 
function(x, y, values, object, cat.x,  cat.y,
       xlab, ylab, fit, n.by, mse, b0, b1, Rsq, by.cat, 
       center_line, run, show_runs, prop, size, radius, digits_d, 
       fun_call=NULL, txdif=NULL) {


  date.ts <- ifelse (.is.date(x[,1]), TRUE, FALSE)
  if (date.ts) center_line <- "off"

  # x and y come across here in their natural state, within each data frame
  # a time series has dates for x and numeric for y, factors are factors, etc

  bubble1 <- ifelse (length(unique(y[,1])) == 1, TRUE, FALSE)

  unique.x <- ifelse (length(unique(x[,1])) == length(x[,1]), TRUE, FALSE)
  unique.y <- ifelse (length(unique(y[,1])) == length(y[,1]), TRUE, FALSE)

  # all processing in terms of numeric variables
  # convert factors to numeric, save levels, so x and y are always numeric
  # x will always be a matrix
  x.lvl <- NULL; y.lvl <- NULL  # if remain null, then not factors
    nm.x <- names(x)
  if (is.factor(x[,1])) {
    x.lvl <- levels(x[,1])
    x <- as.matrix(as.integer(x[,1]))
  }
  else if (!date.ts) {
    x <- as.matrix(x)
    colnames(x) <- nm.x
  }

  nm.y <- names(y)
  if (is.factor(y[,1])) {
    y.lvl <- levels(y[,1])
    y <- as.matrix(as.integer(y[,1]))

  }
  else if (!date.ts) {
    y <- as.matrix(y)
    colnames(y) <- nm.y
  }

  # dimensions
  n.xcol <- ncol(x)
  n.ycol <- ncol(y)  
  n_col <- max(n.xcol, n.ycol)
  nrows <- nrow(x)
  
  if (date.ts) {
    x.val <- x[,1]
    x <- as.matrix(x.val, ncol=1)
  }

  if (is.null(x.lvl) && !is.null(y.lvl) && unique.y || 
      is.null(y.lvl) && !is.null(x.lvl) && unique.x) {
    cleveland <- TRUE 
  }
  else
    cleveland <- FALSE


  #if (!date.ts) {
    #num.cat.x <- is.null(x.lvl)  &&  .is.num.cat(x[,1], n_cat)
    #cat.x <- ifelse (num.cat.x || !is.null(x.lvl), TRUE, FALSE)
  #}
  #else {
    #num.cat.x <- FALSE
    #cat.x <- FALSE
  #}
  #if (!bubble1  &&  !date.ts) {
    #num.cat.y <- is.null(y.lvl) && .is.num.cat(y[,1], n_cat)
    #cat.y <- ifelse (num.cat.y || !is.null(y.lvl), TRUE, FALSE)
  #}
  #else {
    #num.cat.y <- FALSE
    #cat.y <- FALSE
  #}
        #cat.x <- TRUE
        #num.cat.x <- TRUE
        #cat.y <- TRUE
        #num.cat.y <- TRUE


  gl <- .getlabels(xlab, ylab)  # this redoes if already a plot
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
  #by.name <- getOption("byname")

  # decimal digits
  if (is.null(digits_d)) digits_d <- .max.dd(y[,1]) + 1
  options(digits_d=digits_d)

  size.pt <- ifelse (is.null(size), 1, size)  # dummy non-zero value
    
  if (n_col > 1) center_line <- "off"   # no center_line for multiple plots

  if (center_line == "mean") {
    m.y <- mean(y[,1], na.rm=TRUE)
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
    # function call for suggestions
    fncl <- .fun_call.deparse(fun_call)  # class call to class character
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)
  }
  

  # comment after the suggestion?
  cmt <- function(ct, mx.ch=88) {
    fc <- gsub(" = ", "=", fc)
    nch <- nzchar(paste(fncl, fc, ct))
    if (nch > mx.ch) ct <- "" 
    fc <- paste(fncl, fc, ")  ", ct, sep="")
    txsug <- paste(txsug, "\n", fc, sep="")
  }

  if (values == "data") {

    if (object != "line"  &&  !run) {
    
        # ---------------------------
        # contcont 2-way scatter plot
        # ---------------------------

      if (!cat.x  &&  !cat.y  && 
          object %in% c("point", "bubble", "both")  && !run) {
        txsug <- ""
  
        # suggestions
        # -----------
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"

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
              txt <- ", MD_cut=6)  # label Mahalanobis dist > 6 as outliers"
              txsug <- paste(txsug, "\n", fc, txt, sep="")
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
          

          if (object == "bubble") {
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
                }
              }
            }
          }  # end bubble

        }  # end suggest

        blank <- ""
        class(blank) <- "out"  # a blank line when needed

        txreg <- ""
        txcor <- ""

        # output correlation info if no fit line or lm fit only
        # -----------------------------------------------------

        if (fit %in% c("off", "lm")) {

          for (i in 1:n_col) {
            class(txsug) <- "out"

            #  no output correlation if a by variable 
            if (n.by == 0) {
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
            }  # end n.by is 0 

          }  # end for i through n_col
        }  # end output cor info


        # output mse, triggered by a non-lm fit line
        # ------------------------------------------

        if (!is.null(mse)  &&  n.xcol == 1) {  # mse not reported for all
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
            msg <- paste(" Regressed linearized data of transformed",
                        "data values of", nm.y, "with", op1, "\n")
            msg <- paste(msg, "For predicted values, back transform with",
                         op2, "of regression model\n\n")
          }
          else
            msg <- ""

          if (n.by > 0) {
            tx <- character(length=n.by)
              
            for (i in 1:n.by) {
              by.name <- getOption("byname")
              if (i > 1) msg <- ""
              tx[i] <- paste(msg, by.name, ": ", by.cat[i], "  ", sep="") 
              mse.pn <- prettyNum(mse[i], big.mark=",", scientific=FALSE,
                                  format="f", digits=digits_d)
              b0.pn <- .fmt(b0[i], digits_d)
              b1.pn <- .fmt(b1[i], digits_d)
              Rsq.pn <- .fmt(Rsq[i], 3)
              if (!is.na(b1[i])) {  # linear function
                tx[i] <- paste(tx[i], 
                  "Line: b0 =", b0.pn, "   b1 =", b1.pn,
                  "   Fit: MSE =", mse.pn) 
                  rsqu <- ifelse (is.na(Rsq[i]), "", paste("   Rsq =", Rsq.pn))
                  tx[i] <- paste(tx[i], rsqu, "\n", sep="")
              }
              else {
                tx[i] <- paste(tx[i], 
                  " Fit: Mean Squared Error, MSE = ", mse.pn, "\n", sep="")
              }

              # kludge, if removing outliers reg line info not correct,remove
              if (b0[i]==0 && b1[i]==0 && mse[i]==0) tx <- ""
            }  # end for n.by
          }  # end n.by > 0

          else {  # no by vars
            if (length(b1) == 1) {  # > 1 if y=c(y1, y2, ...)
              if (mse[1] > 10000)
                mse.pn <- prettyNum(mse[1], big.mark=",", scientific=FALSE,
                                    format="f", digits=2)  # digits does not work
              else
                mse.pn <- .fmt(mse[1], 3)  # 3 dec digits for smaller numbers

              if (!is.na(b0[1])) {  # missing in loess
                n_digs <- ifelse(b0[1] > 10000, 2, digits_d)
                if (n_digs == 1) n_digs <- 2
                b0.pn <- .fmt(b0[1], n_digs)
                if (abs(b0[1]) < 1) n_digs <- 4
              }

              if (!is.na(b1[1])) {
                n_digs <- ifelse(b1[1] > 10000, 2, digits_d)
                if (n_digs == 1) n_digs <- 2
                if (abs(b1[1]) < 1) n_digs <- 4
                b1.pn <- .fmt(b1[1], n_digs)
              }

              Rsq.pn <- .fmt(Rsq[1], 3)

              if (!is.na(b1)) {  # linear function
                tx = paste(msg,
                      "Line: b0 =", b0.pn, "  b1 =", b1.pn,
                      "   Fit: MSE =", mse.pn) 
                rsqu <- ifelse (is.na(Rsq[1]), "", paste("   Rsq =", Rsq.pn))
                tx <- paste(tx, rsqu, "\n", sep="")

              }
              else {
                tx = paste( 
                  "Fit: Mean Squared Error, MSE = ", mse.pn, "\n", sep="")
              }
              # kludge, if removing outliers reg line info not correct,remove
              if (b0[1]==0 && b1[1]==0 && mse[1]==0) tx <- ""
            } 
            else
              tx <- ""  # currently no reg output if length(b1) > 0
          } 

          txreg <- tx
        }  # end !is.null(mse)

          class(txcor) <- "out"
          class(txreg) <- "out"
          return(list(tipe="contcont", out_suggest=txsug,
                      out_stats=txcor, out_reg=txreg))

      }  # end traditional 2-way scatter plot


      # --------------------------------
      # categorical var with numeric var for means plot or bubble-1D plot

      else if ((cat.x && !cat.y && !unique.x) ||
               (!cat.x && cat.y && !unique.y)) {
   
        if (!bubble1) {  # means plot

          txsug <- ""
          if (getOption("suggest")) {
            txsug <- ">>> Suggestions"

            fc <- ""
            if (!grepl("means", fncl))
              fc <- paste(fc, ", means=FALSE", sep="")
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              txsug <- paste(txsug, "\n", fc, "  # do not plot means", sep="")
            }
            
            fc <- ""
            if (!grepl("values", fncl)) {
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
          return(list(tipe="catcont", out_stats=stats$tx))
        }  # !bubble_1

        else {  # 1-D bubble plot of a factor var, y just a constant

          txsug <- ""
          if (getOption("suggest")) {
            txsug <- ">>> Suggestions"
            
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
                   ", values=\"count\")  # scatter plot of counts", sep="")
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
          txsug <- ">>> Suggestions"
          fc <- ""
          if (!grepl("sort_yx", fncl))
            fc <- paste(fc, ", sort_yx=FALSE", sep="")
          if (!grepl("segments_y", fncl)) 
            fc <- paste(fc, ", segments_y=FALSE", sep="")
          if (nzchar(fc)) {
            fncl <- .fun_call.deparse(fun_call) 
            fncl <- gsub(")$", "", fncl)  # get function call less closing
            fncl <- gsub(" = ", "=", fncl)
            fc <- paste(fncl, fc, ") ", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")
            
            txsug <- .rm.arg.2(" x=", txsug) 
            txsug <- .rm.arg.2("(x=", txsug) 
         }
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
          class(txdif) <- "out"

            return(list(out_suggest=txsug, out_stats=txstats, out_outliers=txotl,
                           out_diff=txdif))
      }  # end Cleveland


      # ------------------------
      # categorical x and y vars
      # ------------------------

      else if (cat.x  &&  cat.y) {
        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"
          
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
                    out_title=txttl, out_stats=txfrq, out_XV=txXV))

      }  # end catcat
    }  # end object != "line"  &&  !run
    
    else {  # line, run chart (object is "both")
  
      txsug <- ""
      if (getOption("suggest")) {
        txsug <- ">>> Suggestions"

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
        if (!grepl("fill", fncl)  &&  (!grepl("stack", fncl)))
          fc <- paste(fc, ", fill=\"on\"", sep="")
        if (nzchar(fc)) {
          fc <- gsub(" = ", "=", fc)
          fc <- paste(fncl, fc, ")   # default color fill", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }        

        txsug <- .rm.arg.2(" x=", txsug) 
        txsug <- .rm.arg.2("(x=", txsug) 
        txsug <- .rm.arg.2(" y=", txsug)   
      }
      
      class(txsug) <- "out"
      output <- list(out_suggest=txsug)
      class(output) <- "out_all"
      print(output)
      
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
     
    }  # end line chart
    
  }  # end if (values == "data")

  else {  # values not data

    if (cat.x  &&  !cat.y  &&  object %in% c("point", "bubble")) {
      txsug <- ""
      
      if (getOption("suggest")) {
        txsug <- ">>> Suggestions"
          
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

        if (nzchar(txsug)) {
          output <- list(out_suggest=txsug)
          class(output) <- "out_all"
          print(output)
        }
      }
    }  # end values not data
    
  }

}
