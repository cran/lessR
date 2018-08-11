.plt.txt <- 
function(x, y, values, object, n.cat,
         cat.x, num.cat.x, cat.y, num.cat.y,
         xlab, ylab, smooth, box.adj,
         center.line, prop, size, show.runs, radius, digits.d, 
         fun.call=NULL) {


  date.ts <- ifelse (.is.date(x[,1]), TRUE, FALSE)

  if (date.ts) center.line <- "off"

  # x and y come across here in their natural state, within each data frame
  # a time series has dates for x and numeric for y, factors are factors, etc
  
  bubble1 <- ifelse (length(unique(y[,1])) == 1, TRUE, FALSE)

  unique.x <- ifelse(length(unique(x[,1])) == length(x[,1]), TRUE, FALSE)
  unique.y <- ifelse(length(unique(y[,1])) == length(y[,1]), TRUE, FALSE)

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
    nm.y <- names(y)
    y <- as.matrix(y)
    colnames(y) <- nm.y
  }

  # dimensions
  n.xcol <- ncol(x)
  n.ycol <- ncol(y)  
  n.col <- max(n.xcol, n.ycol)
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
    #num.cat.x <- is.null(x.lvl)  &&  .is.num.cat(x[,1], n.cat)
    #cat.x <- ifelse (num.cat.x || !is.null(x.lvl), TRUE, FALSE)
  #}
  #else {
    #num.cat.x <- FALSE
    #cat.x <- FALSE
  #}
  #if (!bubble1  &&  !date.ts) {
    #num.cat.y <- is.null(y.lvl) && .is.num.cat(y[,1], n.cat)
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


  gl <- .getlabels(xlab, ylab)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
  #by.name <- getOption("byname")

  # decimal digits
  if (is.null(digits.d)) digits.d <- .max.dd(y[,1]) + 1
  options(digits.d=digits.d)

  size.pt <- ifelse (is.null(size), 1, size)  # dummy non-zero value
    
  # by default display center.line only if runs, so detect if a run
  if (n.col > 1) center.line <- "off"   # no center.line for multiple plots
  if (center.line == "default"  &&  !date.ts  &&  object == "both") {
    m <- mean(y, na.rm=TRUE)
    n.change <- 0
    for (i in 1:(length(y)-1))
      if ((y[i+1] > m) != (y[i] > m)) n.change <- n.change+1 
    if (n.change/(length(y)-1) < .15)
      center.line <- "off" 
    else 
      center.line <- "median"
  }
  else  # default if not automatically assigned above
    if (!(center.line %in% c("off", "mean"))) center.line <- "median"

  if (center.line != "off") {
    if (center.line == "mean") {
      m.y <- mean(y[,1], na.rm=TRUE)
      lbl <- " mean"
      lbl.cat <- "mean:"
    }
    else if (center.line == "median") {
      m.y <- median(y[,1], na.rm=TRUE)
      lbl <- " medn"
      lbl.cat <- "median:"
    }
    else if (center.line == "zero") {
      m.y <- 0
      lbl <- ""
      lbl.cat <- "zero:"
    }
  }


  # -----------
  # text output
  # -----------

  if (getOption("suggest")) {
    # function call for suggestions
    fncl <- .fun.call.deparse(fun.call) 
    fncl <- gsub(")$", "", fncl)  # get function call less closing ) 
    fncl <- gsub(" = ", "=", fncl)
  }
  

  # comment after the suggestion?
  cmt <- function(ct, mx.ch=88) {
    fc <- gsub(" = ", "=", fc)
    nch <- nchar(paste(fncl, fc, ct))
    if (nch > mx.ch) ct <- "" 
    fc <- paste(fncl, fc, ")  ", ct, sep="")
    txsug <- paste(txsug, "\n", fc, sep="")
  }


  if (values == "data") {

    if (!(object %in% c("line", "both"))) {
    
      # traditional two-var numeric var scatter plot
      if (!cat.x  &&  !cat.y  &&  object %in% c("point", "bubble")) {
        txsug <- ""
  
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"

          fc <- paste("Plot(", x.name, ", ", y.name, sep="")

          if (!grepl("fit", fncl)) {
            txt <- ", fit=\"lm\", fit.se=c(.90,.99))  # fit line, standard errors"
            txsug <- paste(txsug, "\n", fc, txt, sep="")
          }

          if (!grepl("out.cut", fncl)) {
            txt <- ", out.cut=.10)  # label top 10% potential outliers"
            txsug <- paste(txsug, "\n", fc, txt, sep="")
          }

          if (!grepl("ellipse", fncl)) {
            txt <- ", ellipse=0.95, add=\"means\")  # 0.95 ellipse with means"
            txsug <- paste(txsug, "\n", fc, txt, sep="")
          }

          if (!grepl("auto", fncl)) {
            txt <- ", auto=TRUE)  # many options, including the above"
            txsug <- paste(txsug, "\n", fc, txt, sep="")
          }

          if (!grepl("smooth", fncl)) {
            txt <- ", shape=\"diamond\")  # change plot character"
            txsug <- paste(txsug, "\n", fc, txt, sep="")
          }
          

          if (object == "bubble") {
            fc <- ""
            smaller <- as.character(.fmt(radius / 1.5, 2))
            larger <- as.character(.fmt(radius * 1.5, 2))
            if (!grepl("bubble", fncl)) {
              if (radius >= 0.25) {
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
          }  # end bubble

        }  # end suggest

        for (i in 1:n.col) {

          if (n.xcol > 1) {
            options(xname = colnames(x)[i])
            stuff <- .cr.main(x[,i], y[,1], brief=TRUE) 
          }
          else {
            options(yname = colnames(y)[i])
            stuff <- .cr.main(x[,1], y[,i], brief=TRUE) 

            txbck <- stuff$txb
            txdsc <- stuff$txd
            txinf <- stuff$txi

            class(txsug) <- "out_piece"
            class(txbck) <- "out_piece"
            class(txdsc) <- "out_piece"
            class(txinf) <- "out_piece"

            if (nzchar(txsug)  &&  i == 1)
              output <- list(out_suggest=txsug, out_background=txbck,
                out_describe=txdsc, out_inference=txinf,
                r=stuff$r, tvalue=stuff$tvalue, df=stuff$df, pvalue=stuff$pvalue,
                lb=stuff$lb, ub=stuff$ub)
            else
              output <- list(out_background=txbck,
                out_describe=txdsc, out_inference=txinf,
                r=stuff$r, tvalue=stuff$tvalue, df=stuff$df, pvalue=stuff$pvalue,
                lb=stuff$lb, ub=stuff$ub)

            class(output) <- "out_all"
            print(output)
          }
        }  # end for i

      }  # end traditional 2-way scatter plot


      # --------------------------------
      # categorical var with numeric var for means plot or bubble-1D plot
      else if ((cat.x && !cat.y && !unique.x) || (!cat.x && cat.y && !unique.y)) {
   
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
              fc <- paste(fc, ", values=\"mean\"", sep="")
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

          if (cat.x && !cat.y) {
            if (!is.null(x.lvl))  # convert back to a factor if was one
              x.by <- factor(x, levels=1:length(x.lvl), labels=x.lvl)
            else
              x.by <- x
            options(yname = x.name)  # reverse order of x and y for .ss.numeric
            options(xname = y.name)
            stats <- .ss.numeric(y, by=x.by, digits.d=digits.d,
                                 brief=TRUE, y.name=x.name)
          }
          else if (!cat.x && cat.y) {
            if (!is.null(y.lvl))  # convert back to a factor if was one
              y.by <- factor(y, levels=1:length(y.lvl), labels=y.lvl)
            else
              y.by <- y
            stats <- .ss.numeric(x, by=y.by, digits.d=digits.d, brief=TRUE)
          }

          txout <- stats$tx

          class(txout) <- "out_piece"

          output <- list(out_suggest=txsug, out_txt=txout)
          class(output) <- "out_all"
          print(output)
        }  # !bubble.1

        else {  # 1-D bubble plot of a factor var, y just a constant

          txsug <- ""
          if (getOption("suggest")) {
            txsug <- ">>> Suggestions"
            
            fc <- ""
            if (!grepl("color.low", fncl))
              fc <- paste(fc, ", color.low=\"lemonchiffon2\"", sep="")
            if (!grepl("color.hi", fncl))
              fc <- paste(fc, ", color.hi=\"maroon3\"", sep="")
            if (nzchar(fc)) {
              fc <- paste(fncl, fc, ") ", sep="")
              txsug <- paste(txsug, "\n", fc, sep="")
            }

            fc <- paste("Plot(", x.name,
                   ", values=\"count\")  # scatter plot of counts", sep="")
            txsug <- paste(txsug, "\n", fc, sep="")

            txsug <- .rm.arg.2(" x=", txsug) 
            txsug <- .rm.arg.2("(x=", txsug) 
          }
        
          if (!is.null(x.lvl))
            x.by <- factor(x, levels=1:length(x.lvl), labels=x.lvl)
          else
            x.by <- factor(x)

          stats <- .ss.factor(x.by, by=NULL, brief=TRUE, digits.d=NULL,
                              x.name, y.name, x.lbl, y.lbl)

          txttl <- stats$title
          counts <- stats$count
          chi <- stats$chi

          class(txsug) <- "out_piece"
          class(txttl) <- "out_piece"
          class(counts) <- "out_piece"
          class(chi) <- "out_piece"
          output <- list(out_suggest=txsug, out_title=txttl,
                         out_counts=counts, out_chi=chi)
          class(output) <- "out_all"
          print(output)      
        }  # else
      }


      # Cleveland dot plot
      else if (cleveland) { 

        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"
          fc <- ""
          if (!grepl("sort.yx", fncl))
            fc <- paste(fc, ", sort.yx=FALSE", sep="")
          if (!grepl("segments.y", fncl)) 
            fc <- paste(fc, ", segments.y=FALSE", sep="")
          if (nzchar(fc)) {
            fncl <- .fun.call.deparse(fun.call) 
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

        txout <- ""
        for (i in 1:n.xcol) {
          stats <- .ss.numeric(x[,i], digits.d=digits.d, brief=TRUE)
          txout[length(txout)+1] <- paste("---", colnames(x)[i], "---")
          for (j in 2:length(stats$tx)) txout[length(txout)+1] <- stats$tx[j]
          if (i < n.xcol) {
            txout[length(txout)+1] <- ""
            txout[length(txout)+1] <- ""
          }
        }
      
        txotl <- ""
        txotl <- .bx.stats(x)$txotl
        if (txotl[1] == "") txotl <- "No (Box plot) outliers"

          class(txsug) <- "out_piece"
          class(txout) <- "out_piece"
          class(txotl) <- "out_piece"

          if (nzchar(txsug))
            output <- list(out_suggest=txsug, out_txt=txout, out_outliers=txotl)
          else
            output <- list(out_txt=txout, out_outliers=txotl)
          class(output) <- "out_all"
          print(output)
      }  # end Cleveland


      # categorical x and y vars

      else if (cat.x  &&  cat.y) {
        txsug <- ""
        if (getOption("suggest")) {
          txsug <- ">>> Suggestions"
          
          fc <- ""
          if (!grepl("size.cut", fncl))
            fc <- paste(fc, ", size.cut=FALSE", sep="")
          if (nzchar(fc)) {
            fncl <- .fun.call.deparse(fun.call) 
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
            fncl <- .fun.call.deparse(fun.call) 
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

        stats <- .ss.factor(x.fac, y.fac, brief=FALSE, digits.d=NULL,
                            x.name, y.name, x.lbl, y.lbl)

        txttl <- stats$txttl
        txfrq <- stats$txfrq
        txXV <- stats$txXV

        class(txsug) <- "out_piece"
        class(txttl) <- "out_piece"
        class(txfrq) <- "out_piece"
        class(txXV) <- "out_piece"
        if (!prop)
          output <- list(out_suggest=txsug, out_title=txttl, out_text=txfrq,
                         out_XV=txXV)
        else {
          txrow <- stats$txrow
          class(txrow) <- "out_piece"
          output <- list(out_title=txttl, out_text=txfrq,
                         out_row=txrow, out_XV=txXV)   }

        class(output) <- "out_all"
        print(output)
      }
      
    }
    
    else {  # line chart (object is "both")
  
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
            fc <- paste(fc, ", area=TRUE", sep="")
            txt <- "just area"
          }
          fc <- paste(fncl, fc, ")   # ", txt, sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }
          
        fc <- ""
        if (!grepl("area", fncl)  &&  (!grepl("stack", fncl)))
          fc <- paste(fc, ", area=TRUE", sep="")
        if (nzchar(fc)) {
          fc <- gsub(" = ", "=", fc)
          fc <- paste(fncl, fc, ")   # default color fill", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }        

        txsug <- .rm.arg.2(" x=", txsug) 
        txsug <- .rm.arg.2("(x=", txsug) 
        txsug <- .rm.arg.2(" y=", txsug)   
      }
      
      class(txsug) <- "out_piece"
      output <- list(out_suggest=txsug)
      class(output) <- "out_all"
      print(output)
      
      # analyze runs
      if (!date.ts  &&  center.line != "off") {
        cat("n:", nrows, "\n")
        n.miss <- sum(is.na(y))
        cat("missing:", n.miss, "\n")
        cat(lbl.cat, round(m.y,digits.d), "\n")
        cat("\n")
        .dash(12); cat("Run Analysis\n"); .dash(12)
        run <- integer(length=0)  # length of ith run in run[i]
        n.runs <- 1  # total number of runs
        run[n.runs] <- 1
        line.out <- "    1"
        cat("\n")
        for (i in 2:length(y)) {
          if (y[i] != m.y) {  # throw out values that equal m.y
            if (sign(y[i]-m.y) != sign(y[i-1]-m.y)) {  # new run
              if (show.runs) {
                if (n.runs < 10) buf <- "  " else buf <- " "
                cat("size=", run[n.runs], "  Run", buf, n.runs, ":",
                    line.out, "\n", sep="")
              }
              line.out <- ""
              n.runs <- n.runs + 1
              run[n.runs] <- 0
            }
            run[n.runs] <- run[n.runs] + 1
            buf <- ifelse (i < 10, "  ", " ")
            line.out <- paste(line.out, buf, i)
          }
        }
        cat("size=", run[n.runs], "  Run", buf, n.runs, ":", line.out,
            "\n", sep="")
        eq.ctr <- which(y==m.y)
        cat("\nTotal number of runs:", n.runs, "\n")
        txt <- "Total number of values that do not equal the "
        cat(txt, lbl.cat, " ", length(y)-length(eq.ctr), "\n", sep="")
        if (length(eq.ctr) != 0) {
          if (show.runs) {
            cat("\nValues ignored that equal the", lbl.cat, "\n")
            for (i in 1:length(eq.ctr))
              cat("    #", eq.ctr[i], " ", y[eq.ctr[i]], sep="", "\n")
            cat("Total number of values ignored:", length(eq.ctr), "\n")
          }
        }
        else 
          cat("Total number of values ignored that equal the", lbl.cat, 
              length(eq.ctr), "\n")
      }  # end analyze runs
     
    }  # end line chart
    
  }  # end if (values == "data")

  else {  # values not data

    if (cat.x  &&  !cat.y  &&  object %in% c("point", "bubble")) {
      txsug <- ""
      
      if (getOption("suggest")) {
        txsug <- ">>> Suggestions"
          
        fc <- ""
        if (!grepl("segments.x", fncl))
          fc <- paste(fc, ", segments.x=FALSE", sep="")
        if (nzchar(fc)) {
          fc <- paste(fncl, fc, ")  # just points", sep="")
          txsug <- paste(txsug, "\n", fc, sep="")
        }

        txsug <- .rm.arg.2(" x=", txsug) 
        txsug <- .rm.arg.2("(x=", txsug)
        txsug <- .rm.arg.2(" y=", txsug) 

        class(txsug) <- "out_piece"

        if (nzchar(txsug)) {
          output <- list(out_suggest=txsug)
          class(output) <- "out_all"
          print(output)
        }
      }
    }  # end values not data
    
  }
    
  cat("\n")

}
