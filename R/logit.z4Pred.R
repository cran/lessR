.logit4Pred <-
function(lm.out, nm, d, my_formula, brief, res_rows,
         n.vars, n.pred, n.obs, n.keep, digits_d, pre, line,
         new.data, pred, pred_all, prob_cut, 
         numeric.all, in.data.frame, X1_new, 
         X2_new, X3_new, X4_new, X5_new, X6_new,
         pdf_file, width, height, ...) {

# table(CarData$rating,predict(logisticModel,type='response')>=0.5)

  pred_sort <- TRUE  # data must be sorted to find cases close to fitted=0.5

# ----------
# prediction
# ----------

  # first iteration through loop, generate output
  # if length of prob_cut > 1, additional values for confusion matrix only
  if (length(prob_cut) == 1)
    p.cut <- prob_cut
  else
    p.cut <- 0.5   # 0.5 for pred output, rest for confusion matrices
    
  for (i in 1:length(prob_cut)) {
    
    # get output pred table, with label
    if (!new.data) {
      p.int <- data.frame(predict(lm.out, type="response", se.fit=TRUE))

      # classify
      label <- integer(length=nrow(p.int))
      for (irow in 1:nrow(p.int))
        label[irow] <- ifelse (p.int$fit[irow] < p.cut, 0, 1)

      if (all(label == 0)  ||  all(label == 1)) { 
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "For threshold ", p.cut, ", all predicted values are ",
          label[1], "\n\n")
      }

      out <- cbind(lm.out$model[c(nm[seq(2,n.vars)], nm[1])], label, 
                   p.int$fit, p.int$se.fit)
    }

    else {
      Xnew.val <- list(X1_new)
      if (n.vars > 2) for (i in 2:(n.pred)) {
        pp <- eval(parse(text=paste("X", toString(i),"_new",sep="")))
        Xnew.val <- c(Xnew.val, list(pp))
      }
      Xnew <- expand.grid(Xnew.val)
      for (i in 1:(n.pred)) names(Xnew)[i] <- nm[i+1]

      p.int <- data.frame(predict(lm.out, type="response",
                          se.fit=TRUE, newdata=Xnew))
      label <- integer(length=nrow(p.int))
      for (i in 1:nrow(p.int))
        label[i] <- ifelse (p.int$fit[i] < p.cut, 0, 1)

      Ynew <- character(length=nrow(Xnew))
      Ynew <- ""
      out <- cbind(Xnew, Ynew, label, p.int$fit,p.int$se.fit)
    }

    # display pred output only if on first iteration
    if (i == 1) {
     
      cat( "\n\n", "  FORECASTS", "\n\n")

      cat("Probability threshold for predicting ",
          levels(lm.out$model[,nm[1]])[2], ":", " ", p.cut, "\n\n", sep="")

      if (is.factor(lm.out$model[,nm[1]]))
        cat(" 0: ", levels(lm.out$model[,nm[1]])[1], "\n",
            " 1: ", levels(lm.out$model[,nm[1]])[2], "\n", sep="")
      cat("\n")

      cat("Data, Fitted Values, Standard Errors\n")
      cat("   [sorted by fitted value]\n")
      if (n.keep > 50 && pred_all == FALSE && !new.data) 
        cat("   [to save space only some intervals printed, ",
            " pred_all=TRUE to see all]\n")

      names(out)[n.vars+1] <- "predict"
      names(out)[n.vars+2] <- "fitted"
      names(out)[n.vars+3] <- "std.err"
      out <- data.frame(out, stringsAsFactors=TRUE)
      if (pred_sort) {
        o <- order(out[,n.vars+2])  # fitted value
        out <- out[o,]
      }

      .dash(68)
      if (n.keep < 25  || pred_all == TRUE || new.data)
        print(out, digits=digits_d)
      else {
        print(out[1:4,], digits=digits_d)
        cat("\n... for the rows of data where fitted is close to 0.5 ...\n\n")
        i.mid <- which.min(abs(0.5-sort(p.int$fit)))  # requires out sorted by fit
        print(out[(i.mid-2):(i.mid+2),], digits=digits_d)
        cat("\n... for the last 4 rows of sorted data ...\n\n")
        print(out[(n.keep-3):n.keep,], digits=digits_d)
      }
      .dash(68)

#     cat("\n\nConfusion Matrix for", nm[1], "\n\n")

      cat("\n\n")
      cat("----------------------------\n")
      cat("Specified confusion matrices\n")
      cat("----------------------------\n")
      cat("\n")
    }  # end (i == 1)  # pred output

    # confusion matrix
    if (i > 1) cat("\n\n")
    .logit5Confuse(lm.out, out, n.vars, nm, new.data, prob_cut[i])

  }  # end (i in 1:length(p.cut))


  # graphics
  if (pred && n.pred==1 && !is.factor(lm.out$model[,nm[2]]) && is.null(X1_new)){

    .opendev(pdf_file, width, height)

    x.values <- lm.out$model[,nm[2]]
    if (!is.factor(lm.out$model[,nm[1]])) {
      y.values <- lm.out$model[,nm[1]] 
      y.label <- paste("Probability of", nm[1])
    }
    else {
      y.values <- as.numeric(lm.out$model[,nm[1]])
      min.y <- min(y.values, na.rm=TRUE)
      y.label <- paste("Probability ", nm[1], " = ",
        levels(lm.out$model[,nm[1]])[2], sep="")
      for (i in 1:length(y.values))
        y.values[i] <- ifelse (y.values[i]==min.y, 0, 1) 
    }
 
    # set margins
    max.width <- strwidth(as.character(max(pretty(y.values))), units="inches")
    
    margs <- .marg(max.width, y.lab=nm[1], x.lab=nm[2], main=NULL, sub=NULL)
    lm <- margs$lm + 0.2
    tm <- margs$tm
    rm <- margs$rm
    bm <- margs$bm

    par(bg=getOption("window_fill"))
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    par(mai=c(bm, lm, tm, rm))

    # plot
    plot(x.values,y.values, type="n", axes=FALSE, ann=FALSE,
         ylim=c(-.10,1.10), ...)
    usr <- par("usr")

    # background color
    fill_bg <- getOption("panel_fill")
    rect(usr[1], usr[3], usr[2], usr[4], col=fill_bg, border="transparent")

    # grid lines
    min.x <- min(x.values, na.rm=TRUE)
    max.x <- max(x.values, na.rm=TRUE)
    axT1 <- pretty(c(min.x, max.x))
    axT2 <- seq(0,1,.2)
    .grid("v", axT1)
    .grid("h", axT2)

    # box around plot
    rect(usr[1], usr[3], usr[2], usr[4],
      col="transparent", border=getOption("panel_color"),
      lwd=getOption("panel_lwd"), lty=getOption("panel_lty"))

    .axes(NULL, NULL, axTicks(1), axTicks(2))

    main.lab <- NULL
    sub.lab <- NULL
    .axlabs(nm[2], y.label, main.lab, sub.lab, 
            cex.lab=getOption("lab_cex"), cex.main=1.0, ...) 

    fill <- getOption("pt_fill")
    trans <- .7
    fill <- .maketrans(fill, (1-trans)*256)
    color <- getOption("pt_color")
    points(x.values,y.values, pch=21, col=color, bg=fill, cex=0.8)
    lines(x.values, p.int$fit, col=color, lwd=2)

    if (!is.null(pdf_file)) {
      dev.off()
      .showfile(pdf_file, "fitted values and scatter plot")
    }
  }

  else {  # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {

      .opendev(pdf_file, width, height)

      panel2.smooth <- function (x, y, pch=par("pch"), cex=.9,
        col.pt=getOption("pt_color"), col.smooth=getOption("col.bar_color"),
        span=2/3, iter=3, ...) 
      {
          usr <- par("usr")          
          rect(usr[1], usr[3], usr[2], usr[4],
               col=getOption("border_fill"), border=getOption("border_color"))
          points(x, y, pch=pch, col=col.pt, bg=getOption("pt_fill"), cex=cex)

          ok <- is.finite(x) & is.finite(y)
          if (any(ok)) 
            lines(lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth, ...)
      }
      pairs(lm.out$model[c(nm)], panel=panel2.smooth)

      if (!is.null(pdf_file)) {
        dev.off()
        .showfile(pdf_file, "scatter plot matrix")
      }
    }
    else {
      cat("\n\n>>> No scatterplot matrix reported because not all variables are ")
      if (!in.data.frame) cat("in the data frame.\n")
      if (!numeric.all) cat("numeric.\n")
    }
  }


}
