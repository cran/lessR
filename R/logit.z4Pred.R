.logit4Pred <-
function(nm, mydframe, my.formula, brief, res.rows,
         n.vars, n.pred, n.obs, n.keep, digits.d, pre, line,
         new.data, pred.sort, pred, pred.all, 
         numeric.all, in.data.frame, X1.new, 
         X2.new, X3.new, X4.new, X5.new,
         pdf, pdf.width, pdf.height) {

# ----------
# prediction
# ----------
     
  cat( "\n\n", "  FORECASTS", "\n\n")

  cat("Data, Fitted Values, Standard Errors\n")
  if (pred.sort) cat("   [sorted by lower bound of prediction interval]\n")
  if (n.keep > 50 && pred.all == FALSE && !new.data) 
    cat("   [to save space only some intervals printed, do pred.all=TRUE to see all]\n")
  .dash(68)
  
  if (!new.data) {
    p.int <- data.frame(predict(lm.out, type="response", se.fit=TRUE))
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],p.int$fit,p.int$se.fit)
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "std.err"
  }
  else {
    Xnew.val <- list(X1.new)
    if (n.vars > 2) for (i in 2:(n.pred)) {
      pp <- eval(parse(text=paste("X", toString(i),".new",sep="")))
      Xnew.val <- c(Xnew.val, list(pp))
    }
    Xnew <- expand.grid(Xnew.val)
    for (i in 1:(n.pred)) names(Xnew)[i] <- nm[i+1]
    p.int <- data.frame(predict(lm.out, type="response", se.fit=TRUE, newdata=Xnew))
    Ynew <- character(length = nrow(Xnew))
    Ynew <- ""
    out <- cbind(Xnew, Ynew, p.int$fit,p.int$se.fit)
    names(out)[n.vars] <- nm[1]
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "std.err"
  }
  
  out <- data.frame(out)
  if (pred.sort) {
    o <- order(out[,n.vars+1])  # fitted value
    out <- out[o,]
  }
  if (n.keep < 50  || pred.all == TRUE || new.data)
    print(out, digits=digits.d)
  else {
    print(out[1:5,], digits=digits.d)
    cat("\n... for the middle 5 rows of sorted data ...\n\n")
    n.mid <- round(n.keep/2)
    print(out[(n.mid-2):(n.mid+2),], digits=digits.d)
    cat("\n... for the last 5 rows of sorted data ...\n\n")
    print(out[(n.keep-4):n.keep,], digits=digits.d)
  }
  .dash(68)

    col.pts <- getOption("col.stroke.pt")
    col.line <- getOption("col.stroke.bar")

  if (pred && n.pred==1 && !is.factor(lm.out$model[,nm[2]]) && is.null(X1.new)) {

    if (!pdf) 
      if (res.rows > 0) dev.set(which=5) else dev.set(which=3)
    else { 
      pdf.file <- "LogitFitted.pdf"
      pdf(file=pdf.file, width=pdf.width, height=pdf.height)
    }

    fl <- "none"
    x.values <- lm.out$model[,nm[2]]
    y.values <- lm.out$model[,nm[1]] 
    .plt.main(x.values, y.values, by=NULL, type="p", n.cat=getOption("n.cat"),
       col.line=col.line, col.area=NULL, col.box="black",
       col.pts=col.pts, 
       col.fill=getOption("col.fill.pt"),
       col.stroke=getOption("col.stroke.pt"),
       col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),
       shape.pts=21, cex.axis=.85, col.axis="gray30",
       col.ticks="gray30", xy.ticks=TRUE,
       xlab=nm[2], ylab=nm[1], main="Scatterplot and Fitted Values",
       cex=.8,    
       x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
       time.start=NULL, time.by=NULL, time.reverse=FALSE, kind="default",
       fit.line=fl, col.fit.line="grey55", center.line=NULL,
       col.bubble=NULL, bubble.size=.25, col.flower=NULL,
       ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE,
       text.out=FALSE, ylim=c(0,1))

    lines(lm.out$model[,nm[2]], p.int$fit, col=col.pts, lwd=2)
  }

  else {  # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {

     if (!pdf) 
      if (res.rows > 0) dev.set(which=5) else dev.set(which=3)
    else { 
      pdf.file <- "LogitScatter.pdf"
      pdf(file=pdf.file, width=pdf.width, height=pdf.height)
    }

      suppressWarnings(pairs(lm.out$model[c(nm)], 
                            panel=panel.smooth, col=col.pts, col.smooth=col.line))
    }
    else {
      cat("\n\n>>> No scatterplot matrix reported because not all variables are ")
      if (!in.data.frame) cat("in the data frame.\n")
      if (!numeric.all) cat("numeric.\n")
      dev.off()
    }
  }


}
