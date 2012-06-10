.reg3Residual <-
function(nm, mydframe,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         res.sort, res.rows, cooks.cut, colors, graphics.save) {

# -----------------
# residual analysis
# -----------------
  
    cat( "\n\n\n", "  ANALYSIS OF RESIDUALS AND INFLUENCE", "\n")
  
    if (show.R) {
      cat(line, sep="")
      cat(pre, "fitted(model)", sep="", "\n")
      cat(pre, "resid(model)", sep="", "\n")
      cat(pre, "rstudent(model)", sep="", "\n")
      cat(pre, "dffits(model)", sep="", "\n")
      cat(pre, "cooks.distance(model)", sep="", "\n")
      cat(line, "\n")
    }
    else cat("\n")
    
    if (explain) {
      .dash(68)
      cat("The identification of observations that have a large residual\n",
          "and/or undue influence on the estimation of the model helps\n",
          "detect potential outliers.  Each of the following statistics is\n",
          "calculated for a single observation (row of data).\n",
         "\n",          
          "residual: Value of the response variable ", nm[1], " minus its\n",
          "    fitted value.\n",
          "\n",
          "rstudent: Studentized residual, standardized value of the residual\n",
          "    from a model estimated without the observation present.\n",
          "\n",
          "dffits: The influence of an observation on its own fitted value.\n",
         "\n",
          "cooks: Cook's Distance, the aggregate influence of the observation\n",
          "    on the estimation of the model coefficients.\n", sep="")
      .dash(68)
      cat("\n")
    }

    cat("Data, Fitted, Residual, Studentized Residual, Dffits, Cook's Distance\n")
    if (res.sort == "cooks") cat("   [sorted by Cook's Distance]\n")
    if (res.sort == "rstudent")  
      cat("   [sorted by Studentized Residual, ignoring + or - sign]\n")
   if (res.sort == "dffits")  
      cat("   [sorted by dffits, ignoring + or - sign]\n")
    txt <- "observations (rows) of data]"
    cat("   [res.rows = ", res.rows, " out of ", n.keep, " ", txt, sep="", "\n")
    .dash(68)

    fit <- fitted(lm.out)
    res <- residuals(lm.out)
    cook <- cooks.distance(lm.out)
    
    out <- cbind(fit, res, rstudent(lm.out), dffits(lm.out), cook)
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],out)
    out <- data.frame(out)
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "residual"
    names(out)[n.vars+3] <- "rstudent"
    names(out)[n.vars+4] <- "dffits"
    names(out)[n.vars+5] <- "cooks"
    if (res.sort != "off") {
      if (res.sort == "cooks") o <- order(out$cooks, decreasing=TRUE)
      if (res.sort == "rstudent") o <- order(abs(out$rstudent),
        decreasing=TRUE)
      if (res.sort == "dffits") o <- order(abs(out$dffits),
        decreasing=TRUE)
      out <- out[o,]
    }
    print(out[1:res.rows,], digits=digits.d)
    rm(out)
  
  
    # frequency distribution of residuals
    dev.set(which=3)
    .den.main(res, main="Evaluate Normality of Residuals", 
       xlab="Residuals", text.out=FALSE, colors=colors,
       bw="nrd0", type="both",
       bin.start=NULL, bin.width=NULL,
       col.bg=NULL, col.grid=NULL, col.bars=NULL,
       col.nrm="black", col.gen="black",
       col.fill.nrm=NULL, col.fill.gen=NULL,
       cex.axis=.85, col.axis="gray30", col.ticks="gray30",
       x.pt=NULL, y.axis=FALSE, 
       x.min=NULL, x.max=NULL, band=FALSE)

    # plot of residuals vs fitted
    max.cook <- max(cook, na.rm=TRUE)
    if (max.cook < cooks.cut) {
      cooks.cut <- floor(max.cook*100)/100
      txt <- paste("The point with the largest Cook's Distance, ", round(max.cook,2), 
        ", is highlighted", sep="")
    }
    else
      txt <- paste("Points with Cook's Distance >", cooks.cut, "are highlighted")
    dev.set(which=4)
    ord <- order(fit)
    fit.ord <- fit[ord]
    res.ord <- res[ord]
    .plt.main(fit.ord, res.ord, type="p", text.out=FALSE,
        main="Residuals vs Fitted Values", xlab="Fitted Values",
        ylab="Residuals", sub=txt, cex.sub=.8, colors=colors,
         col.line=NULL, col.area=NULL, col.box="black",
         col.pts=NULL, col.fill=NULL, trans.pts=NULL,
         pch=NULL, col.grid=NULL, col.bg=NULL,
         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE,
         fit.line="none", center.line=NULL, cex=NULL, 
         time.start=NULL, time.by=NULL, time.reverse=FALSE,
         col.bubble=NULL, bubble.size=.25, col.flower=NULL,
         ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE,
         ncut=4, kind="default")
    abline(h=0, lty="dotted", col="gray70")
    if (colors == "gray") col.ftln <- "gray30" else col.ftln <- "plum"
    lines(lowess(fit.ord, res.ord, f=.9), col=col.ftln)
    res.c <- res[which(cook>=cooks.cut)]
    fit.c <- fit[which(cook>=cooks.cut)]
    if (length(fit.c) > 0) {
      if (colors == "gray") col.out <- "black" else col.out <- "red"
      points(fit.c, res.c, col=col.out, pch=19)
      text(fit.c, res.c, names(fit.c), pos=1, cex=.8)
    }

    rm(fit, res, cook, res.c, fit.c, fit.ord, res.ord)

}
