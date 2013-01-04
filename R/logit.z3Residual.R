.logit3Residual <-
function(lm.out, nm, mydata,
         n.vars, n.pred, n.obs, n.keep, digits.d, pre, line,
         res.sort, res.rows, cooks.cut,
         pdf, pdf.width, pdf.height) {
  
    cat( "\n\n\n", "  ANALYSIS OF RESIDUALS AND INFLUENCE", "\n")

    cat("Data, Fitted, Residual, Studentized Residual, Dffits, Cook's Distance\n")
    if (res.sort == "cooks") cat("   [sorted by Cook's Distance]\n")
    if (res.sort == "rstudent")  
      cat("   [sorted by Studentized Residual, ignoring + or - sign]\n")
   if (res.sort == "dffits")  
      cat("   [sorted by dffits, ignoring + or - sign]\n")
    txt <- "cases (rows) of data]"
    cat("   [res.rows = ", res.rows, " out of ", n.keep, " ", txt, sep="", "\n")
    .dash(68)

    fit <- fitted(lm.out)
    res <- residuals(lm.out, type="response")
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
    if (!pdf) 
      dev.set(which=3)
    else {
      pdf.file <- "LogitResiduals.pdf"
      pdf(file=pdf.file, width=pdf.width, height=pdf.height)
    }

    .den.main(res, main="Evaluate Normality of Residuals", 
       xlab="Residuals", quiet=TRUE, 
       bw="nrd0", type="both",
       bin.start=NULL, bin.width=NULL,
       col.fill=getOption("col.fill.pt"),
       col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),
       col.nrm="black", col.gen="black",
       col.fill.nrm="transparent", col.fill.gen="transparent",
       cex.axis=.85, col.axis="gray30", col.ticks="gray30",
       x.pt=NULL, y.axis=FALSE, 
       x.min=NULL, x.max=NULL, band=FALSE)

    if (pdf) {
      dev.off()
      .showfile(pdf.file, "residuals plot")
    }


    # plot of residuals vs fitted
    max.cook <- max(cook, na.rm=TRUE)
    if (max.cook < cooks.cut) {
      cooks.cut <- floor(max.cook*100)/100
      txt <- paste("Largest Cook's Distance, ", .fmt(max.cook,2), 
        ", is highlighted", sep="")
    }
    else
      txt <- paste("Points with Cook's Distance >", cooks.cut, "are highlighted")

    if (!pdf) 
      dev.set(which=4) 
    else { 
      pdf.file <- "LogitResidFitted.pdf"
      pdf(file=pdf.file, width=pdf.width, height=pdf.height)
    }

    ord <- order(fit)
    fit.ord <- fit[ord]
    res.ord <- res[ord]
    .plt.main(fit.ord, res.ord, by=NULL, type="p", quiet=TRUE,
        main="Residuals vs Fitted Values", xlab="Fitted Values",
        ylab="Residuals", sub=txt, cex.sub=.8,
        col.area=NULL, col.box="black",
        col.stroke=getOption("col.stroke.pt"), 
        col.fill=getOption("col.fill.pt"),
        shape.pts=21, col.bg=getOption("col.bg"),
        col.grid=getOption("col.grid"),
        cex.axis=.85, col.axis="gray30",
        col.ticks="gray30", xy.ticks=TRUE,
        fit.line="none", center.line=NULL, cex=NULL, 
        col.bubble=NULL, bubble.size=.25, col.flower=NULL,
        ellipse=FALSE, n.cat=getOption("n.cat"), kind="default")
    abline(h=0, lty="dotted", col="gray70")
    if (getOption("colors") == "gray") col.ftln <- "gray30" else col.ftln <- "plum"
    lines(lowess(fit.ord, res.ord, f=.9), col=col.ftln)
    res.c <- res[which(cook>=cooks.cut)]
    fit.c <- fit[which(cook>=cooks.cut)]
    if (length(fit.c) > 0) {
      if (getOption("colors") == "gray") col.out <- "black" else col.out <- "red"
      points(fit.c, res.c, col=col.out, pch=19)
      text(fit.c, res.c, names(fit.c), pos=1, cex=.8)
    }

    if (pdf) {
      dev.off()
      .showfile(pdf.file, "residuals vs. fitted plot")
    }

    rm(fit, res, cook, res.c, fit.c, fit.ord, res.ord)

}
