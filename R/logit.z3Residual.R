.logit3Residual <-
function(lm.out, nm, d,
         n.vars, n.pred, n.obs, n.keep, digits_d, pre, line,
         res_sort, res_rows, cooks_cut) {
  
    cat( "\n\n\n", "  ANALYSIS OF RESIDUALS AND INFLUENCE", "\n")

    cat("Data, Fitted, Residual, Studentized Residual, Dffits, Cook's Distance\n")
    if (res_sort == "cooks") cat("   [sorted by Cook's Distance]\n")
    if (res_sort == "rstudent")  
      cat("   [sorted by Studentized Residual, ignoring + or - sign]\n")
   if (res_sort == "dffits")  
      cat("   [sorted by dffits, ignoring + or - sign]\n")
    txt <- "cases (rows) of data]"
    cat("   [res_rows = ", res_rows, " out of ", n.keep, " ", txt, sep="", "\n")
    .dash(68)

    fit <- fitted(lm.out)
    res <- residuals(lm.out, type="response")
    cook <- cooks.distance(lm.out)
    
    out <- cbind(fit, res, rstudent(lm.out), dffits(lm.out), cook)
    out <- cbind(lm.out$model[c(nm[seq(2,n.vars)],nm[1])],out)
    out <- data.frame(out, stringsAsFactors=TRUE)
    names(out)[n.vars+1] <- "fitted"
    names(out)[n.vars+2] <- "residual"
    names(out)[n.vars+3] <- "rstudent"
    names(out)[n.vars+4] <- "dffits"
    names(out)[n.vars+5] <- "cooks"
    if (res_sort != "off") {
      if (res_sort == "cooks") o <- order(out$cooks, decreasing=TRUE)
      if (res_sort == "rstudent") o <- order(abs(out$rstudent),
        decreasing=TRUE)
      if (res_sort == "dffits") o <- order(abs(out$dffits),
        decreasing=TRUE)
      out <- out[o,]
    }
    print(out[1:res_rows,], digits=digits_d)
    rm(out)

}
