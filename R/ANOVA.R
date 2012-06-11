ANOVA <-
function(my.formula, dframe=mydata, 
         brief=FALSE, digits.d=4, ...) {
  
  mydframe <- deparse(substitute(dframe))  # get data frame name for cor before sort
 
  op <- options()  # save current options to reset at end
  options(show.signif.stars=FALSE, scipen=30)

  if (!exists(mydframe)) {
    txtC <- "Function ANOVA requires the data exist in a data frame\n"
    if (mydframe == "mydata") 
      txtA <- ", the default data frame name, " else txtA <- " "
    txtB1 <- "Either create the data frame, such as with data.frame function, or\n"
    txtB2 <- "  specify the actual data frame with the parameter: dframe\n"
    txtB <- paste(txtB1, txtB2, sep="")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtC, "Data frame ", mydframe, txtA, "does not exist\n\n", txtB, "\n")
  }

  nm <- all.vars(my.formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(dframe)

  options(xname = nm[1])
  options(yname = nm[2])
  
  # sort values of the one predictor variable for scatterplot
  #   so that the prediction/confidence intervals can be drawn
  if (n.pred == 1) { 
    o <- order(dframe[,nm[2]], decreasing=FALSE)
    dframe <- dframe[o,]
  }

  in.data.frame <- TRUE
  for (i in 1:n.vars) {
    if (!(nm[i] %in% names(dframe))) {
        cat("\n\n\n>>> Note: ", nm[i], "is not in the data frame.\n")
        in.data.frame <- FALSE
      }
  }

  for (i in 2:n.vars) {
      if (in.data.frame && !is.factor(dframe[ ,which(names(dframe) == nm[i])])) {
        cat("\n>>> Note: Converting", nm[i], "to a factor for this analysis only.\n")
        nms <-  which(names(dframe) == nm[i])
        dframe[ ,nms] <- as.factor(dframe[ ,nms])
      }
    }  

  # ANOVA
  #   all analysis done on data in model construct av.out$model
  #   this model construct contains only model vars, with Y listed first
  av.out <<- aov(my.formula, data=dframe)

  n.keep <- nrow(av.out$model)
    

# ----------
# Background
# ----------

  cat( "\n\n\n", "  BACKGROUND", "\n")

  cat("\n")
  if (sys.nframe() == 1) {  # only accurate if not called from model
    cat("Data Frame: ", mydframe, "\n\n")
  }
  
  for (i in 1:n.vars) {
    ind <- i
    .varlist(n.pred, ind, nm[i], levels(dframe[,nm[i]]))
  }
  
  cat("\nNumber of observations (rows) of data: ", n.obs, "\n")
  cat("Number of observations retained for analysis: ", n.keep, "\n")


# --------------
# Basic Analysis
# --------------
  if (!brief) {
    cat( "\n\n\n", "  BASIC ANALYSIS", "\n")

    if (n.pred == 1) {
      x.values <- av.out$model[,nm[2]]
      y.values <- av.out$model[,nm[1]] 
      .ss.numeric(y.values, by=x.values, dframe=dframe, digits.d=digits.d, brief=TRUE)
    }

    if (n.pred == 2) {
      cat("\nTable of Cell Sample Sizes\n")
      .dash(26)
      l <-  tapply(av.out$model[,nm[1]], 
            list(av.out$model[,nm[2]], av.out$model[,nm[3]] ), length)
      l <- as.table(l)
      names(dimnames(l)) <- c(nm[2], nm[3])
      print(t(l))  # first treatment horizontal dimension

      cat("\nTable of Cell Means\n")
      .dash(19)
      m <-  tapply(av.out$model[,nm[1]], 
            list(av.out$model[,nm[2]], av.out$model[,nm[3]] ), mean, na.rm=TRUE)
      m <- as.table(m)
      names(dimnames(m)) <- c(nm[2], nm[3])
      print(t(m))  # first treatment horizontal dimension

      cat("\nTable of Cell Standard Deviations\n")
      .dash(33)
      s <-  tapply(av.out$model[,nm[1]], 
            list(av.out$model[,nm[2]], av.out$model[,nm[3]] ), sd, na.rm=TRUE)
      s <- as.table(s)
      names(dimnames(s)) <- c(nm[2], nm[3])
      print(round(t(s), digits.d))

    }
  }

  cat("\n\n")
  cat("ANOVA\n")
  .dash(5)
  sm <- summary(av.out)
  print(sm)

  if (n.pred== 1) {
    cat("\n\n")
    HSD <- TukeyHSD(av.out, which=nm[2])
    cat("Tukey Multiple Comparisons of Means\n")
    cat("Familywise Confidence Level:", attr(HSD, which="conf.level"), "\n")
    .dash(35)
    cat("\n")
    print(HSD[[1]])
    if (!brief) {
      .graphwin()
      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))
      par(mar=c(5.1,6.1,4.1,1.5))
      cex.axis <- .8; col.axis <- "gray30"; col.ticks <- "gray30"
      suppressWarnings(plot(HSD, 
        cex.axis=cex.axis, col.axis=col.axis, col.ticks=col.ticks, las=1))
    }
  }

  if (n.pred == 2  &&  !brief) {
  .graphwin()
  interaction.plot(av.out$model[,nm[2]], av.out$model[,nm[3]], 
           av.out$model[,nm[1]],
           xlab=nm[2], ylab=nm[1], trace.label=nm[3])
  }

# pairwise.t.test(mydata$Steady, mydata$TrtA)
# power.anova.test(groups=4, n=8, between.var=16.33, within.var=2.179)

  options(op)  # restore options going into reg

  cat("\n")

}
