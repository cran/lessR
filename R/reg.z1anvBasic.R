.reg1anvBasic <-
function(lm.out, ancova, digits_d=NULL, show_R=FALSE) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L
  n.obs <- nrow(lm.out$model)
  d <- digits_d

  x.cat <- 0
  x.cont <- 0
  balance <- TRUE
  if (ancova) {
    if (is.numeric(lm.out$model[,nm[2]]) && is.factor(lm.out$model[,nm[3]])) {
      x.cat <- 3
      x.cont <- 2
    }
    if (is.numeric(lm.out$model[,nm[3]]) && is.factor(lm.out$model[,nm[2]])) {
      x.cat <- 2
      x.cont <- 3
    }
    lvl <- levels(lm.out$model[,x.cat])
    tbl <- table(lm.out$model[,x.cat])
#   n.cell <- tbl[1]
#   for (i in 2:length(lvl)) {
#     if (n.cell != tbl[i]) balance <- FALSE
#   } 
  }  # end ancova

  # ANOVA 
  smc <- anova(lm.out)
  n.terms <- nrow(smc) - 1
  nt1 <- n.terms + 1

# kable experimentation
# k <- kable(smc, digits=digits_d)
# k <- gsub("|", " ", k, fixed=TRUE)
# k[1] <- gsub("Pr(>F)", "p-value", k[1], fixed=TRUE)
# k[length(k)] <- gsub("NA", "  ", k[length(k)], fixed=TRUE)

  SSE <- smc[nt1,2]

  tx <- character(length=0)

  if (is.null(options()$knitr.in.progress)) {
  }

  tx[length(tx)+1] <- "-- Analysis of Variance"
  if (ancova) {
    tx[length(tx)] <-  paste(tx[length(tx)],
                            "from Type II Sums of Squares")
    lm2.out <- lm(lm.out$model[,1] ~ lm.out$model[,3] + lm.out$model[,2])
    smc[1,] <- anova(lm2.out)[2,]  # replace original first line of ANOVA table
  }
  tx[length(tx)+1] <- ""

  if (show_R) {
    tx[length(tx)+1] <- ""
    .dash2(68)
    tx[length(tx)+1] <- paste("> ","anova(model)", "\n",sep="")
    tx[length(tx)+1] <-.dash2(68)
  }

  # width of column 1
  max.c1 <- max(nchar("Model"), nchar(nm))
  for (i in 1:nt1) {
    c1 <- nchar(rownames(smc)[i])
    if (c1 > max.c1) max.c1 <- c1 
   }

  # width of data columns
  max.ln <- integer(length=4)
  for (i in 1:4) {
    ln.nm <- nchar(colnames(smc)[i])
    max.ln[i] <- ln.nm + 1
    for (j in 1:nrow(smc)) {
      xjc <- .fmt(smc[j,i], d=digits_d)
      if (nchar(xjc) > max.ln[i]) max.ln[i] <- nchar(xjc)
    }
    max.ln[i] <- max.ln[i] + 1L
    if (max.ln[i] < 9L) max.ln[i] <- 9L
  }

  # header
  df.lbl <- .fmtc("     df", max.ln[1]+1)
  SS.lbl <- .fmtc(" Sum Sq", max.ln[2]+1)
  MS.lbl <- .fmtc("Mean Sq", max.ln[3]+1)
  fv.lbl <- .fmtc("F-value", max.ln[4]+1)
  tx[length(tx)+1] <- paste(eval(format("", width=max.c1-5)), df.lbl, SS.lbl,
                             MS.lbl, fv.lbl, "   p-value", sep="")

  # predictors (terms, including interactions)
  if (n.pred > 1) {
    #for (i in 1:(n.pred)) {
    for (i in 1:n.terms) {
      rlb <- .fmtc(rownames(smc)[i], max.c1)
      df <- .fmti(smc[i,1], max.ln[1]-5)
      SS <- .fmt(smc[i,2], digits_d, max.ln[2])
      MS <- .fmt(smc[i,3], digits_d, max.ln[3])
      fv <- .fmt(smc[i,4], digits_d, max.ln[4])
      pv <- .fmt(smc[i,5], 3, 9)
      tx[length(tx)+1] <- paste(rlb, df, SS, MS, fv, pv)
    } 
  }
  if (n.pred > 1  &&  !ancova) tx[length(tx)+1] <- ""


  # Model term in ANOVA table (not applicable to Type II SS from ancova)
  mdl <- NA
  if (n.pred > 0  &&  !ancova) {
    rlb <- .fmtc("Model", max.c1, j="left")

    mod.df <- 0
    for (i in 1:n.terms) mod.df <- mod.df + smc[i,1]
    md <- .fmti(mod.df, max.ln[1]-5) 

    mod.ss <- 0 
    for (i in 1:n.terms) mod.ss <- mod.ss + smc[i,2]
    ms <- .fmt(mod.ss, digits_d, max.ln[2])

    mod.ms <- mod.ss/mod.df
    mm <- .fmt(mod.ms, digits_d, max.ln[3])

    mod.f <- mod.ms/smc[nt1, 3]
    mf <- .fmt(mod.f, digits_d, max.ln[4])

    mod.p <- pf(mod.f, mod.df, smc[nt1,1], lower.tail=FALSE)
    mp <- .fmt(mod.p, 3, 9) 

    tx[length(tx)+1] <- paste(rlb, md, ms, mm, mf, mp)
#   if (n.pred > 1) tx[length(tx)+1] <- ""

    mdl <- c(mod.df, mod.ss, mod.ms, mod.f, mod.p)
    names(mdl) <- c("df", "ss", "ms", "fvalue", "pvalue")
  }

  # Residuals
  rlb <- .fmtc(rownames(smc)[nt1], max.c1, j="left")
  df <- .fmti(smc[nt1,1], max.ln[1]-5)
  SS <- .fmt(smc[nt1,2], digits_d, max.ln[2])
  MS <- .fmt(smc[nt1,3], digits_d, max.ln[3])
  MSW <- smc[nt1,3]
  tx[length(tx)+1] <- paste(rlb, df, SS, MS) 

  rsd <- c(smc[nt1,1], smc[nt1,2], smc[nt1,3])
  names(rsd) <- c("df", "ss", "ms")

  # Total
  tot <- NA
  if (n.pred > 0  &&  !ancova) {
    rlb <- .fmtc(nm[1], max.c1, j="left")

    tot.df <- mod.df + smc[nt1,1]
    td <- .fmti(tot.df, max.ln[1]-5) 

    tot.ss <- mod.ss + smc[nt1,2]
    ts <- .fmt(tot.ss, digits_d, max.ln[2])

    tot.ms <- tot.ss/tot.df
    tm <- .fmt(tot.ms, digits_d, max.ln[3])

    tx[length(tx)+1] <- paste(rlb, td, ts, tm) 

    tot <- c(tot.df, tot.ss, tot.ms)
    names(tot) <- c("df", "ss", "ms")
  }

  return(list(tx=tx, mdl=mdl, rsd=rsd, tot=tot, MSW=MSW))

}
