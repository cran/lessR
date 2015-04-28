corCFA <- 
function(x=mycor, data=mydata,

         labels=c("include", "exclude", "only"),

         iter=50, resid=TRUE, item.cor=TRUE, sort=TRUE,

         main=NULL, heat.map=TRUE, bottom=3, right=3, 

         pdf.file=NULL, pdf.width=5, pdf.height=5,

         F1=NULL, F2=NULL, F3=NULL, F4=NULL, F5=NULL,
         F6=NULL, F7=NULL, F8=NULL, F9=NULL, F10=NULL,
         F11=NULL, F12=NULL) {

  labels <- match.arg(labels)

  if (labels!="only") {
    cor.nm <- deparse(substitute(x))
    .cor.exists(cor.nm)  # see if matrix exists in one of the 3 locations
    if (class(x) == "out_all")
      x <- eval(parse(text=paste(cor.nm, "$cors", sep="")))  # go to $cors 
  }
  else  # only labels
    if (!exists("data", where=.GlobalEnv)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No data table (frame) exists from which to get the labels\n\n")
    }

  NFmax <- 12

  dname <- deparse(substitute(data))
  options(dname = dname)

  # translate variable names into column positions
  if (labels!="only") {
    NVOld <- as.integer(nrow(x))
    vars.all <- as.list(seq_along(as.data.frame(x)))
    names(vars.all) <- names(as.data.frame(x))
    nm <- dimnames(x)[[1]]
  }
  else {
    NVOld <- as.integer(nrow(data))
    vars.all <- as.list(seq_along(data))
    names(vars.all) <- names(data)
    nm <- names(data)
  }
    

  F1n <- eval(substitute(F1), vars.all, parent.frame())
  F2n <- eval(substitute(F2), vars.all, parent.frame())
  F3n <- eval(substitute(F3), vars.all, parent.frame())
  F4n <- eval(substitute(F4), vars.all, parent.frame())
  F5n <- eval(substitute(F5), vars.all, parent.frame())
  F6n <- eval(substitute(F6), vars.all, parent.frame())
  F7n <- eval(substitute(F7), vars.all, parent.frame())
  F8n <- eval(substitute(F8), vars.all, parent.frame())
  F9n <- eval(substitute(F9), vars.all, parent.frame())
  F10n <- eval(substitute(F10), vars.all, parent.frame())
  F11n <- eval(substitute(F11), vars.all, parent.frame())
  F12n <- eval(substitute(F12), vars.all, parent.frame())

  Label <- c(F1n,F2n,F3n,F4n,F5n,F6n,F7n,F8n,F9n,F10n,F11n,F12n)

  # get NF, number of factors
  NF <- 0
  for (i in 1:NFmax) {
    fnum <- eval(parse(text=paste("F", toString(i), "n", sep="")))
    if (!is.null(fnum)) NF <- NF + 1
  }

  if (NF == 0) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Number of Factors: ", NF, "\n",
      "Need to specify some factors.", "\n\n",
      "For example, F1=c(...), F2=c(...), etc.\n\n")
  }

  # get the ordinal position of the first and last vars in Group i
  # get NItems
  LblCut <- matrix(nrow=NF, ncol=2)
  NItems <- 0
  for (i in 1:NF) {
    LblCut[i,1] <- NItems + 1
    cFac <- eval(parse(text=paste("F", toString(i), "n", sep="")))
    if (length(cFac) == 0) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Factor Number ", i, " has no items.\n",
          "Each factor must have at least one item.\n\n")
    }
    NItems <- NItems + length(cFac)
    LblCut[i,2] <- NItems
  }

  for (i in 1:NItems) {
    if (Label[i] > NVOld) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Number of items in correlation matrix: ", NVOld, "\n",
        "Item number in Factor specification: ", Label[i], "\n\n",
        "Specified item does not exist in this correlation matrix.\n\n")
    }
  }

  # display labels by factor
  if (labels == "only") {
    cat("\n")
    nmF <- character(length=NF)
    for (i in 1:NF) nmF[i] <- paste("F", toString(i), sep="")

    for (i in 1:NF) {
      cat(nmF[i], ": ", sep="")
      for (j in LblCut[i,1]:LblCut[i,2]) cat(" ",nm[Label[j]])
      cat("\n")

      for (j in LblCut[i,1]:LblCut[i,2]) {
        options(xname = nm[Label[j]])
        gl <- .getlabels()
        x.lbl <- gl$xl 
        if (!is.null(x.lbl)) {
          if (j == LblCut[i,1]) .dash(30)
          cat(nm[Label[j]], ": ", x.lbl, "\n", sep="")
          if (j == LblCut[i,2]) cat("\n")
        }
      }
    }
    cat("\n\n")
  }  # end labels only


  else { # proceed with the analysis

  # --------------------------------------------------------
  # re-order R matrix

   outR <- x[Label,Label]

  nm.new <- colnames(outR)

  # get width of largest variable label
  cc <- as.character(dimnames(outR)[[1]])
  max.chr <- 0
  for (i in 1:NItems)  
    if (nchar(cc[i]) > max.chr) max.chr <- nchar(cc[i])
  if (max.chr < 4) max.chr <- 4


  # --------------------------------------------------------
  # MIMM CFA

  # expand R matrix to include rows/cols for factors
  rr <- matrix(rep(0, NF*NItems), nrow=NF)
  cc <- matrix(rep(0, NF*(NItems+NF)), nrow=(NItems+NF))
  outR <- cbind(rbind(outR,rr),cc)

  alpha <- double(length=NF)
  omega <- double(length=NF)

  out <- .mimm(outR, LblCut, NItems, NF, iter)

  nmF <- character(length=NF)
  for (i in 1:NF) nmF[i] <- paste("F", toString(i), sep="")

  NVTot <- NItems + NF

  # assign names
  nm  <- character(length=NVTot)
  nm <- c(nm.new, nmF)
  dimnames(out$R) <- list(nm, nm)


  # --------------------------------------------------------
  # Sort within each group by the group factor loading

  if (sort) {

    # get new ordering, factor by factor
    pt <- numeric(length=NItems)
    newLabel <- numeric(length=NItems)
    for (ifac in 1:NF) {
      n1 <- LblCut[ifac,1]
      n2 <- LblCut[ifac,2]
      irow <- NItems + ifac
      for (j in n1:n2) pt[j] <- out$R[irow, j]
      o <- order(pt[n1:n2], decreasing=TRUE)
      for (i in 1:(n2-n1+1)) newLabel[n1-1+i] <- Label[n1-1+o[i]]
    }
    Label <- newLabel

    outR <- x[Label,Label]

    nm.new <- colnames(outR)

    # expand R matrix to include rows/cols for factors
    rr <- matrix(rep(0, NF*NItems), nrow=NF)
    cc <- matrix(rep(0, NF*(NItems+NF)), nrow=(NItems+NF))
    outR <- cbind(rbind(outR,rr),cc)

    # MIMM CFA
    alpha <- double(length=NF)
    omega <- double(length=NF)

    out <- .mimm(outR, LblCut, NItems, NF, iter)

    nmF <- character(length=NF)
    for (i in 1:NF) nmF[i] <- paste("F", toString(i), sep="")

    # assign names
    nm  <- character(length=NVTot)
    nm <- c(nm.new, nmF)
    dimnames(out$R) <- list(nm, nm)
  }

  # --------------------------------------------------------
  if (heat.map) {

    if (is.null(main)) main <- "Item Correlations/Communalities"
   .corcolors(out$R, NItems, main, bottom, right, diag=NULL,
              pdf.file, pdf.width, pdf.height)
  }


  # --------------------------------------------------------
  # Output labels, scale reliabilities

  cat("\n")
  cat('Factor / Scale Composition\n',
      '-------------------------------------------------------------------\n',
      'Each set of items forms a scale, scored as an unweighted composite.\n',
      'Corresponding to each observed scale score is an underlying factor.\n',
      '-------------------------------------------------------------------\n\n',
      sep="")

  for (i in 1:NF) {
    cat(nmF[i], ": ", sep="")
    for (j in LblCut[i,1]:LblCut[i,2]) cat(" ", nm.new[j])
    cat("\n")

  if (labels == "include") {
      for (j in LblCut[i,1]:LblCut[i,2]) {
        options(xname = nm.new[j])
        gl <- .getlabels()
        x.lbl <- gl$xl 
        if (!is.null(x.lbl)) {
          if (j == LblCut[i,1]) .dash(30)
          cat(nm.new[j], ": ", x.lbl, "\n", sep="")
          if (j == LblCut[i,2]) cat("\n")
        }
      }
    }
    if (i == NF) cat("\n\n")
  }

  cat('Reliability Analysis\n',
      '---------------------------------------------------------------------\n',
      'Reliability of the composite, unweighted total score, for each scale.\n',
      sep="")
  if (iter > 0)
    cat('Alpha assumes equal item reliabilities. The more generally preferred\n', 
        'Omega uses each item\'s communality in the computation of reliability.\n',
        sep="")
  cat('---------------------------------------------------------------------\n\n',
      sep="")

  if (iter > 0) 
    cat(' Scale  Alpha  Omega\n',
        ' -------------------\n', sep="")
  else
    cat(' Scale  Alpha\n',
        ' ------------\n', sep="")
  for (i in 1:NF) {
    cat("  ", nmF[i], " ", .fmt(out$Alpha[i],3)) 
    if (iter > 0)
      cat(                  " ", .fmt(out$Omega[i],3), "\n")
    else {
      out$Omega <- NULL
      cat("\n")
    }
  }
  cat("\n\n")



  # --------------------------------------------------------
  # Indicator analysis

  if (iter > 0 ) {
    MaxBad <- 25
    MaxLbl <- NItems
    Bad <- integer(length=NItems)

    buf <- max.chr - 4
    if (buf < 0) buf <- 0

    cat('Indicator Analysis\n')
    .dash(75)
    cat('Fac', ' Indi', .fmtc(" ",buf+1), 'Pat', '    Unique',
       ' Factors with which an indicator correlates too\n')
    cat('tor', ' cator', .fmtc("",buf), 'tern', '   ness',
        '   highly, and other indicator diagnostics.\n')
    .dash(75)

    for (IFac in 1:NF) {
      cat("\n")
      Fnm <- paste("F", as.character(IFac), sep="")

      for (Item in LblCut[IFac,1]:LblCut[IFac,2]) {
        Lam <- out$R[NItems+IFac,Item]
        Unique <- 1 - Lam**2

        if (Lam>0 && Unique>0) {
          NBad <- 0
          for (I in 1:NF) {
            if (abs(out$R[NItems+I,Item]) > Lam) {
              NBad <- NBad + 1
              if (NBad <= MaxBad) Bad[NBad] <- I
            }
          }
          if (NBad > MaxBad) NBad <- MaxBad
          cat(.fmtc(Fnm,3), .fmtc(nm.new[Item],max.chr), .fmt(Lam,3,7),
              .fmt(Unique,3,7))
          cat("    ")
          if (NBad > 0) for (IBad in 1:NBad) cat(paste("F",Bad[IBad]," ",sep=""))
        }

        else if (Lam <= 0)
          cat(.fmtc(Fnm,3), .fmtc(nm.new[Item],4), .fmt(Lam,3,7), '   xxxx',
           '   ** Negative Loading on Own Factor **')

        else if (Unique <= 0) {
          if (LblCut[IFac,2]-LblCut[IFac,1] > 0)
            cat(.fmtc(Fnm,3), .fmtc(nm.new[Item],4), .fmt(Lam,3,7),
                .fmt(Unique,3,7), '   ** Improper Loading **')
          else
            cat(.fmtc(Fnm,3), .fmtc(nm.new[Item],4), .fmt(Lam,3,7),
                .fmt(Unique,3,7), '   ** Factor Defined by Only One Item **')
        }

        cat("\n")
        Bad <- rep(0, NItems)
      }  # each item within a factor

    }  # each factor
    cat("\n\n")
  }


  # --------------------------------------------------------
  # Solution

  if (iter > 0) {
    cat('Latent Variable (factor) ')
    if (item.cor) cat('/ Observed Variable (item) ')
    cat('Correlations\n')
    .dash(65)
    if (item.cor)
      cat('Item Correlation: Correlation of two items with each other\n',
          'Communality, in the diagonal of the item correlations: Proportion\n',
          '  of the correlation of an item with itself that is due only to\n',
          '  its underlying factor\n', sep="")
    cat('Factor Loading: Correlation of an item with a factor\n',
        'Pattern Coefficient: Regression coefficient of an item on its,\n',
        '  underlying factor, a special case of a factor loading\n',
        'Factor Correlation: Correlation of two factors with each other\n', sep="")
    .dash(65)
    cat("\n")

  }
 
  else {
    cat("Item-Scale and Scale-Scale Correlations\n",
        "---------------------------------------\n", sep="")
  }

    # print the solution
    if (item.cor)
      print(round(out$R,2))
    else
      print(round(out$R[1:NVTot,(NItems+1):NVTot],2))


  # --------------------------------------------------------
  if (resid) {

    res <- .resid(out$R, LblCut, NItems, NF)

    # take just 1st NItems vars
    res <- res[1:NItems,1:NItems]

    cat("\n\n")

    cat('Residuals\n',
        '--------------------------------------------------------------\n',
        'Difference between an item correlation and its value imposed\n',
        'by the estimated multiple indicator measurement model.\n',
        '--------------------------------------------------------------\n\n',
        sep="")

    # sum of squares, sum of abs

    cat("Residual summaries\n",
        "------------------\n\n", sep="")

    cat(.fmtc(" ", max.chr+2), "Sum of    Average", "\n",
        .fmtc(" ", max.chr+2), "Squares   Abs Value", "\n",
        .fmtc(" ", max.chr+2), "-------   ---------", "\n", sep="")

    cc <- as.character(dimnames(res)[[1]])
    res.avg <- double(length=NItems)

    ssq.tot <- 0
    abv.tot <- 0
    abv.all <- 0
    for (i in 1:NItems) {
      ssq <- 0
      abv <- 0
      for (j in 1:NItems) {
        ssq <- ssq + res[i,j]^2
        abv <- abv + abs(res[i,j])
        abv.all <- abv.all + abs(res[i,j])
      }
      ssq.tot <- ssq.tot + ssq
      res.avg[i] <- abv / (NItems - 1)
      cat(.fmtc(cc[i],max.chr), "  ", .fmt(ssq,3), "  ", .fmt(res.avg[i],3), "\n")
    }
    abv.all.tot <- abv.all / (NItems^2 - NItems)
    cat("\n")
    cat("Total sum of squares for all items:", .fmt(ssq.tot,3), "\n")
    cat("Average absolute residual w/o the diagonal:", .fmt(abv.all.tot,3), "\n\n\n")

    cat("Item residuals\n",
        "--------------\n\n", sep="")

    print(round(res, 2))
  }
  else  # no residuals
    res <- list(R = NULL)


  # --------------------------------------------------------
  # construct lavaan model

  if (iter > 0) {
    cat("\n\n")
    cat("lavaan code for confirmatory factor analysis of the model,\n",
        "fully standardized, maximum likelihood solution\n",
        "----------------------------------------------------------\n", sep="")

    cat("\n")

    cat("library(lavaan)\n")
    cat("MeasModel <-\n")

    for (i in 1:NF) {
      if (i == 1) {
        cat("\"")
        cat("  ", nmF[i], " =~", sep="")
      }
      else {
        cat("   ")
        cat(nmF[i], " =~", sep="")
      }
      for (j in LblCut[i,1]:LblCut[i,2]) {
        if (j == LblCut[i,1])
          cat(" ", nm.new[j], sep="")
        else  
          cat(" +", nm.new[j])
      }
      cat("\n")
    }
    #for (i in 1:NF)
      #cat("   ", nmF[i], " ~~ 1 * ", nmF[i], "\n", sep="")
    cat("\"\n")

    cat("fit <- cfa(MeasModel, data=mydata, std.ov=TRUE, std.lv=TRUE)\n")
    cat("summary(fit, fit.measures=TRUE)\n")
    cat("\n\n")

    cat("--------\n")
    cat(">>> The preceding code fits the model from data frame:  mydata\n")
    cat(">>> To access the correlation matrix directly without the data\n")
    cat(">>> use the following fit statement instead.\n")
    cat("fit <- cfa(MeasModel, sample.cov=mycor, sample.nobs=nnn, std.lv=TRUE)\n")
    cat(">>>   mycor: name of correlation matrix\n")
    cat(">>>   nnn: numeric, number of observations\n")
    cat("\n")
  }


  # --------------------------------------------------------
  # return

  invisible(list(
     ff.cor=out$R[(NItems+1):NVTot,(NItems+1):NVTot],
     if.cor=out$R[1:NItems,(NItems+1):NVTot],
     diag.cor=diag(out$R[1:NItems,1:NItems]),
     alpha=out$Alpha,
     omega=out$Omega,
     resid=res
  ))

  }
}

