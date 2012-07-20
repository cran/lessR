corCFA <- 
function(x=mycor, iter=15, resid=TRUE, item.cor=TRUE, sort=TRUE,
         main=NULL, heat.map=TRUE, bottom=3, right=3, 
         colors=c("blue", "gray", "rose", "green", "gold", "red"),
         pdf.file=NULL, pdf.width=5, pdf.height=5,
         F1=NULL, F2=NULL, F3=NULL, F4=NULL, F5=NULL,
         F6=NULL, F7=NULL, F8=NULL, F9=NULL, F10=NULL,
         F11=NULL, F12=NULL) {

  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

  NVOld <- as.integer(nrow(x))

  NFmax <- 12

  # set Label equal to the item Labels in the analysis
  Label <- c(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12)

  # get NF, number of factors
  NF <- 0
  for (i in 1:NFmax) {
    fnum <- eval(parse(text=paste("F", toString(i),sep="")))
    if (!is.null(fnum)) NF <- NF + 1
  }

  # get the ordinal position of the first and last vars in Group i
  # get NItems
  LblCut <- matrix(nrow=NF, ncol=2)
  NItems <- 0
  for (i in 1:NF) {
    LblCut[i,1] <- NItems + 1
    cFac <- eval(parse(text=paste("F", toString(i),sep="")))
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


  # --------------------------------------------------------
  # re-order R matrix
  out <- .Fortran("rrdr",
                  R=as.double(as.matrix(x)),
                  Label=as.integer(as.vector(Label)),
                  NVC=as.integer(NItems),
                  NVOld=as.integer(NVOld))

  # construct full R matrix, with all the original vars
  out$R <- matrix(out$R, nrow=NVOld, ncol=NVOld, byrow=TRUE)

  # if some vars deleted, take just 1st NItems vars
  if (NItems < NVOld) out$R <- out$R[1:NItems,1:NItems]

  # assign names
  nm <- character(length=NVOld)
  nm.new <- character(length=NItems)
  nm <- dimnames(x)[[1]]
  for (i in 1:NItems) nm.new[i] <- nm[Label[i]]
  dimnames(out$R) <- list(nm.new, nm.new)


  # --------------------------------------------------------
  # expand R matrix to include rows/cols for factors
  rr <- matrix(rep(0, NF*NItems), nrow=NF)
  cc <- matrix(rep(0, NF*(NItems+NF)), nrow=(NItems+NF))
  out$R <- cbind(rbind(out$R,rr),cc)

  # MIMM CFA
  Alpha <- double(length=NF)
  Omega <- double(length=NF)

  out <- .Fortran("mimm",
                  R=as.double(as.matrix(out$R)),
                  LblCut=as.integer(as.matrix(LblCut)),
                  NVC=as.integer(NItems),
                  NF=as.integer(NF),
                  Iter=as.integer(iter),
                  Alpha=as.double(Alpha),
                  Omega=as.double(Omega))

  nmF <- character(length=NF)
  for (i in 1:NF) nmF[i] <- paste("F", toString(i), sep="")

  NVTot <- NItems + NF

  # construct full R matrix, with all the original vars
  out$R <- matrix(out$R, nrow=NVTot, ncol=NVTot, byrow=TRUE)

  # assign names
  nm  <- character(length=NVTot)
  nm <- c(nm.new, nmF)
  dimnames(out$R) <- list(nm, nm)

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
  cat("\n\n")

  cat('Reliability Analysis\n',
      '---------------------------------------------------------------------\n',
      'Reliability of the composite, unweighted total score, for each scale.\n',
      'Alpha assumes equal item reliabilities. The more generally preferred\n', 
      'Omega uses each item\'s communality as a reliability estimate.\n',
      '---------------------------------------------------------------------\n\n',
      sep="")

  cat(' Scale  Alpha   Omega\n',
      '--------------------\n')
  for (i in 1:NF)
    cat("  ", nmF[i], " ", .fmt(out$Alpha[i],3), 
                      " ", .fmt(out$Omega[i],3), "\n")
  cat("\n\n")


  # --------------------------------------------------------
  # Sort within each group by the group factor loading
  if (sort) {
    srt <- .Fortran("srt",
                    R=as.double(as.matrix(out$R)),
                    Label=as.integer(as.vector(Label)),
                    LblCut=as.integer(as.matrix(LblCut)),
                    NItems=as.integer(NItems),
                    NF=as.integer(NF))
    Label <- srt$Label

    # re-order R matrix
    out <- .Fortran("rrdr",
                    R=as.double(as.matrix(x)),
                    Label=as.integer(as.vector(Label)),
                    NVC=as.integer(NItems),
                    NVOld=as.integer(NVOld))

    # construct full R matrix, with all the original vars
    out$R <- matrix(out$R, nrow=NVOld, ncol=NVOld, byrow=TRUE)

    # if some vars deleted, take just 1st NItems vars
    if (NItems < NVOld) out$R <- out$R[1:NItems,1:NItems]

    # assign names
    nm <- character(length=NVOld)
    nm.new <- character(length=NItems)
    nm <- dimnames(x)[[1]]
    for (i in 1:NItems) nm.new[i] <- nm[Label[i]]
    dimnames(out$R) <- list(nm.new, nm.new)


    # expand R matrix to include rows/cols for factors
    rr <- matrix(rep(0, NF*NItems), nrow=NF)
    cc <- matrix(rep(0, NF*(NItems+NF)), nrow=(NItems+NF))
    out$R <- cbind(rbind(out$R,rr),cc)

    # MIMM CFA
    Alpha <- double(length=NF)
    Omega <- double(length=NF)

    out <- .Fortran("mimm",
                    R=as.double(as.matrix(out$R)),
                    LblCut=as.integer(as.matrix(LblCut)),
                    NVC=as.integer(NItems),
                    NF=as.integer(NF),
                    Iter=as.integer(iter),
                    Alpha=as.double(Alpha),
                    Omega=as.double(Omega))

    nmF <- character(length=NF)
    for (i in 1:NF) nmF[i] <- paste("F", toString(i), sep="")

    # construct full R matrix, with all the original vars
    out$R <- matrix(out$R, nrow=NVTot, ncol=NVTot, byrow=TRUE)

    # assign names
    nm  <- character(length=NVTot)
    nm <- c(nm.new, nmF)
    dimnames(out$R) <- list(nm, nm)
  }

  # --------------------------------------------------------
  if (heat.map) {

    if (missing(colors)) 
      colors <- getOption("colors")
    else
      colors <- match.arg(colors)

    if (is.null(main)) main <- "Item Correlations/Communalities"
   .corcolors(out$R, NItems, colors, main, bottom, right, diag=NULL,
              pdf.file, pdf.width, pdf.height)
  }


  # --------------------------------------------------------
  # Indicator analysis
  MaxBad <- 25
  MaxLbl <- NItems
  Bad <- integer(length=NItems)

  cat('Indicator Analysis\n')
  .dash(75)
  cat(' Fac', ' Indi', '  Pat', '   Unique',
     '  Factors with which an indicator correlates too\n')
  cat(' tor', ' cator', ' tern', '   ness',
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
        cat(.fmtc(Fnm,3), .fmti(Label[Item],4), .fmt(Lam,3,8), .fmt(Unique,3,8))
        cat("    ")
        if (NBad > 0) for (IBad in 1:NBad) cat(paste("F",Bad[IBad]," ",sep=""))
      }

      else if (Lam <= 0)
        cat(.fmtc(Fnm,3), .fmti(Label[Item],4), .fmt(Lam,3,8), '   xxxx ',
         '   ** Negative Loading on Own Factor **')

      else if (Unique <= 0) {
        if (LblCut[IFac,2]-LblCut[IFac,1] > 0)
          cat(.fmtc(Fnm,3), .fmti(Label[Item],4), .fmt(Lam,3,8), .fmt(Unique,3,8),
           '   ** Improper Loading **')
        else
          cat(.fmtc(Fnm,3), .fmti(Label[Item],4), .fmt(Lam,3,8), .fmt(Unique,3,8),
           '   ** Factor Defined by Only One Item **')
      }

      cat("\n")
      Bad <- rep(0, NItems)
    }  # each item within a factor

  }  # each factor
  cat("\n\n")


  # --------------------------------------------------------
  # Solution
  if (iter > 0) {
    cat('Latent Variable (factor) / Observed Variable (item) Correlations\n')
    .dash(69)
    if (item.cor)
      cat('Item Correlation, correlation of two items with each other\n',
          'Communalities in the diagonal of the item correlations, each the\n',
          '  variance of an item due to the common factor\n', sep="")
    cat('Factor Loading, correlations of an item with a factor\n',
        'Pattern Coefficient, regression coefficient of its factor on an item,\n',
        '  a special case of a factor loading\n',
        'Factor Correlation, correlation of two factors with each other\n', sep="")
    .dash(69)
    cat("\n")
  }


  # --------------------------------------------------------
  if (resid) {

    # print the MGRP solution (instead of rely upon return)
    if (item.cor)
      print(round(out$R,2))
    else
      print(round(out$R[1:NVTot,(NItems+1):NVTot],2))

    out <- .Fortran("resid",
                    R=as.double(as.matrix(out$R)),
                    LblCut=as.integer(as.matrix(LblCut)),
                    NItems=as.integer(NItems),
                    NF=as.integer(NF))

    # construct full R matrix, with all the original vars
    out$R <- matrix(out$R, nrow=NVTot, ncol=NVTot, byrow=TRUE)

    # take just 1st NItems vars
    out$R <- out$R[1:NItems,1:NItems]

    # assign names
    dimnames(out$R) <- list(nm.new, nm.new)

    cat("\n\n")

    cat('Residuals\n',
        '--------------------------------------------------------------\n',
        'Each residual is the difference between the corresponding item\n',
        'correlation and its value imposed by the estimated multiple\n', 
        'indicator measurement model.\n',
        '--------------------------------------------------------------\n\n',
        sep="")
  }


  # --------------------------------------------------------
    if (!resid)
      if (item.cor)
        return(round(out$R,2))
      else
        return(round(out$R[1:NVTot,(NItems+1):NVTot],2))
    else
      return(round(out$R,2))

}
