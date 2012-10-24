corProp <- 
function (x=mycor, 
          main=NULL, heat.map=TRUE, bottom=3, right=3, 
          pdf.file=NULL, pdf.width=5, pdf.height=5) {


  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

  NVOld <- as.integer(nrow(x))

  out <- .Fortran("prop",
                  R=as.double(as.matrix(x)),
                  Label=integer(length=NVOld),
                  NVC=NVOld,
                  Diagon=as.integer(0),
                  Power=as.integer(0))

  # construct full R matrix, with all the original vars
  out$R <- matrix(out$R, nrow=NVOld, ncol=NVOld, byrow=TRUE)

  # assign names
  nm <- character(length=NVOld)
  nm <- dimnames(x)[[1]]
  dimnames(out$R) <- list(nm, nm)

  if (heat.map) {

    if (is.null(main)) main <- "Item Proportionalities"
   .corcolors(out$R, NVOld, main, bottom, right, diag=0,
              pdf.file, pdf.width, pdf.height)
  }

  # finish
  cat("\n")
  return(round(out$R,2))

}
