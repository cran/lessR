corReflect <- 
function (x=mycor, vars,
          main=NULL, heat.map=TRUE, bottom=3,right=3, 
          pdf.file=NULL, pdf.width=5, pdf.height=5) {


  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

  # translate variable names into column positions
  vars.all <- as.list(seq_along(as.data.frame(x)))
  names(vars.all) <- names(as.data.frame(x))
  vars.num <- eval(substitute(vars), vars.all, parent.frame())

  NVOld <- as.integer(nrow(x))
  NVC <- as.integer(length(vars.num))

  # re-order R matrix
  out <- .Fortran("rflt",
                  R=as.double(as.matrix(x)),
                  Label=as.integer(as.vector(vars.num)),
                  NVC=NVC,
                  NVOld=NVOld)

  # construct full R matrix, with all the original vars
  out$R <- matrix(out$R, nrow=NVOld, ncol=NVOld, byrow=TRUE)

  # assign names
  nm <- character(length=NVOld)
  nm <- dimnames(x)[[1]]
  dimnames(out$R) <- list(nm, nm)

  if (heat.map) {

    if (is.null(main)) main <- "With Reflected Item Coefficients"
   .corcolors(out$R, NVOld, main, bottom, right, diag=0,
              pdf.file, pdf.width, pdf.height)
  }

  # finish
  cat("\n")
  return(out$R)
}





