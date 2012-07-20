corReflect <- 
function (x=mycor, vars,
          main=NULL, heat.map=TRUE, bottom=3,right=3, 
          colors=c("blue", "gray", "rose", "green", "gold", "red"),
          pdf.file=NULL, pdf.width=5, pdf.height=5) {


  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

  NVOld <- as.integer(nrow(x))
  NVC <- as.integer(length(vars))

  # re-order R matrix
  out <- .Fortran("rflt",
                  R=as.double(as.matrix(x)),
                  Label=as.integer(as.vector(vars)),
                  NVC=NVC,
                  NVOld=NVOld)

  # construct full R matrix, with all the original vars
  out$R <- matrix(out$R, nrow=NVOld, ncol=NVOld, byrow=TRUE)

  # assign names
  nm <- character(length=NVOld)
  nm <- dimnames(x)[[1]]
  dimnames(out$R) <- list(nm, nm)

  if (heat.map) {

    if (missing(colors)) 
      colors <- getOption("colors")
    else
      colors <- match.arg(colors)

    if (is.null(main)) main <- "With Reflected Item Coefficients"
   .corcolors(out$R, NVOld, colors, main, bottom, right, diag=0,
              pdf.file, pdf.width, pdf.height)
  }

  # finish
  cat("\n")
  return(out$R)
}





