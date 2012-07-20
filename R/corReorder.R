corReorder <-
function (x=mycor, vars=NULL, first=0,
          heat.map=TRUE, main=NULL, bottom=3,right=3,
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

  if (is.null(vars)) {
    out <- .Fortran("ordr",
                    R=as.double(as.matrix(x)),
                    Label=integer(length=NVOld),
                    NVC=as.integer(NVOld),
                    IFirst=as.integer(first))
    vars <- out$Label
  }

  NVC <- as.integer(length(vars))

  # re-order R matrix
  out <- .Fortran("rrdr",
                  R=as.double(as.matrix(x)),
                  Label=as.integer(as.vector(vars)),
                  NVC=NVC,
                  NVOld=as.integer(NVOld))

  # construct full R matrix, with all the original vars
  out$R <- matrix(out$R, nrow=NVOld, ncol=NVOld, byrow=TRUE)

  # if some vars deleted, take just 1st NVC vars
  if (NVC < NVOld) out$R <- out$R[1:NVC,1:NVC]

  # assign names
  nm <- character(length=NVOld)
  nm.new <- character(length=NVC)
  nm <- dimnames(x)[[1]]
  for (i in 1:NVC) nm.new[i] <- nm[vars[i]]
  dimnames(out$R) <- list(nm.new, nm.new)

  if (heat.map) {

    if (missing(colors)) 
      colors <- getOption("colors")
    else
      colors <- match.arg(colors)

   if (is.null(main)) main <- "Reordered Item Coefficients"
   .corcolors(out$R, NVC, colors, main, bottom, right, diag=0,
              pdf.file, pdf.width, pdf.height)
  }

  # finish
  cat("\n")
  return(out$R)
}





