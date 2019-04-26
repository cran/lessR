corReflect <- 
function (R=mycor, vars,
          main=NULL, heat_map=TRUE, bottom=3,right=3, 
          pdf_file=NULL, width=5, height=5, ...) {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (length(grep(".", names(dots)[i], fixed=TRUE)) > 0) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  # cor matrix:  mycor as class out_all, mycor$R, or stand-alone matrix
  cor.nm <- deparse(substitute(R))
  .cor.exists(cor.nm)  # see if matrix exists in one of the 3 locations
  if (class(R) == "out_all")
    R <- eval(parse(text=paste(cor.nm, "$R", sep="")))  # go to $R 


  # translate variable names into column positions
  vars.all <- as.list(seq_along(as.data.frame(R)))
  names(vars.all) <- names(as.data.frame(R))
  vars.num <- eval(substitute(vars), vars.all, parent.frame())

  NVOld <- as.integer(nrow(R))
  NVC <- as.integer(length(vars.num))

  Label <- as.integer(as.vector(vars.num))

  for (LL in 1:NVC) {
    for (J in 1:NVOld) {
      if (Label[LL] != J) {
        R[J,Label[LL]] <- -R[J,Label[LL]]
        R[Label[LL],J] <- R[J,Label[LL]]
      }
    }
  }

  if (heat_map) {
    if (is.null(main)) main <- "With Reflected Item Coefficients"
   .corcolors(R, NVOld, main, bottom, right, diag=0,
              pdf_file, width, height)
  }

  cat("\n")
  invisible(R)
}
