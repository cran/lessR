.plt.shapes <-
function(shape, out_shape) {

  bad.shape <- NULL

  shapes.nm <- c("circle", "square", "diamond", "triup", "tridown")
  shapes.all <- c(shapes.nm, 0:25, letters, LETTERS, "+", "*", "#",
                  "%", "!", "=", "-", "&", "$", "?", "|", ">", "<", "@", ".")

  num.flag <- FALSE
  for (i in 1:length(shape)) {
    if (!(shape[i] %in% shapes.all)) {
      bad.shape <- shape[i]
    }
    else if (shape[i] %in% shapes.nm) {
      shape[i] <- which(shape[i] == shapes.nm) + 20
      num.flag <- TRUE
    }
  }
  if (num.flag) shape <- as.numeric(shape)

  num.flag <- FALSE
  if (!(out_shape %in% shapes.all))
    bad.shape <- out_shape 
  else {
    if (out_shape %in% shapes.nm) {  # outlier point
      out_shape <- which(out_shape == shapes.nm) + 20
      num.flag <- TRUE
    }
  }
  if (num.flag) out_shape <- as.numeric(out_shape)

  if (!is.null(bad.shape)) {
    message("\nValid shapes")
    message("------------")
    for (j in 1:length(shapes.nm)) message(shapes.nm[j])
    message("all uppercase and lowercase letters")
    message("all digits")
    message("+ * # % ! = - & $ ? | < > @")
    cat("\n")
    stop(call.=FALSE, "\n","------\n",
    "Not a valid shape: ", bad.shape, "\n\n")
  }

  return(list(shape=shape, out_shape=out_shape))

}
