.plt.shapes <-
function(shape, out_shape, n.by=NULL) {

  # there is a level if a by variable

  bad.shape <- NULL

  shapes.nm <- c("circle", "square", "diamond", "triup", "tridown")
  shapes.pnt <- c("+", "*", "#", "%", "!", "=", "-", "&", "$",
                 "?", "|", ">", "<", "@", ".", "/", ";", ":")
  shapes.dgt <- as.character(0:9)
  shapes.all <- c(shapes.nm, 0:19, letters, LETTERS, shapes.pnt, shapes.dgt)

  if (!is.null(n.by)) {
    # shiny does not get here, shape is never missing, from Plot #771
    if (unique(n.by) > length(shapes.all)) {
      cat("\n")
      stop(call.=FALSE, "\n","------\n",
      "Only ", length(shapes.all), " unique shapes to display\n",
      "You specified a  by  variable with ", unique(n.by), " levels\n\n")
    }

    if (shape[1] == "vary") {
      more <- c(8, 7, 9, 10, 12:14, 11, letters, LETTERS, shapes.pnt)
      shapes.by <-  c("triup", "tridown", "circle", "square", "diamond", more)
      shape <- integer(length(n.by))
      shape <- shapes.by[1:n.by]
    }
  }

  # convert named shape to its numeric code
  num.flag <- FALSE
  for (i in 1:length(shape)) {
    if (!(shape[i] %in% shapes.all)) {
      bad.shape <- shape[i]
    }
    else if (shape[i] %in% shapes.nm) {
      shape[i] <- which(shape[i] == shapes.nm) + 20  # still char
      num.flag <- TRUE
    }
    else if (shape[i] %in% LETTERS) {
      shape[i] <- which(LETTERS == shape[i]) + 64
      num.flag <- TRUE
    }
    else if (shape[i] %in% letters) {
      shape[i] <- which(letters == shape[i]) + 96
      num.flag <- TRUE
    }
  }
  if (num.flag) {
    shape <- as.numeric(shape)
  }

  # convert named out_shape to its numeric code
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
