ttest.brief <-
function(x, y=NULL, dframe=mydata, brief=TRUE, ...) {

  if (!is.null(y)) {
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The form  ttest.brief  does not apply\n",
      "when calling two vectors.\n",
      "Instead use ttest(x, y, brief=TRUE).\n\n")
  }
  else

    ttest(x, y=NULL, dframe, brief=TRUE, ...)

}
