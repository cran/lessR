set <-
function(colors=c("blue", "gray", "rose", "green", "gold", "red"),
         trans.pts=NULL, n.cat=4, width=120) {

  if (!is.null(trans.pts)) {
    if ( trans.pts<0 || trans.pts>1 ) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "The level of  trans.pts  is a proportion, between 0 and 1.\n\n")
    }
  }

  if (!missing(colors)) {
    colors <- match.arg(colors)
    options(colors=colors)
  }

  if (!missing(trans.pts)) options(trans.pts=trans.pts)

  if (!missing(n.cat)) options(n.cat=n.cat)

  if (!missing(width)) options(width=width)

}
