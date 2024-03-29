.reg1bckBasic <-
function(lm.out, dname="d", digits_d=NULL, show_R=FALSE, n.obs, n.keep,
         transf) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L

# ----------
# Background
# ----------

  tx <- character(length=0)

  if(show_R) {
    cv <- paste(nm[1]," ~ ", sep="")
    cv <- paste(cv, nm[2], sep="")
    if (n.vars > 2) for (i in 3:n.vars) cv <- paste(cv, " + ", nm[i], "", sep="")
    tx[length(tx)+1] <- .dash2(68)
    tx[length(tx)+1] <- paste("> ", "lm.out <- lm(", cv, ")", sep="")
    tx[length(tx)+1] <- .dash2(68)
  }
  
  tx[length(tx)+1] <- paste("Data Frame: ", dname)  # not accurate from Model
  tx[length(tx)+1] <- ""

  for (i in 1:n.vars) {
    txbck <- .varlist2(n.pred, i, nm[i], "Predictor", n.obs, n.keep)
    for (j in 1:length(txbck)) tx[length(tx)+1] <- txbck[j] 
  }

  if (!is.null(transf)) {
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("Data are", transf)
  }
  
  return(list(out_background=tx, n.vars=n.vars, n.obs=n.obs, n.keep=n.keep))

}
