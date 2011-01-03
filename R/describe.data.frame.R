describe.data.frame <-
function(x, ...)  {

  for (i in 1:ncol(x)) describe(x[,i], lbl=names(x)[i], ...)
  
}
