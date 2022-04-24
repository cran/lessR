reshape_long <-
  function(data, transform, group="Group", response="Value", ID="ID",
           prefix=ID, sep="") {

  # if a tibble convert to data frame
  df.name <- deparse(substitute(data))  # get name of data table
  dfs <- .getdfs()
  if (!is.null(dfs)) {
    if (df.name %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data)
      }
    }
  }

  if (is.null(ID)) ID <- "xxQ7q"

  data.vars <- as.list(seq_along(data))
  names(data.vars) <- names(data)
  ind <- eval(substitute(transform), envir=data.vars, parent.frame())
  trns <- names(data[,ind])

  dl <- reshape(data, direction="long", idvar=ID,
                  timevar=group, varying=trns,
                  v.names=response, times=trns)

  row.names(dl) <- 1:nrow(dl)

  if (ID == "xxQ7q")
    dl[,"xxQ7q"] <- NULL
  else {
    if (!is.null(prefix)) {
      ind <- which(names(dl) == ID)
      sp <- sep
      dl[,ind] <- paste(prefix, sp, dl[,ind], sep="") 
      # reorder with ID var first
      old.ind <- 1:ncol(dl)
      new.ind <- setdiff(old.ind, ind)
      new.ind <- c(ind, new.ind) 
      dl <- dl[,new.ind]
    }
  }


  return(dl)

}

