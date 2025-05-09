reshape_long <-
  function(data, transform, group="Group", response="Response", ID="ID",
           prefix=ID, sep="", shape=c("rect", "square")) {
 
  shape <- match.arg(shape)
  group.miss <- ifelse (missing(group), TRUE, FALSE)
 
  # if a tibble, convert to data frame
  df.name <- deparse(substitute(data))  # is NULL if from shiny
  if (exists(df.name, envir=parent.frame())) {
    if (any(grepl("tbl", class(data), fixed=TRUE)))
      data <- data.frame(data)
  }

  if (shape == "rect") {
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
  }

  else { # shape is "square"

    var1 <- ifelse (group.miss, "Row", paste(group, 2, sep=""))
    var2 <- ifelse (group.miss, "Col", paste(group, 1, sep=""))

    dl <- as.data.frame(as.table(data))
    names(dl) <- c(var1, var2, response)

    # factor levels preserve original matrix order
    # For y-axis (Var1), reverse levels so first variable appears at the top
    dl[,1] <- factor(dl[,1], levels = rev(rownames(data)))
    dl[,2] <- factor(dl[,2], levels = colnames(data))
  }

  return(dl)

}

