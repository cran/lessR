Write <- 
function(to=NULL, data=d, format=c("csv", "R", "Excel"), rowNames=NULL,
         ExcelTable=FALSE, ExcelColWidth=TRUE,
         quiet=getOption("quiet"), ...) {

  format <- match.arg(format)

  df.name <- deparse(substitute(data))

  .param.old(...)

  if (!exists(df.name, where=.GlobalEnv)) {
    cat("\n");
    if (grepl('"', df.name))
      cat(">>> Do NOT have quotes around the data frame name.\n\n")
    stop(call.=FALSE, "\n","------\n",
         "Data frame ", df.name, " does not exist.\n")
    cat("\n")
  }

  if (is.null(rowNames)) {  # if just consecutive integers, ignore row names
    if (format %in% c("csv", "Excel")) {
      rn <- length(setdiff(row.names(data), as.character(1:nrow(data))))
      rowNames <- ifelse (rn == 0, FALSE, TRUE) 
    } 
  }

  if (format == "csv") {
    if (is.null(to))
      file.data <- paste(df.name, ".csv", sep="")
    else {
       txt <- ifelse (grepl(".csv", to), "", ".csv")
       file.data <- paste(to, txt, sep="")
    }
    write.csv(data, file=file.data, row.names=rowNames, ...)
    .showfile(file.data, c(df.name, "data values"))

    l <- attr(data, which="variable.labels") # save variable labels
    if (!is.null(l)) {
      l <- data.frame(l)
      file.lbl <- substr(file.data,1,nchar(file.data)-4)
      file.lbl <- paste(paste(file.lbl,"_lbl",sep=""), ".csv" ,sep="")
      write.table(l, file=file.lbl, col.names=FALSE, dec=".", sep=",")
      .showfile(file.lbl, c(df.name, "variable labels"))
    }
  }
  
  else if (format == "Excel") {
    if (is.null(to))
      file.data <- paste(df.name, ".xlsx", sep="")
    else {
      txt <- ifelse (grepl(".xlsx", to), "", ".xlsx")
      file.data <- paste(to, txt, sep="")
    }

    wb <- createWorkbook()
    addWorksheet(wb, df.name)
    if (ExcelColWidth)
      setColWidths(wb, sheet=1, cols=1:ncol(data), widths="auto") 
    if (ExcelTable)
      writeDataTable(wb, df.name, x=data, colNames=TRUE,
           xy=c("A",1), rowNames=rowNames, tableStyle="TableStyleLight9")
    else {
      hsl <- createStyle(fgFill="gray85", border="bottom")
      writeData(wb, df.name, x=data, colNames=TRUE, xy=c("A",1),
                rowNames=rowNames, headerStyle=hsl)
    }

    saveWorkbook(wb, file=file.data, overwrite=TRUE)
    txt <- "Alexander Walker's openxlsx package]"
    if (ExcelTable  &&  !quiet)
      cat("[with the writeDataTable function from", txt, "\n")
    else
      cat("[with the writeData function from", txt, "\n")
    cat("\n")
    .showfile(file.data, c(df.name, "data values"))
    cat("\n")

  }
  
  else if (format == "R") {
    if (is.null(to))
      file.data <- paste(df.name, ".rda", sep="")
    else {
      txt <- ifelse (grepl(".rda", to), "", ".rda")
      file.data <- paste(to, txt, sep="")
    }
    save(list=df.name, file=file.data, ...)
    .showfile(file.data, c(df.name, "data frame contents"))
  }

}
