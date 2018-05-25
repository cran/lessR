Write <- 
function(ref=NULL, data=mydata, format=c("csv", "R", "Excel"), 
         row.names=TRUE, ...) {

  format <- match.arg(format)

  df.name <- deparse(substitute(data))

  if (!exists(df.name, where=.GlobalEnv)) {
    cat("\n");
    if (grepl('"', df.name))
      cat(">>> Do NOT have quotes around the data frame name.\n\n")
    stop(call.=FALSE, "\n","------\n",
         "Data frame ", df.name, " does not exist.\n")
    cat("\n")
  }

  if (format == "csv") {
    if (is.null(ref))
      file.data <- paste(df.name, ".csv", sep="")
    else {
       txt <- ifelse (grepl(".csv", ref), "", ".csv")
       file.data <- paste(ref, txt, sep="")
    }
    write.csv(data, file=file.data, ...)
    .showfile(file.data, c(df.name, "data values"))

    mylabels <- attr(data, which="variable.labels") # save variable labels
    if (!is.null(mylabels)) {
      mylabels <- data.frame(mylabels)
      file.lbl <- substr(file.data,1,nchar(file.data)-4)
      file.lbl <- paste(paste(file.lbl,"_lbl",sep=""), ".csv" ,sep="")
      write.table(mylabels, file=file.lbl, col.names=FALSE, dec=".", sep=",")
      .showfile(file.lbl, c(df.name, "variable labels"))
    }
  }
  
  else if (format == "R") {
    if (is.null(ref))
      file.data <- paste(df.name, ".rda", sep="")
    else {
      txt <- ifelse (grepl(".rda", ref), "", ".rda")
      file.data <- paste(ref, txt, sep="")
    }
    save(list=df.name, file=file.data, ...)
    .showfile(file.data, c(df.name, "data frame contents"))
  }
  
  else if (format == "Excel") {
    if (is.null(ref))
      file.data <- paste(df.name, ".xlsx", sep="")
    else {
      txt <- ifelse (grepl(".xlsx", ref), "", ".xlsx")
      file.data <- paste(ref, txt, sep="")
    }
    wb <- createWorkbook()
    addWorksheet(wb, df.name)
    hs1 <- createStyle(fgFill=rgb(.9,.9,.9), halign="CENTER",
          textDecoration="italic", border="Bottom")
    setColWidths(wb, sheet=1, cols=1:ncol(data), widths="auto") 
    writeDataTable(wb, "mydata", x=mydata, colNames=TRUE,
         startCol="A", startRow=1, tableStyle="TableStyleLight9",
         rowNames=row.names)
         #borders="none", headerStyle=hs1, borderStyle="dashed")
    saveWorkbook(wb, file=file.data, overwrite=TRUE)
    txt <- "Alexander Walker's openxlsx package]"
    cat("[with the writeDataTable function from", txt, "\n")
    cat("\n")
    .showfile(file.data, c(df.name, "data values"))
    cat("\n")

  }

}
