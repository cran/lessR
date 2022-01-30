Write <- 
function(data=d, to=NULL, format=c("csv", "R", "Excel", "ODS", "SPSS"),
         rowNames=NULL,

         ExcelTable=FALSE, ExcelColWidth=TRUE,

         quiet=getOption("quiet"), ...) {

  message(">>> Note:  data  is now the first parameter,  to  is second\n")

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
    if (format %in% c("csv", "Excel", "ODS")) {
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
      l <- data.frame(l, stringsAsFactors=TRUE)
      file.lbl <- substr(file.data,1,nchar(file.data)-4)
      file.lbl <- paste(paste(file.lbl,"_lbl",sep=""), ".csv" ,sep="")
      write.table(l, file=file.lbl, col.names=FALSE, dec=".", sep=",")
      .showfile(file.lbl, c(df.name, "variable labels"))
    }
  }
  
  else if (format == "SPSS") {
    if (is.null(to))
      file.data <- paste(df.name, ".sav", sep="")
    else {
      txt <- ifelse (grepl(".sav", to), "", ".sav")
      file.data <- paste(to, txt, sep="")
    }

    haven::write_sav(data, file.data)

    txt <- "Wickham and Miller's haven package]"
    if (!quiet)
      cat("[with the write_sav() function from", txt, "\n")
    cat("\n")
    .showfile(file.data, c(df.name, "data values"))
    cat("\n")
  }
  
  else if (format == "ODS") {
    if (is.null(to))
      file.data <- paste(df.name, ".ods", sep="")
    else {
      txt <- ifelse (grepl(".ods", to), "", ".ods")
      file.data <- paste(to, txt, sep="")
    }

    readODS::write_ods(data, file.data)

    txt <- "Schutten and Chan's readODS package]"
    if (!quiet)
      cat("[with the write_ods() function from", txt, "\n")
    cat("\n")
    .showfile(file.data, c(df.name, "data values"))
    cat("\n")
  }

  else if (format == "Excel") {
    if (is.null(to))
      file.data <- paste(df.name, ".xlsx", sep="")
    else {
      txt <- ifelse (grepl(".xlsx", to), "", ".xlsx")
      file.data <- paste(to, txt, sep="")
    }

    wb <- createWorkbook()
    openxlsx::addWorksheet(wb, df.name)
    if (ExcelColWidth)
      openxlsx::setColWidths(wb, sheet=1, cols=1:ncol(data), widths="auto") 
    if (ExcelTable)
      openxlsx::writeDataTable(wb, df.name, x=data, colNames=TRUE,
           xy=c("A",1), rowNames=rowNames, tableStyle="TableStyleLight9")
    else {
      hsl <- openxlsx::createStyle(fgFill="gray85", border="bottom")
      openxlsx::writeData(wb, df.name, x=data, colNames=TRUE, xy=c("A",1),
                rowNames=rowNames, headerStyle=hsl)
    }

    openxlsx::saveWorkbook(wb, file=file.data, overwrite=TRUE)
    txt <- "Schauberger and  Walker's openxlsx package]"
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
