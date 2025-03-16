Write <- 
function(data=d, to=NULL, 

         format=c("csv", "txt", "tsv", "prn",
                  "Excel", "ODS", "R", "SPSS", "feather", "parquet"),

         row_names=NULL, quote="if_needed", missing=NULL, dec=".", sep=NULL,  

         ExcelTable=FALSE, ExcelColWidth=TRUE,

         quiet=getOption("quiet"), ...) {


  format.miss <- ifelse (missing(format), TRUE, FALSE)
  format <- match.arg(format)

  in.df <- deparse(substitute(data))

  dots <- list(...)
  n.values <- 0
  if (length(dots) > 0) {
    for (i in seq_along(dots)) {
      if (names(dots)[i] == "rowNames") row_names <- dots[[i]]
    }
  }

  .param.old(...)

  if (!exists(in.df, where=.GlobalEnv)) {
    cat("\n");
    if (grepl('"', in.df))
      cat(">>> Do NOT place quotes around the data frame name.\n\n")
    stop(call.=FALSE, "\n","------\n",
         "Data frame ", in.df, " does not exist.\n")
    cat("\n")
  }

  if (is.null(row_names)) {  # if just consecutive integers, ignore row names
    is.seq <- identical(rownames(data), seq_len(length(nrow(data))))
    row_names <- ifelse (is.seq, FALSE, TRUE) 
  }

  # check if a filename has a target ext or set ext from format
  fmt_to_ext <- c(csv="csv", txt="txt", tsv="tsv", prn="prn", 
                  R="rda", Excel="xlsx", ODS="ods", SPSS="sav",
                  feather="feather", parquet="parquet")
  if (!is.null(to)) {
    ext <- tolower(tools::file_ext(to))
    if (nzchar(ext)) {  # non-empty string, file extension exists
      ext_to_fmt <- c(csv="csv", txt="txt", tsv="tsv", prn="prn", 
                      rda="R", xlsx="Excel", ods="ODS", sav="SPSS",
                      feather="feather", parquet="parquet")
      format <- ext_to_fmt[ext]  # is NA if file type not recognized
      if (is.na(format) && format.miss) {
        cat("\n"); stop(call.=FALSE, "\n------\n",
          "File type ", ext, " not recognized. Specify  format  parameter\n",
          "such as format=\"Excel\" or format=\"csv\".\n\n")
      }
      out.df <- to
    }  # end extension exists
    else {  # assign extension from format
      out.df <- paste(to, ".", fmt_to_ext[format], sep="")
    }
  }  # end !is.null(to)
  else {  # create the output df name and ext
    out.df <- paste(in.df, ".", fmt_to_ext[format], sep="")
  }


  if (format %in% c("feather", "parquet", "SPSS")) {  # save the row names
    if (row_names) {
      data <- cbind(RowName = rownames(data), data)
      rownames(data) <- NULL
    }
  }
  

  if (format %in% c("csv", "txt", "tsv", "prn")) {  # text file, txt is default

    if (format %in% c("csv", "tsv", "txt"))
      if (is.null(missing)) missing <- " "
    else if (format == "prn")
      if (is.null(missing)) missing <- "NA"
    if (format == "csv") sep <- ","
    if (format == "tsv") sep <- "\t"
    if (format == "prn") sep <- " "
    if (format == "txt") if (is.null(sep)) sep <- ","

    # Add quotes to data values that match the pattern
    if (grepl("needed", quote, fixed=TRUE)) {
      data[] <- lapply(data, function(col) {
        if (is.character(col)) {
          return(sapply(col, function(x) {
            if (grepl("[[:space:],;]", x)) {
              return(paste0('"', x, '"'))
            } 
            else {
              return(x)
            }
          }))
        }  # end is a character variable
        else {
          return(col)
        }
      })  # end lapply()

      # Add quotes to row names that match the pattern
      if (is.character(rownames(data))) {
        row.nms <- rownames(data)
        rownames(data) <- ifelse(grepl("[[:space:],;]", row.nms),
                                 paste0('"', row.nms, '"'), row.nms)
        rm(row.nms)
      }
    }  # end "if_needed"

    write.table(data, file=out.df, row.names=row_names, ...,
                sep=sep, dec=dec, na=missing, quote=FALSE)

    # write variable labels if they exist as data frame l
    if ("l" %in% .getdfs()) {
      file.lbl <- substr(out.df,1,nchar(out.df)-4)
      file.lbl <- paste(paste(file.lbl,"_lbl",sep=""), ".csv" ,sep="")
      write.table(l, file=file.lbl, col.names=FALSE, dec=".", sep=",")
      .showfile(file.lbl, c(in.df, "variable labels"))
    }
  }  # end text file
  
  else if (format == "feather") {
    arrow::write_feather(data, out.df)
    if (!quiet)
      cat("[with the write_feather() function from",
          "Richardson's et al. arrow package]\n")
  }  
  
  else if (format == "parquet") {
    arrow::write_parquet(data, out.df)
    if (!quiet)
      cat("[with the write_parquet() function from",
          "Richardson's et al. arrow package]\n")
  }  

  else if (format == "SPSS") {
    haven::write_sav(data, out.df)
    if (!quiet)
      cat("[with the write_sav() function from",
      "Wickham and Miller's haven package]\n")
  }
  
  else if (format == "ODS") {
    readODS::write_ods(data, out.df, row_names=row_names)
    if (!quiet)
      cat("[with the write_ods() function from",
          "Schutten and Chan's readODS package]\n")
  }

  else if (format == "Excel") {
    wb <- createWorkbook()
    openxlsx::addWorksheet(wb, in.df)
    if (ExcelColWidth)
      openxlsx::setColWidths(wb, sheet=1, cols=1:ncol(data), widths="auto") 
    if (ExcelTable)
      openxlsx::writeDataTable(wb, in.df, x=data, colNames=TRUE,
           xy=c("A",1), rowNames=row_names, tableStyle="TableStyleLight9")
    else {
      hsl <- openxlsx::createStyle(fgFill="gray85", border="bottom")
      openxlsx::writeData(wb, in.df, x=data, colNames=TRUE, xy=c("A",1),
                rowNames=row_names, headerStyle=hsl)
    }

    openxlsx::saveWorkbook(wb, file=out.df, overwrite=TRUE)
    txt <- "Schauberger and Walker's openxlsx package]"
    if (ExcelTable  &&  !quiet)
      cat("[with the writeDataTable() function from", txt, "\n")
    else
      cat("[with the writeData() function from", txt, "\n")

  }
  
  else if (format == "R") {
    save(list=in.df, file=out.df, ...)
  }

  if (!quiet) .showfile(out.df, c(in.df, "data values"))
}
