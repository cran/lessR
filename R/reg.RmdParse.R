
# parser called by .reg.Rmd() to process a file of marked text
.RmdParse <- function(f.name, path, n.pred=-1,
                   explain=NULL, results=NULL, interpret=NULL,
                   res_sort="") {
# path <- getwd()
  if (!is.null(path)) {  # a system file, spec includes full reference
    f.name <- file.path(path, f.name) 
  }
  txt <- readLines(f.name, skipNul=TRUE)  # read all lines in the file

  the.ln <- NULL
  i.line <- 0
  while (i.line < length(txt)) {
    i.line <- i.line + 1
    ln <- paste(txt[i.line], " ", sep="")  # the ith line of vector txt

    if (substr(ln, 1, 1) == "%") { next }  # comment, skip and go to next line 

    cl <- gregexec("`", ln)
    if (cl[[1]][1] != -1) {  # at least 1 var found in the.ln
      cols <- cl[[1]][1,]  # columns that contain vars marks
      n.sym <- length(cols) / 2  # symbol mark comes in pairs
      ind <- (1:length(cols)) %% 2
      c.evn <- cols[ind==0]  # even indices cols
      c.odd <- cols[ind==1]  # odd indices cols
      c.beg <- c(1, c.evn+1)
      c.end <- c(c.odd-1, nchar(ln))

      # sym is symbol, whatever is between ` and `
      for (i.sym in 1:n.sym) {  # process each var within the ith line
        chr <- substr(ln, c.beg[i.sym], c.end[i.sym])
        sym <- substr(ln, c.odd[i.sym]+1, c.evn[i.sym]-1)

        if (substr(sym, 1, 3) == "end") { next }  # end if-block 

        else  # a function
          if (substr(sym, 1,3) == "eq." || substr(sym, 1,4) == "out." ||
              substr(sym, 1,5) == "intr." || substr(sym, 1,5) == "plot." ||
              substr(sym, 1,3) == "in.")
            the.ln <- paste(the.ln, chr, eval.parent(as.name(sym), n=1)(),
                            sep="")

        else  # inline R expression
          if (substr(sym, 1,2) == "r ") {
            sym <- paste("`", sym, "`", sep="") 
            the.ln <- paste(the.ln, chr, sym, sep="")
          }

        else  # assignment statement
          if (grepl("<-", sym, fixed=TRUE))
            the.ln <- paste(the.ln, "\n```{r echo=FALSE}\n", sym, "\n", "```",
                            sep="")
        
        else  # header
          if (substr(sym, 1,1) == "#")
            the.ln <- paste(the.ln, chr, sym, "\n", sep="")

        else  # if block
          if (substr(sym, 1,2) == "if") {
            delete <- FALSE
            if (grepl("explain", sym)) if (!explain) delete <- TRUE
            if (grepl("results", sym)) if (!results) delete <- TRUE
            if (grepl("interpret", sym)) if (!interpret) delete <- TRUE
            if (grepl("n.pred=1", sym)) if (n.pred>1) delete <- TRUE
            if (grepl("n.pred>1", sym)) if (n.pred==1) delete <- TRUE
            if (grepl("n.pred>2", sym)) if (n.pred<=2) delete <- TRUE
            if (grepl("cooks", sym)) if (res_sort != "cooks") delete <- TRUE
            if (delete) {
              while (substr(txt[i.line], 1,4) != "`end")
                i.line <- i.line + 1
            }
          } 

        else  # a variable with an assigned value, such as a variable name
          the.ln <- paste(the.ln, chr, eval.parent(as.name(sym), n=1), sep="")

        if (i.sym == n.sym) {  # get any text past the last sym
          chr <- substr(ln, c.beg[n.sym+1], c.end[n.sym+1])
          the.ln <- paste(the.ln, chr, sep="")
         }

      }  # end each var one at a time on indx
    }  # end found vars on indx

    else { # no vars in the line
      if (ln == " ") ln <- "\n\n"
      the.ln <- paste(the.ln, ln, sep="")
    }
  }  # end line-by-line processing

  return(the.ln)
}
