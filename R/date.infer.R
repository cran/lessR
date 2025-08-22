date.infer <- function (x.call) {

  x11 <- x.call[1,1]

  n.ch <- nchar(x11)
  if (n.ch %in% 6:10) {
    isQ <- grepl("Q1|Q2|Q3|Q4", x11)
    isM <- grepl("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec", x11)
    if (isM) {
      x.call[,1] <- gsub(" ", "", x.call[,1])  # remove all spaces
      year <- substr(x.call[,1], 1, 4)
      monthNm <- substr(x.call[,1], 5, 7)
      x.call[,1] <- as.Date(paste(year, monthNm, "01", sep="-"),
                             format="%Y-%b-%d")
    }
    else if (isQ) {  # convert dates entered as 2024 Q3 to R Date
      parts <- strsplit(gsub("\\s+", "", x.call[,1]), "Q")  # remove Q
      # Extract quarter, `[` is R extraction operator
      year <- as.numeric(sapply(parts, `[`, 1))  # 1st list element (year)
      quarter <- as.numeric(sapply(parts, `[`, 2))  # 2nd list element
      month <- 1 + (quarter - 1) * 3  # get month Q1=1, Q2=4, Q3=7, Q4=10
      x.call[,1] <- as.Date(paste(year, month, "01", sep="-"))  #to Date
    }  # end quarter
    else {  # see if numeric numeric date format
      punct <- " "  # see if there are two punctuation delimiters
      if (length(gregexpr("/", x11, fixed=TRUE)[[1]]) == 2) punct <- "/"
      if (length(gregexpr("-", x11, fixed=TRUE)[[1]]) == 2) punct <- "-"
      if (length(gregexpr(".", x11, fixed=TRUE)[[1]]) == 2) punct <- "."
      if (punct %in% c("/", "-", "."))   # only evaluate probable dates
        x.call[,1] <- .charToDate(x.call[,1], punct)
    }  # numeric date format
  }  # end n.ch is 6, 7, 8

  return(x.call)
}
