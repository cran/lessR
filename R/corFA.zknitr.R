.corfa.knitr <-
function(dname=mydata, n.inds, n.factors, digits.d=3, explain=TRUE) {


  # set parameters
  d <- digits.d
  

  tx <- character(length = 0)

  tx[length(tx)+1] <- "---"
  tx[length(tx)+1] <- "title: \"Factor Analysis\""
  tx[length(tx)+1] <- "output: html_document"
  tx[length(tx)+1] <- "---"

  ref <- getOption("data.file")
  if (is.null(ref)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
       "To generate a knitr file, need to first read the data for this\n",
       "regression analysis with the lessR function Read.\n\n")
  }
  read.lessR <- FALSE
  if (ref %in% c("Employee", "Reading", "Cars93", "Jackets", "Learn", "Mach4")) {
    ref  <- paste(ref, "\"", ", format=\"lessR", sep="")
    read.lessR <- TRUE
  }


  tx[length(tx)+1] <- paste(
    "The purpose of this analysis is to identify the factors that underlie ", 
    "the observed variables, the items.",
    sep="")

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
  tx[length(tx)+1] <- "suppressPackageStartupMessages(library(lessR))  # load lessR"
  tx[length(tx)+1] <- "```"




  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## The Data"

  tx[length(tx)+1] <- "First read the data"
  if (explain) tx[length(tx)] <- paste(tx[length(tx)],
    ", with the brief version of `Read`", sep="")
  tx[length(tx)] <-  paste(tx[length(tx)],".", sep="")

  if (explain) {
    if (read.lessR)
      tx[length(tx)+1] <- paste(
         "Here read from a data file included with the `lessR` package.",
         sep="")
  }

  tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
  tx[length(tx)+1] <- paste(dname, " <- rd.brief(\"", ref, "\")", sep="")
  tx[length(tx)+1] <- "```"

  tx[length(tx)+1] <- paste(
    "Data from the following variables are available for analysis: ",
    "`r names(", dname, ")`.",
    sep="")




  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "## Basic Analysis"

  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- "### The Estimated Model"
  tx[length(tx)+1] <- ""

    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste(
      "First obtain the least squares estimates of the model, the values of ",
      "each regression coefficient, $b_i$. Do so with the `lessR` function ",
      "`Regression`, here abbreviated as `reg`.",
      sep="")

  cv <- " "
  tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
  tx[length(tx)+1] <- "# Generate graphics later at specific locations with `plotHere` function"
  tx[length(tx)+1] <- paste("r <- reg(", cv, ", graphics=FALSE)", sep="")
  tx[length(tx)+1] <- "```"

    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste(
      "The first part of the output specifies the analysis ",
      "in terms of the variables and data.",
      sep="")
  tx[length(tx)+1] <- paste("```{r", show, "}", sep="")
  tx[length(tx)+1] <-"r$out_background" 
  tx[length(tx)+1] <- "```"

  return(tx)

}
