Prop_test <-
function (variable=NULL, success=NULL, by=NULL, data=d,
          n_succ=NULL, n_fail=NULL, n_tot=NULL, n_table=NULL,
          Yates=FALSE, p0=NULL, digits_d=3) {

  # length(x) is count_n(x) + count_NA(x)
  count_n <- function(x) sum(!is.na(x))
  count_NA <- function(x) sum(is.na(x))

  # classify analysis by (a) if data or freqs and (b) by type
  # ---------------------------------------------------------
  # data or not
  do_data <- is.null(n_succ) && is.null(n_tot) && is.null(n_table) 

  # type of analysis
  p1 <- FALSE  # analyze a single proportion
  pm <- FALSE  # compare proportions over groups
  gf <- FALSE  # goodness-of-fit
  ct <- FALSE  # cross-tabs independence
  # ---------------------------------------------------------

  # option to provide raw data from which to compute proportion information
  # two potential variables: variable and by
  # variable is required if entering data
  if (do_data) {
    data.vars <- as.list(seq_along(data))
    names(data.vars) <- names(data)
    ind.var <- eval(substitute(variable), envir=data.vars, parent.frame())
    nm.var <- names(data)[ind.var]
    if (!missing(by)) {
      null_by <- FALSE    
      ind.grp <- eval(substitute(by), envir=data.vars, parent.frame())
      nm.grp <- names(data)[ind.grp]
    }
    else
      null_by <- TRUE

    # discover the type of do_data analysis
    if (!is.null(success) && null_by) p1 <- TRUE
    if (!is.null(success) && !null_by) pm <- TRUE
    if (is.null(success) && null_by) gf <- TRUE
    if (!is.null(n_table)) ct <- TRUE
    if (is.null(success) && !null_by) ct <- TRUE
  }  # end do_data

  # provide proportion information directly
  else {
    if (!is.null(n_succ) && (is.null(n_tot) && is.null(n_fail)))  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "When providing the number of successes, need the\n",
        "  corresponding number of trials, n_tot, or failures, n_fail.\n\n")
    }
    miss.n_table <- ifelse (is.null(n_table), TRUE, FALSE)

    if (!is.null(n_fail)) n_tot <- n_succ + n_fail
    if (length(n_succ) == 1 && !is.null(n_tot)) p1 <- TRUE
    if (length(n_succ) > 1 && !is.null(n_tot)) pm <- TRUE
    if (is.null(n_succ) && miss.n_table) gf <- TRUE
    if (!is.null(n_table)) ct <- TRUE
  }


  # --------------
  # one proportion
  if (p1) {  

    if (do_data) {
      n_NA <- count_NA(data[,ind.var])
      n_tot <- count_n(data[,ind.var])
      n_succ <- length(data[data[,ind.var]==success, ind.var])
    }

    if (missing(p0)) p0 <- 0.5
    out <- binom.test(n_succ, n_tot, p=p0)

    cat("\n")
    cat(paste(">>>", out$method, "of a proportion <<<", "\n\n"))
    if (do_data) {
      cat("Variable:", nm.var, "\n")
      cat("success:", success, "\n")
      cat("\n")
    }
    cat("------ Describe ------\n\n")
    if (do_data) cat("Number of missing values:", n_NA, "\n")
    cat("Number of successes:", out$statistic, "\n")
    cat("Number of failures:", n_tot - n_succ, "\n")
    cat("Number of trials:", out$parameter, "\n")
    cat("Sample proportion:", .fmt(out$estimate, digits_d), "\n")
    cat("\n")
    cat("------ Infer ------\n\n")
    cat("Hypothesis test for null of ", p0, ", p-value: ",
        .fmt(out$p.value,digits_d), sep="", "\n")
    cat("95% Confidence interval:", .fmt(out$conf.int[1],digits_d), "to",
        .fmt(out$conf.int[2],digits_d), "\n")

    return(invisible(out))
  }  # end p1


  # --------------------
  # multiple proportions
  if (pm) {

    if (do_data) {
      # if more than 2 levels, reduce to 2 levels
      if (length(unique(data[,ind.var])) > 2) {  
        for (i in 1:nrow(data))
          data[i,ind.var] <- ifelse (data[i,ind.var]==success, success, "fail")
      }

      # two-column table, Successes and Failures
      tbl <- table(data[,ind.grp], data[,ind.var]) 

      # if needed, re-order the two tbl columns, success in 1st column
      ind <- which(colnames(tbl) == success)
      if (ind != 1) {
        tbln <- tbl
        tbln[,1] = tbl[,2,drop=FALSE]
        tbln[,2] = tbl[,1,drop=FALSE]
        colnames(tbln)[1] = colnames(tbl)[2]
        colnames(tbln)[2] = colnames(tbl)[1]
        tbl <- tbln
      }
    }  # end do data

    else {  # construct the table from given proportion info
      n_fail <- n_tot - n_succ
      m <- matrix(c(n_succ, n_fail), byrow=FALSE, ncol=2)
      if (is.null(names(n_succ)))
        rn <-  1:nrow(m)
      else
        rn <- names(n_succ)
      dimnames(m) <- list(rn, c("Success", "Fail"))
      tbl <- as.table(m)
    }

    # do the analysis
    out <- prop.test(tbl, correct=Yates)
    rn <- rownames(tbl)
    for (i in 1:length(rn))
      names(out$estimate)[i] <- rownames(tbl)[i]

    cat("\n")
    cat(paste(">>>", out$method, " <<<", "\n\n"))
    if (do_data) {
      cat("Variable:", nm.var, "\n")
      cat("success:", success, "\n")
      cat("by:", nm.grp, "\n")
    }

    cat("\n")
    cat(">>> Description")
    ns <- .fmt(tbl[,1], 0)
    nt <- .fmt(rowSums(tbl), 0)
    vals <- t(.fmt(out$estimate, digits_d))
    vals <- as.vector(vals)
    m <- matrix(c(ns, nt, vals), byrow=TRUE, ncol=length(ns))
    k <- data.frame(m)
    names(k) <- names(out$estimate)
    nm1 <- paste("n_", success, sep="")
    row.names(k) <- c(nm1, "n_total", "proportion")
    k <- kable(k, format="pandoc", align="r")
    print(k)

    cat("\n")
    cat(">>> Inference\n\n")
    cat("Chi-square statistic:", .fmt(out$statistic, digits_d), "\n")
    cat("Degrees of freedom:", .fmt(out$parameter, 0), "\n")
    cat("Hypothesis test of equal population proportions: p-value = ",
        .fmt(out$p.value,digits_d), sep="", "\n")

    return(invisible(out))
  }


  # ---------------
  # goodness-of-fit
  if (gf) {

    if (do_data) {
      tbl <- table(data[,ind.var])
      nms <- names(tbl)
    }
    else {
      tbl <- n_tot
      if (is.null(names(n_tot)))
        nms <-  1:length(n_tot)
      else
        nms <- names(n_tot)
    }

    # do the analysis
    out <- chisq.test(tbl)

    obs <- .fmt(out$observed, 0)
    exp <- .fmt(out$expected, digits_d)
    res <- .fmt(out$residuals, digits_d)
    std <- .fmt(out$stdres, digits_d)

    cat("\n")
    cat(paste(">>>", out$method, " <<<", "\n\n"))
    if (do_data)
      cat("Variable:", nm.var, "\n")

    cat("\n")
    cat(">>> Description")
    m <- matrix(c(obs,exp,res,std), byrow=TRUE, ncol=length(tbl))
    k <- data.frame(m)
    names(k) <- nms
    row.names(k) <- c("observed", "expected", "residual", "stdn res")
    k <- kable(k, format="pandoc", align="r")
    print(k)

    cat("\n")
    cat(">>> Inference\n\n")
    cat("Chi-square statistic:", .fmt(out$statistic, digits_d), "\n")
    cat("Degrees of freedom:", .fmt(out$parameter, 0), "\n")
    cat("Hypothesis test of equal population proportions: p-value = ",
        .fmt(out$p.value,digits_d), sep="", "\n")

    return(invisible(out))
  }  # end gf


  # -----------------------
  # cross-tabs independence
  if (ct) {

    if (do_data) {
      tbl <- table(data[,ind.grp], data[,ind.var])
      names(attributes(tbl)$dimnames) <- c(nm.grp, nm.var)

      cat("variable:", nm.var, "\n")
      cat("by:", nm.grp, "\n")
    }  # end do_data

    else {  # freq input
      # read the cross-tab table
      if (nchar(n_table) == 0) {
        n_table <- file.choose()
        cat("File: ", n_table, "\n\n")
      }
      if (grepl(".csv$", n_table) || grepl(".txt$", n_table)) 
        tbl <- read.csv(n_table, header=FALSE)
      else if (grepl(".xlsx$", n_table)) 
        tbl <- openxlsx::read.xlsx(n_table)
      else if (grepl("FreqTable99$", n_table)) { 
        f.nm <- "dataFreqTable99.rda"
        path.name <- paste(find.package("lessR"), "/data/",  f.nm, sep="")
        x.env <- new.env()  # scratch environment
        load(path.name, envir=x.env)
        dname <- "dataFreqTable99"
        tbl <- get(dname, pos=x.env)
      }
      else {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Frequency table file must be of file type\n",
          "  .csv for a Comma Separated Value text file or\n",
          "  .xlsx for an Excel file.\n\n")
      }
      nms <- 1:ncol(tbl)
      suppressWarnings(names(tbl) <- rep("", ncol(tbl)))
    }  # end freq input

    # analysis
    out <- chisq.test(tbl, correct=Yates)   

    n.cases <- sum(rowSums(tbl))
    min_rc <- min(nrow(tbl)-1, ncol(tbl)-1)
    V <- sqrt(out$statistic / (min_rc * n.cases))
    txt <- ifelse(out$parameter == 1, " (phi)", "") 
    txt <- paste("Cramer\'s V", txt, ":", sep="")

    out <- chisq.test(tbl, correct=Yates)
    obs <- .fmt(out$observed, 0)
    exp <- .fmt(out$expected, digits_d)
    res <- .fmt(out$residuals, digits_d)
    std <- .fmt(out$stdres, digits_d)

    k <- data.frame(row.names=1:(nrow(tbl)*ncol(tbl)))
    k[,1] <- rep(1:nrow(tbl), ncol(tbl))
    k[,2] <- rep(1:ncol(tbl), each=nrow(tbl))
    k[,3] <- obs; k[,4] <- exp;  k[,5] <- res;  k[,6] <- std
    names(k) <- c("Row", "Col", "Observed", "Expected", "Residual", "Stnd Res")
    k$Residual <- as.numeric(k$Observed) - as.numeric(k$Expected)
    k <- k[order(k[,1]),]  # display by row

    cat("\n")
    cat(paste(">>>", out$method, " <<<", "\n\n"))

    cat(">>> Description\n\n")
    if (do_data)
      print(addmargins(tbl))
    else {
      cat("Cell Frequencies")
      print(tbl, row.names=FALSE)
    }
    cat("\n", txt, .fmt(V,3), "\n\n")
    print(k, row.names=FALSE)

    cat("\n")
    cat(">>> Inference\n\n")
    cat("Chi-square statistic:", .fmt(out$statistic, digits_d), "\n")
    cat("Degrees of freedom:", .fmt(out$parameter, 0), "\n")
    cat("Hypothesis test of equal population proportions: p-value = ",
        .fmt(out$p.value,digits_d), sep="", "\n")

    return(invisible(out))

  }  # end ct

}
