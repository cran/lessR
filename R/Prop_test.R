Prop_test <-
function (variable=NULL, success=NULL, by=NULL, data=d,
          n_succ=NULL, n_fail=NULL, n_tot=NULL,
          p0=NULL, digits_d=3) {

  do_data <- is.null(n_succ) && is.null(n_tot) 

  # length(x) is count_n(x) + count_NA(x)
  count_n <- function(x) sum(!is.na(x))
  count_NA <- function(x) sum(is.na(x))

  # type of analysis
  pm <- FALSE  # compare proportions over groups
  p1 <- FALSE  # analyze a single proportion
  gf <- FALSE  # goodness-of-fit test
  ct <- FALSE  # cross-tabs, independence of two categorical variables

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
    if (is.null(success) && null_by) gf <- TRUE
    if (is.null(success) && !null_by) ct <- TRUE
    if (!is.null(success) && null_by) p1 <- TRUE
    if (!is.null(success) && !null_by) pm <- TRUE
  }  # end do_data

  # provide proportion information directly
  else {

    if (!is.null(n_succ) && (is.null(n_tot) && is.null(n_fail)))  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "When providing the number of successes, need the\n",
        "  corresponding number of trials, n_tot, or failures, n_fail.\n\n")
    }

    l.ns <- length(n_succ)
    l.nt <- length(n_tot)
    if (!is.null(n_fail)) n_tot <- n_succ + n_fail
    if (l.nt > 1 && is.null(n_fail) && is.null(n_succ)) gf <- TRUE
    if (l.ns == 1 && !is.null(n_tot)) p1 <- TRUE
    if (l.ns > 1 && !is.null(n_tot)) pm <- TRUE
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
    cat("------ Description ------\n\n")
    if (do_data) cat("Number of missing values:", n_NA, "\n")
    cat("Number of successes:", out$statistic, "\n")
    cat("Number of failures:", n_tot - n_succ, "\n")
    cat("Number of trials:", out$parameter, "\n")
    cat("Sample proportion:", .fmt(out$estimate, digits_d), "\n")
    cat("\n")
    cat("------ Inference ------\n\n")
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
      if (length(unique(data[,ind.var])) > 2) {
        for (i in 1:nrow(data))
          data[i,ind.var] <- ifelse (data[i,ind.var]==success, success, "z")
      }

      # two-column table, Successes and Failures
      tbl <- table(data[,ind.grp], data[,ind.var]) 

      # if needed, re-order the two tbl columns, success are in 1st column
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
    out <- prop.test(tbl)
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

    out <- chisq.test(tbl)

    obs <- .fmt(out$observed, 0)
    exp <- .fmt(out$expected, digits_d)
    res <- .fmt(out$residuals, digits_d)
    std <- .fmt(out$stdres, digits_d)

    cat("\n")
    cat(paste(">>>", out$method, " <<<", "\n\n"))
    if (do_data) {
      cat("Variable:", nm.var, "\n")
    }

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
      out <- chisq.test(tbl)   

      n.cases <- sum(rowSums(tbl))
      min_rc <- min(nrow(tbl)-1, ncol(tbl)-1)
      V <- sqrt(out$statistic / (min_rc * n.cases))
      txt <- ifelse(out$parameter == 1, " (phi)", "") 
      txt <- paste("Cramer\'s V", txt, ":", sep="")

      cat("\n")
      cat(paste(">>>", out$method, " <<<", "\n\n"))
      if (do_data) {
        cat("Variable:", nm.var, "\n")
        cat("by:", nm.grp, "\n")
      }

      cat("\n")
      cat(">>> Description")
      cat("\n\n")
      tbl.m <- addmargins(tbl)
      print(tbl.m)
      cat("\n")
      cat(txt, .fmt(V,3), "\n")

      cat("\n")
      cat(">>> Inference\n\n")
      cat("Chi-square statistic:", .fmt(out$statistic, digits_d), "\n")
      cat("Degrees of freedom:", .fmt(out$parameter, 0), "\n")
      cat("Hypothesis test of independence: p-value = ",
          .fmt(out$p.value,digits_d), sep="", "\n")

      return(invisible(out))
    }  # end do_data
  }  # end ct

}
