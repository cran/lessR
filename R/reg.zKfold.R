.regKfold <-
function(data, my_formula, kfold, new_scale, scale_response, nm,
         predictors, n.vars, n.keep, seed, digits_d=NULL, show_R) {


  if (kfold < 2)  {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "kfold must be 2 or larger.\n\n")
  }

  # create needed dummy variables
  data <- na.omit(data)
  d.new = data.frame(row.names=row.names(data))
  for (i in 1:ncol(data)) {
    nm.var <- names(data)[i]
    if (is.character(data[,i])) data[,i] <- factor(data[,i])  
    if (is.factor(data[,i])) {
      cnt <- data.frame(contrasts(data[,i]))
      rn <- rownames(cnt)
      for (i.nm in 1:length(names(cnt)))  # dummy vars
        names(cnt)[i.nm] <- paste(nm.var, "_", names(cnt)[i.nm], sep="")
      nm.dmm <- names(cnt)
      cnt <- cbind(x=rn, cnt) 
      names(cnt)[1] <- nm.var
      d.mrg <- merge(data, cnt, by=nm.var, all.x=TRUE)
      d.mrg <- d.mrg[, which(names(d.mrg) %in% nm.dmm), drop=FALSE]
      names(d.mrg) <- nm.dmm
     d.new <- cbind(d.new, d.mrg)
    }  # end is.factor
    else { 
      d.new[,ncol(d.new) + 1] <- data[,i]
      names(d.new)[ncol(d.new)] <- nm.var
    }
  }  # i in 1:ncol(data)


  # train/test split
  dd <- d.new  # store original in case rescaling each fold
  nm <- names(dd)

  tx <- character(length = 0)

  # training data indices
  t_n <- integer(length=kfold)
  t_se <- double(length=kfold)
  t_MSE <- double(length=kfold)
  t_Rsq <- double(length=kfold)

  # testing data indices
  k_n <- integer(length=kfold)
  k_se <- double(length=kfold)
  k_MSE <- double(length=kfold)
  k_Rsq <- double(length=kfold)

  # -------------------------------------------
  # training and testing analysis for each fold

  # define folds on the basis of the remainders of the
  #   scrambled integers of the row numbers of the data
  if (!is.null(seed)) set.seed(seed)
  nk <- sample(1:n.keep, n.keep, replace=FALSE) %% kfold 

  for (i.fold in 1:kfold) {
    train <- which(nk != (i.fold-1)) # combine all other folds
    test <- which(nk == (i.fold-1))  # one test fold

    # separate rescaling for train and test data
    if (new_scale != "none") {
      i.start <- ifelse (scale_response, 1, 2)
      for (j in i.start:n.vars)  {
        unq.x <- length(unique(dd[,nm[j]])) 
        if (unq.x > 2  &&  is.numeric(dd[,nm[j]])) {
          dd[train,nm[j]] <- rescale(dd[train,nm[j]], data=NULL,
                                  kind=new_scale, digits_d)
          dd[test,nm[j]] <- rescale(dd[test,nm[j]], data=NULL,
                                  kind=new_scale, digits_d)
        }
      }  # end for (i in 1:n.vars)
    }  # end new_scale != "none"

    # get formula for regression
    if (is.null(digits_d)) digits_d <- 3
    predictors <- names(dd)[2:ncol(dd)]
    prdt <- paste(predictors, collapse = "+")
    ex <- paste(names(dd)[1], "~", prdt) 
    my_formula <- as.formula(ex)

    # training data analysis
    lm.sol <- lm(my_formula, data=dd[train, ])
    t_n[i.fold] <- nrow(lm.sol$model)
    anv.sol <- anova(lm.sol)
    t_MSE[i.fold] <- anv.sol[nrow(anv.sol),3]
    smry.sol <- summary(lm.sol)
    t_Rsq[i.fold] <- smry.sol$r.squared
    t_se[i.fold] <- smry.sol$sigma
    est <- smry.sol$coefficients[,1]
    coefs = as.matrix(est)
    
    # testing data analysis
    d_test = na.omit(dd[test, nm])
    k_n[i.fold] <- nrow(d_test)
    y <- d_test[, nm[1], drop=FALSE]
    y <- data.matrix(y)
    X <- d_test[, predictors, drop=FALSE]
    X <- data.matrix(cbind(1, X))

    if (nrow(coefs) != ncol(X))  {
      cat("\nMatrix of estimated coefficients in test data\n")
      colnames(coefs) <- "Coefficients"
      print(coefs)
      cat("\nVariable names:", colnames(X)[2:ncol(X)], "\n")
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "At least one variable has no test data.\n",
        "More variables than estimated coefficients.\n\n")
    }

    y_hat <- X %*% coefs
    SSE <- sum((y - y_hat)^2)
    k_MSE[i.fold] <- SSE / (nrow(y) - (length(predictors)+1))
    k_se[i.fold] <- sqrt(k_MSE[i.fold])
    my <- mean(y)
    SSY <- sum((y - my)^2)
    k_Rsq[i.fold] <- 1 - (SSE / SSY)
  }  # end for (i.fold in 1:kfold)


  # ------------------------------------------
  # display result for each fold and the means

  # get column widths
  max.num <- integer(length=6)
  max.num[1] <- max(nchar(as.character(floor(t_se))) + digits_d + 1)
  max.num[2] <- max(nchar(as.character(floor(t_MSE))) + digits_d + 1)
  max.num[3] <- max(nchar(as.character(floor(t_Rsq))) + digits_d + 1)
  max.num[4] <- max(nchar(as.character(floor(k_se))) + digits_d + 1)
  max.num[5] <- max(nchar(as.character(floor(k_MSE))) + digits_d + 1)
  max.num[6] <- max(nchar(as.character(floor(k_Rsq))) + digits_d + 1)
  max.num[which(max.num < 6)] <- 6

  # heading labels
  buf <- .fmtc("", w = max.num[1] + max.num[2] + max.num[3] + 18)
  buf2 <- .fmtc("", w = max.num[1] + max.num[2] + max.num[3] + 13)
  buf3 <- .fmtc("", w = max.num[1] + max.num[2] + max.num[3] - 14) 
  buf4 <- .fmtc("", w = max.num[1] + max.num[2] + max.num[3] + 15)
  dash <- .fmtc("", w = max.num[4] + max.num[5] + max.num[6] + 6)
  dash <- gsub(" ", "-", dash, fixed=TRUE)

  tx[length(tx)+1] <- paste("       ",
       "Model from Training Data", buf3,
       "Applied to Testing Data", sep="")
  tx[length(tx)+1] <- paste(format("", width=7), dash, "   ", dash, sep="")

  t_se.lbl <- .fmtc("se", max.num[1]+1)
  t_MSE.lbl <- .fmtc("MSE", max.num[2]+1)
  t_Rsq.lbl <- .fmtc( "Rsq", max.num[3]+1)
  k_se.lbl <-  .fmtc("se", max.num[4]+1)
  k_MSE.lbl <- .fmtc("MSE", max.num[5]+1)
  k_Rsq.lbl <- .fmtc("Rsq", max.num[6]+1)
  tx[length(tx)+1] <- paste("fold",
           "    n", t_se.lbl, t_MSE.lbl, t_Rsq.lbl,
           "     n", k_se.lbl, k_MSE.lbl, k_Rsq.lbl, sep="")

  # indices for the different folds
  mx.nch <- nchar(as.character(max(t_n)))
  for (i in 1:kfold) {
    tx[length(tx)+1] <- paste(" ", i, "|", 
      .fmti(t_n[i], mx.nch),
      .fmt(t_se[i], digits_d, max.num[1]),
      .fmt(t_MSE[i], digits_d, max.num[2]),
      .fmt(t_Rsq[i], digits_d, max.num[3]), "|",
      .fmti(k_n[i], 3),
      .fmt(k_se[i], digits_d, max.num[4]),
      .fmt(k_MSE[i], digits_d, max.num[5]),
      .fmt(k_Rsq[i], digits_d, max.num[6]))
    }

  # mean testing results
  tm_se=mean(t_se)
  tm_MSE=mean(t_MSE)
  tm_Rsq=mean(t_Rsq)
  km_se=mean(k_se)
  km_MSE=mean(k_MSE)
  km_Rsq=mean(k_Rsq)

  tx[length(tx)+1] <- paste("     ", dash, "  ", dash)
  buf1 <- " "
  for (ic in 1:(mx.nch)) buf1 = paste(buf1, " ", sep="")
  buf2 <- paste(buf1, " ", sep="")
  tx[length(tx)+1] <- paste("Mean", buf1,
      .fmt(tm_se, digits_d, max.num[4]),
      .fmt(tm_MSE, digits_d, max.num[5]),
      .fmt(tm_Rsq,  digits_d, max.num[6]), buf2, 
      .fmt(km_se, digits_d, max.num[4]),
      .fmt(km_MSE, digits_d, max.num[5]),
      .fmt(km_Rsq,  digits_d, max.num[6]))


  return(list(tx=tx, m_se=km_se, m_MSE=km_MSE, m_Rsq=km_Rsq))

}
