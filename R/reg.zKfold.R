.regKfold <-
function(data, my_formula, kfold, new_scale, scale_response, nm,
         predictors, n.vars, n.keep, seed, digits_d=NULL, show_R) {

  dd <- data  # store original in case rescaling each fold

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
  if (!is.null(seed))
    set.seed(seed)
  nk <- sample(1:n.keep, n.keep) %% kfold

  for (i in 1:kfold) {
    train <- which(nk != (i-1)) # combine all other folds
    test <- which(nk == (i-1))  # one test fold
    if (new_scale != "none") {  # separate rescaling for train and test data
      i.start <- ifelse (scale_response, 1, 2)
      for (j in i.start:n.vars)  {
        unq.x <- length(unique(data[,nm[j]])) 
        if (unq.x > 2  &&  is.numeric(data[,nm[j]])) {
          dd[train,nm[j]] <- rescale(data[train,nm[j]], data=NULL,
                                  kind=new_scale, digits_d)
          dd[test,nm[j]] <- rescale(data[test,nm[j]], data=NULL,
                                  kind=new_scale, digits_d)
        }
      }  # end for (i in 1:n.vars)
    }  # end new_scale != "none"

    d_test = na.omit(dd[test, nm])
    k_n[i] <- nrow(d_test)

    # training data solution
    lm.sol <- lm(my_formula, data=dd[train, ])
    t_n[i] <- nrow(lm.sol$model)
    if (is.null(digits_d)) digits_d <- 3
    anv <- .reg1anvBasic(lm.sol, digits_d, show_R)
    t_MSE[i] <- anv$MSW
    fit <- .reg1fitBasic(lm.sol, anv$tot["ss"], digits_d, show_R)
    t_se[i] <- fit$se
    t_Rsq[i] <- fit$Rsq
    est <- .reg1modelBasic(lm.sol, digits_d, show_R)
    coefs = as.matrix(est$estimates)
    
    # testing data analysis
    y <- d_test[, nm[1], drop=FALSE]
    y <- data.matrix(y)
    X <- d_test[, predictors, drop=FALSE]
    X <- data.matrix(cbind(1, X))

    y_hat <- X %*% coefs
    SSE <- sum((y - y_hat)^2)
    k_MSE[i] <- SSE / (nrow(y) - n.vars)
    k_se[i] <- sqrt(k_MSE[i])
    SSY <- sum((y - mean(y))^2)
    k_Rsq[i] <- 1 - (SSE / SSY)
  }  # end for (i in 1:kfold)


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
  for (i in 1:kfold) {
    tx[length(tx)+1] <- paste(" ", i, "|", 
      .fmti(t_n[i], 3),
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
  tx[length(tx)+1] <- paste("Mean", "    ",
      .fmt(tm_se, digits_d, max.num[4]),
      .fmt(tm_MSE, digits_d, max.num[5]),
      .fmt(tm_Rsq,  digits_d, max.num[6]), "     ",
      .fmt(km_se, digits_d, max.num[4]),
      .fmt(km_MSE, digits_d, max.num[5]),
      .fmt(km_Rsq,  digits_d, max.num[6]))


  return(list(tx=tx, m_se=km_se, m_MSE=km_MSE, m_Rsq=km_Rsq))

}
