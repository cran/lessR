Model <-
function(my_formula, data=d, brief=getOption("brief"), xlab=NULL, ...) {


  if (missing(my_formula)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify a model by listing it first or set according to:  my_formula\n\n")
  }

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  if ("mydata" %in% dfs  &&  !("d" %in% dfs)) d <- mydata 

  dname <- deparse(substitute(data))  # get data frame name for cor before sort
  options(dname = dname)

  nm <- all.vars(my_formula)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(data)

  var.type <- integer(length=n.vars)
  n.unique <- integer(length=n.vars)
  for (i in 1:n.vars) {
    var.type[i] <- ""
    if (is.factor(data[,nm[i]])) var.type[i] <- "cat"
    n.unique[i] <- length(unique(data[,nm[i]]))
    #if (n.unique[i] < n_cat) var.type[i] <- "cat" 
    #else 
    if (is.numeric(data[,nm[i]])) var.type[i] <- "num"
  }

  all.preds.cat <- TRUE
  for (i in 2:n.vars) if (var.type[i] != "cat") all.preds.cat <- FALSE
  all.preds.num <- TRUE
  for (i in 2:n.vars) if (var.type[i] != "num") all.preds.num <- FALSE

  if (var.type[1] == "num") {

    if (all.preds.num) {  # regression

      is.bin <- TRUE
      for (i in 1:n.obs) if (data[i,nm[1]]!=0 && data[i,nm[1]]!=1) is.bin <- FALSE

      if (is.bin) {
        cat("\n")
        .dash(44)
        cat("Run Logit Regression analysis to account for\n",
            "binary response variable ", nm[1], ".  \n", sep="")
        .dash(44)
        cat("\n")
        Logit(my_formula, data, ...)
      }

      else if (length(unique(data[,nm[1]])) == 2) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "The response variable ", nm[1], " is binary, but can only have values of 0 or 1.\n\n",
          "Use Recode to transform the existing values to 0's and 1's.\n\n")
      }

      else {
        cat("\n")
        .dash(60)
        cat("Run Regression analysis to account for response variable ", nm[1], 
            ".  \n", sep="")
        .dash(60)
        cat("\n")
        Regression(my_formula, data, ...)
      }
    }

    else if (all.preds.cat) {

      if (n.pred == 1  && (n.unique[2] == 2)) {  # t-test
        cat("\n")
        .dash(60)
        cat("Predictor variable, ", nm[2], ", has exactly two values_\n",
            "Run the t-test function to compare the corresponding\n",
            "group means of response variable ", nm[1], ".\n", sep="")
        .dash(60)
        cat("\n")
        f <- .tt.formula(my_formula, y, data, separate=FALSE,
                         Ynm, Xnm, X1nm, X2nm, ...)  # formula
        x <- f$x;  y <- f$y;
        Ynm <- f$Ynm;  Xnm <- f$Xnm;  X1nm <- f$X1nm;  X2nm <- f$X2nm 

        digits_d <- .max.dd(x) + 1
        if (digits_d == 1) digits_d <- 2
        options(digits_d=digits_d)  # .fmt requires if not specified

        # get lab_x_cex  lab_y_cex
        lab_cex <- getOption("lab_cex")
        lab_x_cex <- getOption("lab_x_cex")
        lab_x_cex <- ifelse(is.null(lab_x_cex), lab_cex, lab_x_cex)
        adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex

        # get variable labels if exist plus axes labels
        gl <- .getlabels(xlab=NULL, ylab=xlab, main=NULL, lab_x_cex=lab_x_cex,
                         graph.win=FALSE)  # # graphics window not yet set-up
        x.lab <- gl$yb
        #x.name <- gl$yn; x.lbl <- gl$yl; x.lab <- gl$yb

        # need to assign to out to avoid returned info displayed at console
        if (mean(x, na.rm=TRUE) > mean(y, na.rm=TRUE))
          out <- .TwoGroup(x, y,
            n1=NULL, n2=NULL, m1=NULL, m2=NULL, s1=NULL, s2=NULL,
            from.data=TRUE, Ynm, Xnm, X1nm, X2nm, 
            brief=FALSE, digits_d, 
            conf_level=0.95, alternative="two_sided",
            mmd=NULL, msmd=NULL, Edesired=NULL, 
            bw1="nrd", bw2="nrd", graph=TRUE, xlab=xlab,
            line_chart=FALSE, show_title=TRUE,
            pdf_file=NULL, width=5, height=5, ...)
        else {  # switch
          Xtmp <- X2nm
          X2nm <- X1nm
          X1nm <- Xtmp
          out <- .TwoGroup(x, y,
            n1=NULL, n2=NULL, m1=NULL, m2=NULL, s1=NULL, s2=NULL,
            from.data=TRUE, Ynm, Xnm, X1nm, X2nm, 
            brief=FALSE, digits_d, 
            conf_level=0.95, alternative="two_sided",
            mmd=NULL, msmd=NULL, Edesired=NULL, 
            bw1="nrd", bw2="nrd", graph=TRUE, xlab=xlab,
            line_chart=FALSE, show_title=TRUE,
            pdf_file=NULL, width=5, height=5, ...)
        }
      }

      else  {  # ANOVA
        cat("\n")
        .dash(60)
        cat("Run the ANOVA function to compare the corresponding \n",
            "group means of response variable ", nm[1], ".\n", sep="")
        .dash(60)
        ANOVA(my_formula, data, brief=brief, ...)
      }
    }  # all preds are categorical

    else cat("\nModels with numerical and categorical predictors not currently supported.\n")

  }  # dv is numerical

}
