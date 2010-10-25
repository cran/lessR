stats.t.test <-
function(n1 = NULL, n2 = NULL,  m1 = NULL, m2 = NULL, s1 = NULL, s2 = NULL, 
           Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", conf.level = 0.95,
           digits = 2, ...) {


	if (is.null(n1) | is.null(n2)) stop("Specify a sample size for each group.")
	if (is.null(m1) | is.null(m2)) stop("Specify a mean for each group.")
	if (is.null(s1) | is.null(s2)) stop("Specify a standard deviation for each group.")
	
	cat("------------------------------------------------------------\n")
	cat("Compare", Ynm, "across", Xnm, "levels", X1nm, "and", X2nm, "\n")
	cat("------------------------------------------------------------\n\n")


	cat("------ Description ------\n\n")

	v1 <- s1^2
	v2 <- s2^2

	clpct <- paste(toString(round((conf.level)*100, 2)), "%", sep="")

	m1.out <- round(m1,digits)
	m2.out <- round(m2,digits)
	s1.out <- round(s1,digits)
	s2.out <- round(s2,digits)
	Xnmval <- paste(Xnm, X1nm)
	cat(Ynm, " for ", Xnmval, ":  n = ", n1, ",   mean = ", m1.out, ",   sd = ", s1.out, sep="", "\n")
	Xnmval <- paste(Xnm, X2nm)
	cat(Ynm, " for ", Xnmval, ":  n = ", n2, ",   mean = ", m2.out, ",   sd = ", s2.out, sep="", "\n\n")

	# sw
	df1 <- n1 - 1
	df2 <- n2 - 1
	swsq <- (df1*v1 + df2*v2) / (df1 + df2)
	sw <- sqrt(swsq)
	sw.out <- round(sw,digits)
	cat("Equal Group Variances Assumed, Within-group Standard Deviation:  ", sw.out, "\n\n")

	# mean diff and standardized mean diff
	mdiff <- m1 - m2
	mdiff.out <- round(mdiff,digits)
	cat("Mean Difference of ", Ynm, ":  " , mdiff.out, sep="", "\n\n")
	
	# smd
	d <- mdiff/sw
	d.out <- round(d,digits)
	cat("Standardized Mean Difference of ", Ynm, ", Cohen's d:  ", d.out, sep="", "\n")



	cat("\n\n------ Homogeneity of Variance------\n\n")

	cat("Note:  This hypothesis test performs poorly in non-normal samples and", "\n")
	cat("       the t-test is typically robust to violations of assumptions.", "\n")
	cat("       Use as a heuristic guide instead of interpreting literally.", "\n\n")


	# Homogeneity of Variance
	if (v1 >= v2) {
		vratio <- v1/v2
		vr <- paste(toString(round(v1,digits+1)), "/", toString(round(v2,digits+1)), sep="")
		df.num <- df1
		df.den <- df2
	}
	else {
		vratio <- v2/v1
		vr <- paste(toString(round(v2,digits+1)), "/", toString(round(v1,digits+1)), sep="")
		df.num <- df2
		df.den <- df1
	}

	v.out <- round(vratio,digits+1)

	p.var <- pf(vratio, df1=df.num, df2=df.den)
	p.var <- 2 * min(p.var, 1-p.var)  # adjust for two-sided test, results same as var.test{stats}
	pv.out <- round(p.var,min(4,digits+1))

	cat("Null hypothesis is equal variances of ", Ynm, ", i.e., homogeneous.", sep="", "\n")

	cat("Variance Ratio test:  F = ", vr, " = ", v.out, ",  df = ", df.num, ";", df.den, ",  p-value = ", 
			pv.out, sep="", "\n")



	cat("\n\n------ Inference ------\n\n")

	# t-test
	df <- df1 + df2
	sterr <- sw * sqrt(1/n1 + 1/n2)
	tcut <- qt((1-conf.level)/2, df=df, lower.tail=FALSE)
	E <- tcut * sterr
	lb <- mdiff - E
	ub <- mdiff + E
	tvalue <- round(mdiff/sterr, min(2,digits+1))
	pvalue <- round(2*pt(tvalue, df=df, lower.tail=FALSE),min(4,digits+1))
	
	cat("Standard Error of Mean Difference: SE = ", round(sterr,digits), "\n\n")
	cat("Hypothesis Test of 0 Mean Diff:  t = ", tvalue, ",  df = ", df, ",  p-value = ", pvalue, sep="", "\n\n")
	cat("Margin of Error for ", clpct, " Confidence Level:  ", round(E,digits), sep="", "\n")
	title <- " Confidence Interval for Mean Difference:  "
	cat(clpct, title, round(lb,digits), " to ", round(ub,digits), sep="", "\n\n")

	# smd confidence interval	
	check.MBESS <- suppressWarnings(require(MBESS, quietly=TRUE))
	if (check.MBESS) {
		cid <- ci.smd(smd=d, n.1=n1, n.2=n2, conf.level=conf.level)
		deltaL <- round(cid$Lower.Conf.Limit.smd,digits)
		deltaU <- round(cid$Upper.Conf.Limit.smd,digits)
		cat(clpct," Confidence Interval for smd:  ", deltaL, " to ", deltaU, sep="", "\n")
	}
	else {
		cat(">>> The confidence interval for smd requires package MBESS.", "\n")
		cat(">>> Confidence interval for smd not provided here, but all other output unaffected.", "\n")
		cat(">>> To get the MBESS package, run one time only: install.packages('MBESS')", "\n")
		cat(">>> IGNORE resulting 'Error in eval' error message below.", "\n")
	}

}