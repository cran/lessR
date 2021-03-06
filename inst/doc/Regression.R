## ---------------------------------------------------------------------------------------------------------------------
library("lessR")

## ----read-------------------------------------------------------------------------------------------------------------
d <- Read("Employee")

## ----fig.width=4.5, fig.height=4.5------------------------------------------------------------------------------------
reg_brief(Salary ~ Years + Pre)

## ---------------------------------------------------------------------------------------------------------------------
reg(Salary ~ Years + Pre)

## ---- fig.width=4.5, fig.height=4-------------------------------------------------------------------------------------
reg_brief(Salary ~ Years, rescale="z", plot_errors=TRUE)

## ---------------------------------------------------------------------------------------------------------------------
reg(Salary ~ Years, kfold=3)

## ---------------------------------------------------------------------------------------------------------------------
r <- reg(Salary ~ Years + Pre)

## ---------------------------------------------------------------------------------------------------------------------
r

## ---------------------------------------------------------------------------------------------------------------------
names(r)

## ---------------------------------------------------------------------------------------------------------------------
r$out_estimates

## ---------------------------------------------------------------------------------------------------------------------
r$coefficients

## ---------------------------------------------------------------------------------------------------------------------
r <- reg(Salary ~ Years, pred_rows="all", graphics=FALSE)
r$out_predict = sub(", ", ",", r$out_predict, fixed=TRUE)
dp <- read.table(text=r$out_predict)
dp[.(row.names(dp) == "Pham,Scott"),]

## ---------------------------------------------------------------------------------------------------------------------
cnt <- contr.sum(n=3)
cnt

## ----fig.width=4.5, fig.height=4--------------------------------------------------------------------------------------
d$Plan <- factor(d$Plan)
reg_brief(Salary ~ Plan, contrasts=list(Plan=cnt))

## ----fig.width=5------------------------------------------------------------------------------------------------------
reg_brief(Salary ~ 1, plot_errors=TRUE)

## ----likert, fig.width=4.5, fig.height=4.5----------------------------------------------------------------------------
dd <- Read("Mach4")
reg_brief(m10 ~ m02, data=dd)

## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------------
Logit(Gender ~ Salary)

## ---------------------------------------------------------------------------------------------------------------------
Logit(Gender ~ Years + Salary, prob_cut=c(.3, .5, .7))

