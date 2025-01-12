## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.width=5.5, fig.height=3,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lessR)

## ----read---------------------------------------------------------------------
d <- Read("Employee")

## ----labels-------------------------------------------------------------------
l <- rd("Employee_lbl")
l

## ----bcEx, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Bar chart of tablulated counts of employees in each department."----
BarChart(Dept)

## ----fig.width=4, fig.height=3.75, fig.align='center'-------------------------
BarChart(Dept, fill="darkred", color="black", transparency=.8,
         labels_color="black")

## ----fig.width=4, fig.height=3.5, fig.align='center'--------------------------
BarChart(Dept, theme="gray", labels="off", horiz=TRUE)

## ----hs, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Histogram of tablulated counts for the bins of Salary."----
Histogram(Salary)

## ----binwidth, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Customized histogram."----
Histogram(Salary, bin_start=35000, bin_width=14000, fill="reds")

## ----sp, fig.width=4----------------------------------------------------------
Plot(Years, Salary)

## ----spEnhance, fig.width=4---------------------------------------------------
Plot(Years, Salary, enhance=TRUE)

## ----x1, fig.height=3---------------------------------------------------------
Plot(Salary)

## ----spBubble, fig.width=4----------------------------------------------------
Plot(JobSat, Gender)

## ----fig.height=4.25, fig.width=5---------------------------------------------
ttest(Salary ~ Gender)

## ----fig.width=5--------------------------------------------------------------
ANOVA(breaks ~ tension * wool, data=warpbreaks)

## -----------------------------------------------------------------------------
d <- Read("Jackets")
Prop_test(Jacket, by=Bike)

## -----------------------------------------------------------------------------
d <- Read("Employee", quiet=TRUE)
reg(Salary ~ Years + Pre)

## -----------------------------------------------------------------------------
r <- reg(Salary ~ Years + Pre)
names(r)

## -----------------------------------------------------------------------------
r$out_fit

## -----------------------------------------------------------------------------
d <- Read("StockPrice")
head(d)

## -----------------------------------------------------------------------------
d <- Read("StockPrice")
Plot(Month, Price, filter=(Company=="Apple"), area_fill="on")

## -----------------------------------------------------------------------------
Plot(Month, Price, by=Company)

## -----------------------------------------------------------------------------
Plot(Month, Price, ts_unit="quarters", ts_agg="mean")

## -----------------------------------------------------------------------------
d <- d[400:473,]
Plot(Month, Price, ts_unit="months", ts_agg="mean", ts_ahead=24)

## -----------------------------------------------------------------------------
d <- Read("Mach4", quiet=TRUE)
l <- Read("Mach4_lbl", var_labels=TRUE)

## ----fig.width=3.5, fig.height=3.5--------------------------------------------
mycor <- cr(m01:m20)
R <- mycor$R

## -----------------------------------------------------------------------------
efa(R, n_factors=4)

## -----------------------------------------------------------------------------
MeasModel <- 
" 
   Deceit =~ m07 + m06 + m10 + m09 
   Trust =~ m12 + m05 + m13 + m01 
   Cynicism =~ m11 + m16 + m04 
   Flattery =~ m15 + m02 
"

## -----------------------------------------------------------------------------
d <- Read("StockPrice", quiet=TRUE)
pivot(d, c(mean, sd), Price, by=Company)

## -----------------------------------------------------------------------------
getColors("hues")

## ----echo=FALSE---------------------------------------------------------------
getColors("hues", output=TRUE)

## -----------------------------------------------------------------------------
getColors("blues")

## ----echo=FALSE---------------------------------------------------------------
getColors("blues", output=TRUE)

## -----------------------------------------------------------------------------
getColors("rusts", "blues")



## ----echo=FALSE---------------------------------------------------------------
getColors("rusts", "blues", output=TRUE)

