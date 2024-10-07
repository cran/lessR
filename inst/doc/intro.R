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

## -----------------------------------------------------------------------------
reg(Salary ~ Years + Pre)

## -----------------------------------------------------------------------------
d <- Read("StockPrice")
head(d)

## -----------------------------------------------------------------------------
d <- Read("StockPrice")
Plot(Month, Price, filter=(Company=="Apple"), area_fill="on")

## -----------------------------------------------------------------------------
Plot(Month, Price, by=Company)

## -----------------------------------------------------------------------------
Plot(Month, Price, time_unit="quarters", time_agg="mean")

## -----------------------------------------------------------------------------
d <- d[400:473,]
Plot(Month, Price, time_unit="months", time_agg="mean", time_ahead=24)

## -----------------------------------------------------------------------------
d <- Read("StockPrice", quiet=TRUE)
pivot(d, c(mean, sd), Price, by=Company)

