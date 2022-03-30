## ----include=FALSE----------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library("lessR"))

## ----read-------------------------------------------------------------------------------------------------------------
d <- Read("Employee")

## ----labels-----------------------------------------------------------------------------------------------------------
l <- rd("Employee_lbl")
l

## ----hsform, dataTable, echo=FALSE, out.width='28%', fig.asp=.7, fig.align='center', out.extra='style="border-style: none"'----
knitr::include_graphics(system.file("img", "hsExplain.png", package="lessR"))

## ----hs, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Histogram of tablulated counts for the bins of Salary."----
Histogram(Salary)

## ----binwidth, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Customized histogram."-----------------------
Histogram(Salary, bin_start=35000, bin_width=14000)

## ----colors, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Customized histogram."-------------------------
Histogram(Salary, fill="reds", color="transparent")

## ----density, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Histogram with density plot."-----------------
Histogram(Salary, density=TRUE)

## ----VBS, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="VBS plot."----------------------------------------
Plot(Salary)

