## ----include=FALSE----------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library("lessR"))

## ----read-------------------------------------------------------------------------------------------------------------
d <- Read("Employee")

## ----labels-----------------------------------------------------------------------------------------------------------
l <- rd("Employee_lbl")

## ---------------------------------------------------------------------------------------------------------------------
ttest(Salary)

## ----fig.height=3.75, fig.width=5-------------------------------------------------------------------------------------
ttest(Salary, mu=52000)

## ---------------------------------------------------------------------------------------------------------------------
ttest(n=37, m=73795.557, s=21799.533, Ynm="Salary", mu=52000)

## ----fig.height=4.25, fig.width=5-------------------------------------------------------------------------------------
ttest(Salary ~ Gender)

## ----fig.height=4.25, fig.width=5-------------------------------------------------------------------------------------
tt_brief(Salary ~ Gender)

## ----fig.height=4.25, fig.width=5-------------------------------------------------------------------------------------
tt_brief(Pre, Post)

## ----fig.width=5------------------------------------------------------------------------------------------------------
ANOVA(breaks ~ tension, data=warpbreaks)

## ----fig.width=5------------------------------------------------------------------------------------------------------
av_brief(breaks ~ tension, data=warpbreaks)

## ----fig.width=5------------------------------------------------------------------------------------------------------
ANOVA(breaks ~ tension * wool, data=warpbreaks)

## ---------------------------------------------------------------------------------------------------------------------
data(warpbreaks)
dm <- pivot(warpbreaks, mean, breaks, c(tension, wool))
dm

## ----fig.width=5------------------------------------------------------------------------------------------------------
Plot(tension, breaks_mean, by=wool, segments=TRUE, size=2, data=dm,
     main="Cell Means")

## ---------------------------------------------------------------------------------------------------------------------
d <- read.csv(header=TRUE, text="
Person,sup1,sup2,sup3,sup4
p1,2,4,4,3
p2,2,5,4,6
p3,8,6,7,9
p4,4,3,5,7
p5,2,1,2,3
p6,5,5,6,8
p7,2,3,2,4")

## ---------------------------------------------------------------------------------------------------------------------
to("sup", 4)

## ---------------------------------------------------------------------------------------------------------------------
d <- reshape(d, direction="long",
        idvar="Person", varying=list(to("sup", 4)), 
        timevar="Supplement", v.names="Reps")

## ---------------------------------------------------------------------------------------------------------------------
row.names(d) <- NULL
d[1:10,]

## ----fig.width=6------------------------------------------------------------------------------------------------------
ANOVA(Reps ~ Supplement + Person)

