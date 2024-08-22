.plt.funnel <-
function (data.do, n.x_var, y.miss, cat.x, cat.y, run, date.ts, facet1.miss,
          y.unique) {

  # T.type specifies type of Trellis plot
  T.type <- NULL

  if (data.do  &&  n.x_var == 1) {
    if (y.miss) {  # y missing
      if (!cat.x) {  # continuous x
        if (!run && !date.ts) {  # not a run chart or ts
        Trellis <- TRUE
        T.type <- "cont"
       }
       else {  # run chart
          if (facet1.miss) {  # single panel
            Trellis <- FALSE
          }
          else {  # facet1 present
            Trellis <- TRUE
            T.type <- "cont_cont"  
          }
        }
      }  # end !cat.x
      else {  # single cat variable
        if (facet1.miss)
          Trellis <- FALSE
        else {  # facet1 activated
          Trellis <- TRUE
          T.type <- "dot"
        }
      }
    }  # end y.miss 

    else {  # y present
      if (!cat.x) {
        if (!cat.y) {  # two continuous vars
          if (facet1.miss)
            Trellis <- FALSE
          else {  # facet1 is set
            Trellis <- TRUE
            T.type <- "cont_cont"
          }
        }  # end !cat.y
        else {  # y is cat
          if (y.unique) {  # Cleveland
            if (facet1.miss)
              Trellis <- FALSE
            else {
              cat("\n"); stop(call.=FALSE, "\n","------\n",
                "Currently, Trellis plots not available for dot plots \n",
                "with unique values for the categorical variable\n\n")
            }
          }
          else {
            if (!facet1.miss) {
              Trellis <- TRUE  # cat-cont
              T.type <- "cont_cat"
            }
            else
              Trellis <- FALSE
          }
        }
      }
      else  {  # x is cat
        if (!cat.y) {  # y is num
          if (facet1.miss) {
            Trellis <- FALSE
          }
          else {
            cat("\n"); stop(call.=FALSE, "\n","------\n",
                "\n>>> Violin plot does not currently work with this",
                " variable order\n",
                "    Switch variable order to get violin plots\n\n", sep="")
          }
        }
        else { # y is cat
          Trellis <- FALSE
        }
      }  # x is cat
    }  # y present
  }  # end: data.do  &&  n.x_var == 1

  else {  # for all analysis of stat transformed data
    if (n.x_var == 1) {
      if (!cat.x) {
        Trellis <- FALSE
      }
      else {  # is cat.x
        if (facet1.miss) {
          Trellis <- FALSE
        }
        else {
          Trellis <- TRUE
          T.type <- "dot"
        }
      }  # end is cat.x
    }  # end n.x_var=1

    else {  # multiple x's
      Trellis <- FALSE
    }
  }

  return(list(Trellis=Trellis, T.type=T.type))

}
