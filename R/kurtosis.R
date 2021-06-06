  kurtosis <- function(x, na.rm=TRUE) {
  # SAS implementation

    if (na.rm) x <- na.omit(x)
    n <- length(x)

    if (n > 3) {

      m <- mean(x)

      kt.sum <- 0
      for (j in 1:n)
        kt.sum <- kt.sum + ( (x[j]-m)^4 )  # 4th moment

      kt.coef1 <- (n*(n+1)) / ((n-1)*(n-2)*(n-3))
      kt.coef2 <- 3 * ( ((n-1)^2) / ((n-2)*(n-3)) )
      kt <- ( kt.coef1 * (kt.sum/(var(x)^2)) ) - kt.coef2
    }

    else
      kt <- NA

    return(kt)

  }
