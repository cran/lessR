  skew <- function(x, na.rm=TRUE) {
  # skewness:  adjusted Fisher-Pearson standardized moment coefficient

    if (na.rm) x <- na.omit(x)
    n <- length(x)

    if (n > 2) {

      m <- mean(x)
      s <- sd(x)

      sk.sum <- 0
      for (j in 1:n)
        sk.sum <- sk.sum + (( (x[j]-m) / s)^3) 

      sk.coef <- n / ((n-1)*(n-2))
      sk <- sk.coef * sk.sum
    }

    else
      sk <- NA

    return(sk)

  }
