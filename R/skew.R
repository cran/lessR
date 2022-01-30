  skew <- function(x, na.rm=TRUE) {
  # skewness:  adjusted Fisher-Pearson standardized moment coefficient

    if (na.rm) x <- na.omit(x)
    n <- length(x)

    if (n > 2) {

      m <- mean(x)
      s <- sd(x)

      g1 <- sum(((x-m) / s)^3)

      sk.coef <- n / ((n-1)*(n-2))
      sk <- sk.coef * g1
    }

    else
      sk <- NA

    return(sk)

  }
