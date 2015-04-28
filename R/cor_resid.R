.resid <- function(R, LblCut, NItems, NF) {
  # Compute Residuals from MGRP

   Clster <- integer(length=NItems)

  # Compute the cluster membership of each variable

   CNum <- 1
   for (I in 1:NItems) {
     if (I > LblCut[CNum,2]) CNum <- CNum + 1
     Clster[I] <- CNum
   }

  # Compute residual matrix, observed-predicted, with the product
  #  rule for external consistency

  Err <- 0
  for (I in 2:NItems) {
    FI <- NItems + Clster[I]
    for (J in 1:(I-1)) {
      FJ <- NItems + Clster[J]
      Pred <- R[I,FI] * R[FI,FJ] * R[FJ,J]
      if (I < J) R[J,I] <- R[I,J] - Pred
      if (I < J) Err <- Err + abs(R[J,I])
      if (J < I) R[I,J] <- R[J,I] - Pred
      if (J < I) Err <- Err + abs(R[I,J])
    }
  }

  # Fill in the upper triangle with residuals, and diagonal with 0.0

  for (I in 1:NItems) {
    for (J in 1:I) {
      R[J,I] <- R[I,J]
      if (I == J) R[I,I] <- 0.0
    }
  }

  return(R)
}
