* Compute Residuals from MGRP.

      SUBROUTINE resid(R,LblCut,NItems,NF)

      IMPLICIT NONE

      INTEGER NItems, NF

      INTEGER LblCut(NF,2)
      double precision R(NItems+NF,NItems+NF)

      INTEGER I, J
      INTEGER CNum, FI, FJ
      INTEGER Clster(NItems)
      double precision Pred, Err

* Compute the cluster membership of each variable.

       CNum = 1
       DO I = 1,NItems
         IF (I .GT. LblCut(CNum,2)) CNum = CNum + 1
         Clster(I) = CNum
       ENDDO

* Compute residual matrix, observed-predicted, with the product
*  rule for external consistency.

      Err = 0
      DO I = 2,NItems
        FI = NItems + Clster(I)
        DO J = 1,I-1
          FJ = NItems + Clster(J)
          Pred = R(I,FI) * R(FI,FJ) * R(FJ,J)
          IF (I .LT. J) R(J,I) = R(I,J) - Pred
          IF (I .LT. J) Err = Err + ABS(R(J,I))
          IF (J .LT. I) R(I,J) = R(J,I) - Pred
          IF (J .LT. I) Err = Err + ABS(R(I,J))
        ENDDO
      ENDDO

* Fill in the upper triangle with residuals, and diagonal with 0.0.

      DO I = 1,NItems
      DO J = 1,I
          R(J,I) = R(I,J)
          IF (I .EQ. J) R(I,I) = 0.0
        ENDDO
      ENDDO

      RETURN
      END
