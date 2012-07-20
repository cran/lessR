* Reorder R

      SUBROUTINE rrdr(R,Label,NVC,NVOld)

      IMPLICIT NONE

      INTEGER NVC, NVOld
      INTEGER Label(NVC)
      double precision R(NVOld,NVOld)

      INTEGER I, J, II, JJ
      double precision Temp(NVOld)

* Save the diagonal of R.

      DO I = 1,NVOld
        Temp(I) = R(I,I)
      ENDDO

* Reorder lower triangle and diagonal.
* Label(I): ordinal position of Label(I) in R before reordering.

      DO I = 1,NVC
        DO J = I,NVC
          II = Label(I)
          JJ = Label(J)
          IF (II .LT. JJ) R(J,I) = R(II,JJ)
          IF (II .EQ. JJ) R(J,I) = Temp(II)
          IF (II .GT. JJ) R(J,I) = R(JJ,II)
        ENDDO
      ENDDO

* Set upper triangle equal to lower triangle.

      DO I = 1,NVC
        DO J = I,NVC
          R(I,J) = R(J,I)
        ENDDO
      ENDDO

      RETURN
      END

