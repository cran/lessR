* Subprogram to reflect designated variables in the matrix.

      SUBROUTINE rflt(R,Label,NVC,NVOld)

      IMPLICIT NONE

      INTEGER NVC, NVOld
      INTEGER Label(NVC)
      double precision R(NVOld,NVOld)

      INTEGER J, LL

* Reflect the variables.

      DO LL = 1,NVC
        DO J = 1,NVOld
          IF (Label(LL) .NE. J) THEN
            R(J,Label(LL)) = -R(J,Label(LL))
            R(Label(LL),J) = R(J,Label(LL))
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END
