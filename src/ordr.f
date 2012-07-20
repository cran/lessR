* Computational subroutine for Order.

      SUBROUTINE ordr(R,Label,NVC,IFirst)

      IMPLICIT NONE

      INTEGER NVC
      INTEGER Label(NVC), IFirst
      double precision R(NVC,NVC)

      INTEGER I, J, K, NV
      INTEGER IMax, Lbctad
      REAL RMax, VMax, VT

      NV = NVC

* If IFirst = 0 (default), best variable is chosen first.
* If IFirst gt 0, IFirst is the first variable chosen by user.

      DO I = 1,NV
        Label(I) = 0
      ENDDO

* Find the best variable for IFirst if not user specified.
      IF (IFirst .EQ. 0) THEN
        VMax = 0.0
        DO I = 1,NV
          VT = 0.0
          DO J = 1,NV
            VT = VT + R(I,J)**2
          ENDDO
          IF (VT .GT. VMax) THEN
            VMax = VT
            IMax = I
          ENDIF
        ENDDO
        IFirst = IMax
      ENDIF

* IFirst gives starting value.

      Label(1) = IFirst * 1000
      Label(IFirst) = Label(IFirst) + 1

      DO I = 2,NV
        RMax = 0.0
        K = Label(I-1) / 1000
        DO J = 1,NV
          Lbctad = Label(J) - (Label(J)/1000) * 1000
          IF (Lbctad .EQ. 0) THEN
            IF (ABS(R(K,J)) .GT. RMax) THEN
              RMax = ABS(R(K,J))
              IMax = J
            ENDIF
          ENDIF
        ENDDO
        Label(IMax) = Label(IMax) + I
        Label(I) = Label(I)+1000 * IMax
      ENDDO

      DO I = 1,NV
        Label(I) = Label(I) / 1000
      ENDDO

      RETURN
      END
