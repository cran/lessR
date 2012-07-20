* Proportionality coefficients

      SUBROUTINE prop(R,Label,NVC,Diagon,Power)

      IMPLICIT NONE

      INTEGER NVC, Power, Diagon
      INTEGER Label(NVC)
      double precision R(NVC,NVC)

      double precision Diag(NVC)

      INTEGER I, J, K
      REAL D1, D2, X1, X2
      REAL RII, RJJ, RJI, RPower
  

* Get the float version of Power, RPower.

      IF (Power .EQ. 0) THEN
        RPower = 1.
      ELSE 
        RPower = FLOAT(Power) / 100.
      ENDIF

* Compute sum of squares for each column and store in Diag.

      DO I = 1,NVC
        Diag(I) = 0.
        DO J = 1,NVC
          Diag(I) = Diag(I) + R(I,J)**2
        ENDDO
      ENDDO

* Compute sum of cross-products and store in upper triangle.  Leave
*    the correlations in the lower triangle with diagonal unchanged.
*    R(I,J) is the cross-product, R(J,I) is the original correlation.

      DO I = 1,NVC
        DO J = I+1,NVC
          R(I,J) = 0
          DO K = 1,NVC
            IF (I .LE. K) X1 = R(K,I)
            IF (I .GT. K) X1 = R(I,K)
            IF (J .LE. K) X2 = R(K,J)
            IF (J .GT. K) X2 = R(J,K)
            R(I,J) = R(I,J) + X1*X2
          ENDDO
        ENDDO
      ENDDO

* Normalize cross products, i.e., obtain the proportionality coefs
*   excluding the diagonal.  If the diagonal is ignored, then
*   correspondingly reduce the appropriate sums of squares and
*   cross-products.

      DO I = 1,NVC
        DO J = I+1,NVC
          IF (Diagon .EQ. 0) THEN
            RII = R(I,I)
            RJJ = R(J,J)
            RJI = R(J,I)
            D1 = Diag(I) - (RII**2+RJI**2)
            D2 = Diag(J) - (RJJ**2+RJI**2)
            R(I,J) = R(I,J) - ((RII*RJI) + (RJJ*RJI))
          ELSEIF (Diagon .EQ. 1) THEN
            D1 = Diag(I)
            D2 = Diag(J)
          ENDIF
          R(I,J) = R(I,J) / (SQRT(D1*D2))
          R(I,J) = R(I,J)**RPower
          R(J,I) = R(I,J)
        ENDDO
      ENDDO

* Set the diagonal to 1.OO.

      DO I = 1,NVC
        R(I,I) = 1.00
      ENDDO

      RETURN
      END

