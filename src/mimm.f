* Multiple groups CFA

      SUBROUTINE mimm(R,LblCut,NVC,NF,Iter,Alpha,Omega)

      IMPLICIT NONE

      INTEGER NVC
      INTEGER NF, Iter, LblCut(NF,2)
      double precision R(NVC+NF,NVC+NF)
      double precision Alpha(NF), Omega(NF)

      REAL Lam, Unique, SumLam, SumUnq

      INTEGER I, IFac, It, J, K, L, LI, N, NItems, FI
      REAL DSum, FacSD, RMean, XN
      REAL OldFF, OldII

      NItems = NVC

* Compute item-factor and fac-fac covariances by calculating sums.
*   First calculate the item-fac covariances in I = 1..NItems by summing
*   within-cluster columns. Once these are computed, calculate the
*   factor-factor covariances in I = NItems+1..NF part of first loop by
*   applying the same formula to the recently computed item-factor
*   covariances.

* R(L,I): at this point these are covariances, not correlations.
* L: ordinal position of the Jth fac in the list of items and factors.

      DO I = 1,NItems+NF
        DO J = 1,NF
          L = NItems + J
          R(I,L) = 0.0
          DO K = LblCut(J,1),LblCut(J,2)
            R(I,L) = R(I,L) + R(I,K)
          ENDDO
          R(L,I) = R(I,L)
        ENDDO
      ENDDO

* Compute coefficient alpha for each group.
* N: Number of items in the Jth group.
* R(L,L): Jth factor variance, computed in previous paragraph.
* DSum: sum of the diagonal of the Jth group.
* RMean: mean of the off-diagonal correlations of Jth group.

      DO J = 1,NF
        L = NItems + J
        N = LblCut(J,2) - LblCut(J,1) + 1
        IF (N .EQ. 1) THEN
          Alpha(J) = 1.0
        ELSE
          DSum = 0.0
          DO I = LblCut(J,1),LblCut(J,2)
            DSum = DSum + R(I,I)
          ENDDO
          XN = FLOAT(N)
          RMean = (R(L,L)-DSum) / (XN*(XN-1.0))
          Alpha(J) = (XN*RMean) / ((XN-1)*RMean+1.0)
        ENDIF
      ENDDO


* Compute communalities of the Jth group by iterating.
* Iter: Number of iterations.
* OldFF: Factor variance at the beginning of current iteration.
* OldII: Communality at the beginning of current iteration.

      IF (Iter .GE. 0) THEN

        DO J = 1,NF
          IF (Alpha(J) .GT. 0) THEN
            DO It = 1,Iter
              L = NItems + J
              OldFF = R(L,L)
              R(L,L) = 0.0
              DO I = LblCut(J,1),LblCut(J,2)
                OldII = R(I,I)
                R(I,I) = R(I,L)**2 / OldFF
                R(I,L) = R(I,L) + (R(I,I) - OldII)
                R(L,L) = R(L,L) + R(I,L)
              ENDDO
            ENDDO
          ENDIF
        ENDDO

      ENDIF

* Standardize the item-factor and factor-factor covariances.
* Since the items are already standardized, item-factor covariances are
*   standardized by dividing by FacSD. This is accomplished in Loop 140
*   as well as beginning the standardization of the factor-factor
*   covariances.  The second division by the remaining FacSD for the
*   factor-factor covariances is accomplished in Loop 150.

      DO J = 1,NF
        L = NItems + J
        FacSD = SQRT(R(L,L))
        DO I = 1,L
          R(I,L) = R(I,L) / FacSD
          R(L,I) = R(I,L)
        ENDDO
        DO I = J,NF
          LI = NItems + I
          R(L,LI) = R(L,LI) / FacSD
          R(LI,L) = R(L,LI)
        ENDDO
      ENDDO

* Compute the Joreskog (1971) reliability coef, which is Omega.
* Omega uses each items communality as a reliability estimate.
* Alpha assumes equal item reliabilities.

      DO IFac = 1,NF
        FI = NItems + IFac
        SumLam = 0
        SumUnq = 0
        DO J = LblCut(IFac,1),LblCut(IFac,2)
          Lam = R(FI,J)
          Unique = 1 - Lam**2
          SumLam = SumLam + Lam
          SumUnq = SumUnq + Unique
        ENDDO
        Omega(IFac) = (SumLam**2) / (SumLam**2 + SumUnq)
        IF (Iter .EQ. 0) Omega(IFac) = 1.0
      ENDDO

      RETURN
      END

