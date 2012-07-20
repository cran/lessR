
* Sort within each group by the group factor loading (option).
*   For each group, the item-factor correlations are placed in Set, and
*   DecSrt is called.  Passed to DecSrt are the beginning addresses of
*   the groups of correlations and labels to be sorted, and the number
*   of correlations and labels used.
* After sorting, reorder the variables.

      SUBROUTINE srt(R,Label,LblCut,NItems,NF)

* Sort group of labels in descending order by values passed in A.

      IMPLICIT NONE

      INTEGER NF, NItems
      INTEGER Label(NItems), LblCut(NF,2)
      double precision R(NItems+NF,NItems+NF)

      double precision B(NItems)
      double precision set(NItems)

      INTEGER N, N1, N2
      INTEGER I, II, J, JJ, IRow, ITemp, LN
      double precision Temp
      LOGICAL More

      integer kk

      DO I = 1,NF
        IRow = NItems + I
        N1 = LblCut(I,1)
        N2 = LblCut(I,2)
        DO J = N1, N2
          Set(J) = R(IRow,J)
          B(J) = R(IRow,J)
        ENDDO

* Place A in B in order to protect A.

        N = N2 - N1 + 1
        DO II = 1,N
          JJ = N1 + (II - 1)
          B(II) = Set(JJ)
        ENDDO

* Sort B, carrying labels along (bubble sort).

        LN = N - 1
        More = .TRUE.
  20    IF (.NOT.More .OR. LN.EQ.0) GOTO 15
        More = .FALSE.
        DO II = 1,LN
          JJ = N1 + (II - 1)
          IF (B(JJ) .LT. B(JJ+1)) THEN
            ITemp = Label(JJ)
            Label(JJ) = Label(JJ+1)
            Label(JJ+1) = ITemp
            Temp = B(JJ)
            B(JJ) = B(JJ+1)
            B(JJ+1) = Temp
            More = .TRUE.
          ENDIF
        ENDDO
        LN = LN - 1
        GOTO 20
  15    CONTINUE

      ENDDO


      RETURN
      END

