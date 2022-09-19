Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  ITRIMHPSORT                   source/output/cluster/itrimhpsort.F
Chd|-- called by -----------
Chd|        HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ITRIMHPSORT(TAB,LEN)
C-----------------------------------------------
c in place heap sort algorithm of integer table with elimination
c of double entries. Returns the sorted table and final length.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: LEN, TAB(LEN)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,VAL
c=======================================================================
      IF (LEN < 2) RETURN
      L  = LEN/2 + 1
      K = LEN
c
      !The index L will be decremented from its initial value during the
      !"hKing" (heap creation) phase. Once it reaches 1, the index K
      !will be decremented from its initial value down to 1 during the
      !"retKement-and-promotion" (heap selection) phase.

      DO   ! main heap sort loop
        IF (L > 1)THEN
          L=L-1
          VAL=TAB(L)
        ELSE
          VAL=TAB(K)
          TAB(K)=TAB(1)
          K=K-1
          IF (K == 1) THEN
            TAB(1)=VAL
            EXIT
          END IF
        END IF
        I=L
        J=L+L
        DO WHILE (J <= K)
          IF(J < K) THEN
            IF (TAB(J) < TAB(J+1)) J=J+1
          END IF
          IF (VAL < TAB(J)) THEN
            TAB(I)=TAB(J)
            I=J
            J=J+J
          ELSE
            J=K+1
          END IF
        END DO
        TAB(I)=VAL
      ENDDO  ! main heap sort loop
c
c-----------
c     eliminate double entries
c-----------
      J   = 1
      VAL = TAB(1)
      DO I=2,LEN
        IF (TAB(I) == VAL) CYCLE
        VAL = TAB(I)
        J   = J+1
        TAB(J) = VAL
      END DO
      LEN = J
c-----------
      RETURN
      END











