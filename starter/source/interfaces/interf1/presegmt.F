Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    presegmt       ../starter/source/interfaces/interf1/presegmt.F
!||--- called by ------------------------------------------------------
!||    lecint         ../starter/source/interfaces/interf1/lecint.F
!||    prescrint      ../starter/source/interfaces/interf1/prescrint.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE PRESEGMT(IRECT,NODES,NRT,NNO,NST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)  :: NRT, NNO, IRECT(4,*), NODES(*)
      INTEGER, INTENT(OUT) :: NST 
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K
      INTEGER, DIMENSION(:), ALLOCATABLE :: NTAG
      INTEGER, DIMENSION(:), ALLOCATABLE :: NSEG
C=======================================================================
      CALL MY_ALLOC(NSEG,NNO+1)
      CALL MY_ALLOC(NTAG,NUMNOD)

      NST  = 0
      NSEG(1) = 1
      NSEG(2:NNO+1) = 0
C
      DO I=1,NNO
        NTAG(NODES(I))=I
      ENDDO
C
      DO I=1,NRT
        DO J=1,4
          IF (IRECT(J,I) /= 0) THEN
            K = NTAG(IRECT(J,I))+1
            NSEG(K)=NSEG(K)+1
          ENDIF
        ENDDO
      ENDDO
C
      DO I=1,NNO
        NST = NST + NSEG(I+1)
      ENDDO
C
      DEALLOCATE(NTAG)
      DEALLOCATE(NSEG)
      RETURN
      END
