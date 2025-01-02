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
C
      !||====================================================================
      !||    inint0   ../starter/source/interfaces/interf1/inint0.F
      !||--- called by ------------------------------------------------------
      !||    iniend   ../starter/source/interfaces/inter3d1/iniend.F
      !||    inint2   ../starter/source/interfaces/inter2d1/inint2.F
      !||    inint3   ../starter/source/interfaces/inter3d1/inint3.F
      !||====================================================================
      SUBROUTINE ININT0(X,IRECT,NSEG,NOD2SEG,NSV,MSR,ILOC,NMN,NSN,NRT, SIRECT, S_N2SEG)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: SIRECT !< array size IRECT(SIRECT)
      INTEGER,INTENT(IN) :: S_N2SEG !< array size NOD2SEG(S_N2SEG)
      INTEGER, INTENT(IN) :: NMN, NSN, NRT
      INTEGER, INTENT(IN) :: IRECT(4,SIRECT/4)
      INTEGER, INTENT(IN) :: NSEG(*), NSV(*), MSR(*)
      INTEGER, INTENT(INOUT) :: NOD2SEG(S_N2SEG)
      INTEGER, INTENT(INOUT) :: ILOC(*)
      my_real, INTENT(IN) :: X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, JJ, J, KK, L1, L2, L
      my_real LOWER_DIST, DIST
      LOGICAL lFOUND
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(NRT == 0) RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      KK = 0
C  
C   Compute nearest secnd node from main node (stored in ILOC)
C      inter type1 : main<->secnd ; nearest main node (lag) from ale node (ale)
      DO II=1,NMN
         I=MSR(II)
         LOWER_DIST = EP20
         DO JJ=1,NSN
           J=NSV(JJ)
           DIST = (X(1,I)-X(1,J))**2 + (X(2,I)-X(2,J))**2 + (X(3,I)-X(3,J))**2
           IF(DIST <= LOWER_DIST)THEN
             LOWER_DIST = DIST
             ILOC(II)= JJ ! nearest secnd node more main node ii
           ENDIF
         ENDDO
      ENDDO
C  
C   Compute NOD2SEG : List of Connected Segments to secnd nodes
C       inter1 : main <-> secnd ; connected main segment to main nodes (lag)
      DO I=1,NRT
         DO J=1,4
           IF(IRECT(J,I) /= 0) THEN
             lFOUND=.FALSE.
             DO KK=1,NSN
               IF(NSV(KK) == IRECT(J,I)) THEN
                 lFOUND = .TRUE.
                 EXIT
               ENDIF
             ENDDO

             IF(lFOUND)THEN
               II=0
               L1=NSEG(KK)
               L2=NSEG(KK+1)-1
               DO L=L1,L2
                 IF(NOD2SEG(L) /= 0) II=II+1
               ENDDO
               NOD2SEG(L1+II)=I
             ENDIF
           ENDIF
         ENDDO
      ENDDO
C
      RETURN
      END
