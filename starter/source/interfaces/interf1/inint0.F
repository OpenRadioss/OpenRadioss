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
C
Chd|====================================================================
Chd|  ININT0                        source/interfaces/interf1/inint0.F
Chd|-- called by -----------
Chd|        INIEND                        source/interfaces/inter3d1/iniend.F
Chd|        ININT2                        source/interfaces/inter2d1/inint2.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ININT0(X,IRECT,NSEG,NOD2SEG,NSV,MSR,ILOC,NMN,NSN,NRT)
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
      INTEGER, INTENT(IN)  :: NMN, NSN, NRT
      INTEGER, INTENT(IN)  :: IRECT(4,*), NSEG(*), NSV(*), MSR(*)
      INTEGER, INTENT(OUT) :: NOD2SEG(*), ILOC(*)
      my_real, INTENT(IN)  :: X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, JJ, J, K, KK, L1, L2, L
      my_real CMS, DMS
C
      IF(NRT.EQ.0) RETURN
Cfa
C   Compute ILOC nearest slave node from master node
      DO II=1,NMN
         I=MSR(II)
         CMS=EP30
         DO JJ=1,NSN
            J=NSV(JJ)
            DMS=(X(1,I)-X(1,J))**2+(X(2,I)-X(2,J))**2+(X(3,I)-X(3,J))**2
            IF(DMS.GT.CMS) CYCLE
            CMS=DMS
            ILOC(II)= JJ
         ENDDO
      ENDDO
Cfa
C   Compute NOD2SEG : List of Connected Segments to slave nodes
      DO I=1,NRT
         DO J=1,4
            IF(IRECT(J,I).EQ.0) CYCLE
            DO K=1,NSN
               KK=K
               IF(NSV(K).EQ.IRECT(J,I)) EXIT
            ENDDO
            II=0
            L1=NSEG(KK)
            L2=NSEG(KK+1)-1
            DO L=L1,L2
               IF(NOD2SEG(L).NE.0) II=II+1
            ENDDO
            NOD2SEG(L1+II)=I
         ENDDO
      ENDDO
C
      RETURN
      END
