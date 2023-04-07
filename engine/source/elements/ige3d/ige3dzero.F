Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  IGE3DZERO                     source/elements/ige3d/ige3dzero.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IGE3DZERO(
     1   NCTRL,   VOLM,    SIGM,    EINTM,
     2   RHOM,    QM,      FX,      FY,
     3   FZ,      BtDBA,   STIG,    MASS,
     4   MMUNK,   AFACE,   DETMIN,  EPLASM,
     5   EPSDG,   G_PLA,   G_EPSD,  NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCTRL,G_PLA,G_EPSD,NEL
C   REAL ou REAL*8
      my_real
     .   VOLM(*),SIGM(NEL,6),EINTM(*),RHOM(*),
     .   QM(*), FX(MVSIZ,*),FY(MVSIZ,*),
     .   FZ(MVSIZ,*),BtDBA(3*NCTRL,*),STIG(MVSIZ,*),
     .   MASS(NCTRL,MVSIZ),MMUNK(MVSIZ),AFACE(6,*),
     .   DETMIN(MVSIZ),EPLASM(*),EPSDG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, ITEST
C-----------------------------------------------
      DO I=1,NEL
       SIGM(I,1) = ZERO
       SIGM(I,2) = ZERO
       SIGM(I,3) = ZERO
       SIGM(I,4) = ZERO
       SIGM(I,5) = ZERO
       SIGM(I,6) = ZERO
       RHOM(I)   = ZERO
       EINTM(I)  = ZERO
       QM(I)     = ZERO
       VOLM(I) = ZERO
       DO J=1,NCTRL
        FX(I,J)=ZERO
        FY(I,J)=ZERO
        FZ(I,J)=ZERO
        STIG(I,J)= ZERO
       ENDDO
      ENDDO

      IF(IDTMIN(101)==1)THEN
       DO I=1,NEL
        DO J=1,NCTRL
         MMUNK(I)=EP10
         MASS(J,I)=ZERO
        ENDDO
       ENDDO
      ENDIF 

      IF(IDTMIN(101)==2)THEN
       DO I=1,NEL
        AFACE(1,I)=ZERO
        AFACE(2,I)=ZERO
        AFACE(3,I)=ZERO
        AFACE(4,I)=ZERO
        AFACE(5,I)=ZERO
        AFACE(6,I)=ZERO
        DETMIN(I)=EP10
       ENDDO
      ENDIF

      IF (G_PLA > 0) THEN
        DO I=1,NEL
          EPLASM(I)=ZERO  
        ENDDO
      ENDIF
      IF (G_EPSD > 0) THEN
        DO I=1,NEL
          EPSDG(I)=ZERO
        ENDDO
      ENDIF

      RETURN
      END
