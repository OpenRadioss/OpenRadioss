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
Chd|  Q4ZERO2                       source/elements/solid_2d/quad4/q4zero2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4ZERO2(
     +   FY1, FZ1, FY2, FZ2, FY3, FZ3, FY4, FZ4, 
     +   FAY, FAZ, FAY1, FAY2, FAY3, FAY4,
     +   SIGM,EINTM,RHOM,QM,EPLASM,
     +   EPSDM,STIN,PP,G_PLA,G_EPSD,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER G_PLA,G_EPSD,NEL
C     REAL
      my_real
     .  FY1(*), FY2(*), FY3(*), FY4(*), FZ1(*), FZ2(*), FZ3(*), FZ4(*),
     .  FAY(*), FAZ(*), FAY1(*), FAY2(*), FAY3(*), FAY4(*),PP(*),
     .  EINTM(*),RHOM(*),QM(*),EPSDM(*),EPLASM(*),SIGM(NEL,6),STIN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=1,NEL
        SIGM(I,1) = ZERO
        SIGM(I,2) = ZERO
        SIGM(I,3) = ZERO
        SIGM(I,4) = ZERO
        SIGM(I,5) = ZERO
        SIGM(I,6) = ZERO
        RHOM(I)   = ZERO
c        EPLASM(I) = ZERO
        EINTM(I)  = ZERO
        QM(I)     = ZERO
        STIN(I)   = ZERO
        PP(I)     = ZERO
      ENDDO
      IF (G_PLA > 0) THEN
        DO I=1,NEL
          EPLASM(I) = ZERO
        ENDDO
      ENDIF
      IF (G_EPSD > 0) THEN
        DO I=1,NEL
          EPSDM(I) = ZERO
        ENDDO
      ENDIF
      DO I=1,NEL
        FY1(I)=ZERO
        FZ1(I)=ZERO
        FY2(I)=ZERO
        FZ2(I)=ZERO
        FY3(I)=ZERO
        FZ3(I)=ZERO
        FY4(I)=ZERO
        FZ4(I)=ZERO
        FAY(I)=ZERO
        FAZ(I)=ZERO
        FAY1(I)=ZERO
        FAY2(I)=ZERO
        FAY3(I)=ZERO
        FAY4(I)=ZERO
      ENDDO
C
      RETURN
      END
