Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SMALLC3                       source/elements/solid/solide/smallc3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMALLC3(
     1   SAV,     OFFG,    OFF,     WXX,
     2   WYY,     WZZ,     DXX,     DYY,
     3   DZZ,     RHO,     RHOO,    DVOL,
     4   VOL,     SDV,     NEL,     ISMSTR,
     5   JLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JLAG
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*),OFF(*),WXX(*),WYY(*),WZZ(*),
     .   DXX(*),DYY(*),DZZ(*),RHO(*),RHOO(*),DVOL(*),VOL(*),SDV(*)
      DOUBLE PRECISION
     .  SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION X , Y, Z    ,DV   ,DV1
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES POUR SMALL STRAIN
C-----------------------------
      IF (ISMDISP > 0 .OR. ISMSTR == 11) THEN
       DO I=1,NEL
         DV=(DXX(I)+DYY(I)+DZZ(I))*DT1
         DV1=DV+SDV(I)
C-----------------------------
         RHO(I) = RHOO(I) - RHO(I)*DV1
         RHO(I) = MAX(RHO(I),EM30)
         DVOL(I)=VOL(I)*DV1
       ENDDO
      ELSEIF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3))THEN
C----- to be done outside the integrating point cycle      
      ENDIF
      IF (ISMDISP>0) THEN
      ELSEIF(ISMSTR<=4.AND.JLAG>0)THEN
C      ISMSTR=3 <==> ISMSTR=1
       DO I=1,NEL
        IF(OFFG(I)>ONE)THEN
         DV=(DXX(I)+DYY(I)+DZZ(I))*DT1+SDV(I)
         RHO(I) = RHOO(I) - RHO(I)*DV
         RHO(I) = MAX(RHO(I),EM30)
         DVOL(I)=VOL(I)*DV
        ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
