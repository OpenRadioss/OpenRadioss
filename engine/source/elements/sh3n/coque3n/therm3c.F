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
      !||    therm3c       ../engine/source/elements/sh3n/coque3n/therm3c.F
      !||--- called by ------------------------------------------------------
      !||    c3forc3       ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk   ../engine/source/elements/xfem/c3forc3_crk.F
      !||====================================================================
      SUBROUTINE THERM3C(NEL   ,PM    ,THK   ,IXTG   , 
     2                   PX1   ,PY1   ,PY2   ,AREA   ,DT     ,
     3                   TEMPNC,TEMPEL,DHEAT ,FPHI   ,THEACCFACT)
C-----------------------------------------------
C  calculates nodal thermic force from heat energy increment
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL 
      INTEGER, INTENT(IN) :: IXTG(NIXTG,*)
      my_real ,INTENT(IN) :: THEACCFACT
      my_real ,INTENT(IN) :: DT
      my_real :: AREA(NEL), PX1(MVSIZ), PX2(MVSIZ),PY1(MVSIZ), PY2(MVSIZ),
     .   TEMPNC(*), FPHI(MVSIZ,3), PM(*),DHEAT(NEL),
     .   THK(NEL),TEMPEL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      my_real :: CA,CB ,KC,PHIX,PHIY,A
!===========================================================================
        CA = PM(75)
        CB = PM(76)
!
        DO I=1,NEL
cc          ! element form functions
cc          PX1(I) = HALF*(X(2,IXTG(3,I)) - X(2,IXTG(4,I)))/AREA(I)
cc          PX2(I) = HALF*(X(2,IXTG(4,I)) - X(2,IXTG(2,I)))/AREA(I)
cc          PY1(I) = HALF*(X(1,IXTG(4,I)) - X(1,IXTG(3,I)))/AREA(I)
cc          PY2(I) = HALF*(X(1,IXTG(2,I)) - X(1,IXTG(4,I)))/AREA(I)
          
          KC = (CA + CB*TEMPEL(I))*DT / MAX(EM20,AREA(I))*THEACCFACT
          PHIX =  TEMPNC(IXTG(2,I))*PX1(I) - TEMPNC(IXTG(3,I))*PX1(I)
          
          PHIY =  TEMPNC(IXTG(2,I))*PY1(I)  + TEMPNC(IXTG(3,I))*PY2(I) -
     .            TEMPNC(IXTG(4,I))*(PY1(I) + PY2(I))
C     
          PHIX = KC*PHIX*THK(I)
          PHIY = KC*PHIY*THK(I)
C
C         nodal thermal force (flux)
C
          A = THIRD * DHEAT(I)
          FPHI(I,1) = A - PHIX*PX1(I) - PHIY*PY1(I)
          FPHI(I,2) = A + PHIX*PX1(I) - PHIY*PY2(I)
          FPHI(I,3) = A + PHIY*(PY1(I)+PY2(I))
       ENDDO
!------------       
      RETURN
      END
