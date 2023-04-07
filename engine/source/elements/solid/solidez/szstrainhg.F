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
Chd|  SZSTRAINHG                    source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZSTRAINHG(
     1   JR_1,    JS_1,    JT_1,    STRHG,
     2   NEL,     HGX1,    HGX2,    HGX3,
     3   HGX4,    HGY1,    HGY2,    HGY3,
     4   HGY4,    HGZ1,    HGZ2,    HGZ3,
     5   HGZ4,    NU,      NU1)
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
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   HGX1(MVSIZ), HGX2(MVSIZ), HGX3(MVSIZ), HGX4(MVSIZ),
     .   HGY1(MVSIZ), HGY2(MVSIZ), HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ1(MVSIZ), HGZ2(MVSIZ), HGZ3(MVSIZ), HGZ4(MVSIZ),
     .   JR_1(*),JS_1(*),JT_1(*) ,STRHG(NEL,3,6),NU(*),NU1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,IR,IS,IT,IP
      my_real
     .   HGX(MVSIZ,4), HGY(MVSIZ,4), HGZ(MVSIZ,4),
     .   KSI,ETA,ZETA,COEF,NJR_1(MVSIZ),NJS_1(MVSIZ),NJT_1(MVSIZ),
     .   JSR_1(MVSIZ),JRS_1(MVSIZ),JTR_1(MVSIZ),JRT_1(MVSIZ),JTS_1(MVSIZ),
     .   JST_1(MVSIZ),F1,F2,F3,F1X,F2Y,F3Z
C-----------------------------------------------
      COEF = EIGHT
      DO I=1,NEL
       NJR_1(I) = COEF*JR_1(I)*DT1
       NJS_1(I) = COEF*JS_1(I)*DT1
       NJT_1(I) = COEF*JT_1(I)*DT1
      ENDDO
      DO I=1,NEL
       HGX(I,1) = NJR_1(I)*HGX1(I)
       HGX(I,2) = NJR_1(I)*HGX2(I)
       HGX(I,3) = NJR_1(I)*HGX3(I)
       HGX(I,4) = NJR_1(I)*HGX4(I)
       HGY(I,1) = NJS_1(I)*HGY1(I)
       HGY(I,2) = NJS_1(I)*HGY2(I)
       HGY(I,3) = NJS_1(I)*HGY3(I)
       HGY(I,4) = NJS_1(I)*HGY4(I)
       HGZ(I,1) = NJT_1(I)*HGZ1(I)
       HGZ(I,2) = NJT_1(I)*HGZ2(I)
       HGZ(I,3) = NJT_1(I)*HGZ3(I)
       HGZ(I,4) = NJT_1(I)*HGZ4(I)
      ENDDO
C------ STRHG(NEL,3,j) : J=1,3: term ksi,eta,zeta of Eii; J=4: shear;   
C------                  J=5: mode 4 HGi4; J=6: nu*HGi4;   
         DO I=1,NEL
          F1X = HGX(I,4)
          F2Y = HGY(I,4)
          F3Z = HGZ(I,4)
          STRHG(I,1,1)= STRHG(I,1,1)-NU1(I)*(HGY(I,3)+HGZ(I,2))
          STRHG(I,1,2)= STRHG(I,1,2)+HGX(I,3)
          STRHG(I,1,3)= STRHG(I,1,3)+HGX(I,2)
          STRHG(I,1,4)= STRHG(I,1,4)+HGX1(I)*NJS_1(I)+HGY2(I)*NJR_1(I)
          STRHG(I,1,5)= STRHG(I,1,5)+F1X
          STRHG(I,1,6)= STRHG(I,1,6)-NU(I)*F1X
          STRHG(I,2,1)= STRHG(I,2,1)+HGY(I,3)
          STRHG(I,2,2)= STRHG(I,2,2)-NU1(I)*(HGX(I,3)+HGZ(I,1))
          STRHG(I,2,3)= STRHG(I,2,3)+HGY(I,1)
          STRHG(I,2,4)= STRHG(I,2,4)+HGX1(I)*NJT_1(I)+HGZ3(I)*NJR_1(I)
          STRHG(I,2,5)= STRHG(I,2,5)+F2Y
          STRHG(I,2,6)= STRHG(I,2,6)-NU(I)*F2Y
          STRHG(I,3,1)= STRHG(I,3,1)+HGZ(I,2)
          STRHG(I,3,2)= STRHG(I,3,2)+HGZ(I,1)
          STRHG(I,3,3)= STRHG(I,3,3)-NU1(I)*(HGX(I,2)+HGY(I,1))
          STRHG(I,3,4)= STRHG(I,3,4)+HGY2(I)*NJT_1(I)+HGZ3(I)*NJS_1(I)
          STRHG(I,3,5)= STRHG(I,3,5)+F3Z
          STRHG(I,3,6)= STRHG(I,3,6)-NU(I)*F3Z
         END DO
C      
      RETURN
      END
