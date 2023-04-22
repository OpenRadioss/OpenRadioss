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
Chd|  S6CKGEO3                      source/elements/thickshell/solide6c/s6ckgeo3.F
Chd|-- called by -----------
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|-- calls ---------------
Chd|        S6CKEH3                       source/elements/thickshell/solide6c/s6ckeh3.F
Chd|====================================================================
      SUBROUTINE S6CKGEO3(
     1   SIG,     VOL,     PXC1,    PXC2,
     2   PXC3,    PXC4,    PYC1,    PYC2,
     3   PYC3,    PYC4,    K11,     K12,
     4   K13,     K14,     K15,     K16,
     5   K22,     K23,     K24,     K25,
     6   K26,     K33,     K34,     K35,
     7   K36,     K44,     K45,     K46,
     8   K55,     K56,     K66,     NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),  
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),  
     .   K11(9,*),K12(9,*),K13(9,*),K14(9,*) ,K15(9,*)   ,
     .   K16(9,*),K22(9,*) ,K23(9,*)  ,K24(9,*),K25(9,*),
     .   K26(9,*),K33(9,*),K34(9,*),K35(9,*),K36(9,*) ,
     .   K44(9,*),K45(9,*),K46(9,*) ,K55(9,*),K56(9,*),
     .   K66(9,*),VOL(*),SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .   FXX(MVSIZ),FYY(MVSIZ),FXY(MVSIZ),
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),
     .   PX4(MVSIZ),PX5(MVSIZ),PX6(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),
     .   PY4(MVSIZ),PY5(MVSIZ),PY6(MVSIZ)
C-----------------------------------------------
       DO I=1,NEL
        FXX(I)=SIG(I,1)*VOL(I)
        FYY(I)=SIG(I,2)*VOL(I)
        FXY(I)=SIG(I,4)*VOL(I)
       ENDDO
C ----------PXI,PYI,PZI-------------------------------------
      DO I=1,NEL
       PX1(I) = PXC1(I)-PXC4(I) 
       PX2(I) = PXC2(I)-PXC4(I) 
       PX3(I) = PXC3(I)-PXC4(I) 
       PX4(I) = PXC1(I)+PXC4(I) 
       PX5(I) = PXC2(I)+PXC4(I) 
       PX6(I) = PXC3(I)+PXC4(I) 
      ENDDO
      DO I=1,NEL
       PY1(I) = PYC1(I)-PYC4(I) 
       PY2(I) = PYC2(I)-PYC4(I)
       PY3(I) = PYC3(I)-PYC4(I) 
       PY4(I) = PYC1(I)+PYC4(I) 
       PY5(I) = PYC2(I)+PYC4(I) 
       PY6(I) = PYC3(I)+PYC4(I) 
      ENDDO
      CALL S6CKEH3(1,NEL,
     .   PX1, PY1 ,PX1, PY1, FXX, FYY ,FXY,K11)
      CALL S6CKEH3(1,NEL,
     .   PX1, PY1 ,PX2, PY2, FXX, FYY ,FXY,K12)
      CALL S6CKEH3(1,NEL,
     .   PX1, PY1 ,PX3, PY3, FXX, FYY ,FXY,K13)
      CALL S6CKEH3(1,NEL,
     .   PX1, PY1 ,PX4, PY4, FXX, FYY ,FXY,K14)
      CALL S6CKEH3(1,NEL,
     .   PX1, PY1 ,PX5, PY5, FXX, FYY ,FXY,K15)
      CALL S6CKEH3(1,NEL,
     .   PX1, PY1 ,PX6, PY6, FXX, FYY ,FXY,K16)
      CALL S6CKEH3(1,NEL,
     .   PX2, PY2 ,PX2, PY2, FXX, FYY ,FXY,K22)
      CALL S6CKEH3(1,NEL,
     .   PX2, PY2 ,PX3, PY3, FXX, FYY ,FXY,K23)
      CALL S6CKEH3(1,NEL,
     .   PX2, PY2 ,PX4, PY4, FXX, FYY ,FXY,K24)
      CALL S6CKEH3(1,NEL,
     .   PX2, PY2 ,PX5, PY5, FXX, FYY ,FXY,K25)
      CALL S6CKEH3(1,NEL,
     .   PX2, PY2 ,PX6, PY6, FXX, FYY ,FXY,K26)
      CALL S6CKEH3(1,NEL,
     .   PX3, PY3 ,PX3, PY3, FXX, FYY ,FXY,K33)
      CALL S6CKEH3(1,NEL,
     .   PX3, PY3 ,PX4, PY4, FXX, FYY ,FXY,K34)
      CALL S6CKEH3(1,NEL,
     .   PX3, PY3 ,PX5, PY5, FXX, FYY ,FXY,K35)
      CALL S6CKEH3(1,NEL,
     .   PX3, PY3 ,PX6, PY6, FXX, FYY ,FXY,K36)
      CALL S6CKEH3(1,NEL,
     .   PX4, PY4 ,PX4, PY4, FXX, FYY ,FXY,K44)
      CALL S6CKEH3(1,NEL,
     .   PX4, PY4 ,PX5, PY5, FXX, FYY ,FXY,K45)
      CALL S6CKEH3(1,NEL,
     .   PX4, PY4 ,PX6, PY6, FXX, FYY ,FXY,K46)
      CALL S6CKEH3(1,NEL,
     .   PX5, PY5 ,PX5, PY5, FXX, FYY ,FXY,K55)
      CALL S6CKEH3(1,NEL,
     .   PX5, PY5 ,PX6, PY6, FXX, FYY ,FXY,K56)
      CALL S6CKEH3(1,NEL,
     .   PX6, PY6 ,PX6, PY6, FXX, FYY ,FXY,K66)
C
      RETURN
      END
