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
Chd|  MROTENS                       source/materials/mat_share/mrotens.F
Chd|-- called by -----------
Chd|        M25LAW                        source/materials/mat/mat025/m25law.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MROTENS (LFT,LLT,E1,E2,E3,
     .                   E4,DYZ,E6,R11,R12,
     .                   R13,R21,R22,R23,R31,
     .                   R32,R33)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT
      my_real E1(*),E2(*),E3(*),E4(*),DYZ(*),E6(*),
     .   R11(*), R12(*), R13(*), 
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   A11(MVSIZ), A12(MVSIZ), A13(MVSIZ), 
     .   A21(MVSIZ), A22(MVSIZ), A23(MVSIZ),
     .   A31(MVSIZ), A32(MVSIZ), A33(MVSIZ)
C-----------------------------------------------
      DO 200 I=LFT,LLT
      A11(I)=E1(I)*R11(I)+E4(I)*R21(I)+E6(I)*R31(I)
      A12(I)=E1(I)*R12(I)+E4(I)*R22(I)+E6(I)*R32(I)
      A13(I)=E1(I)*R13(I)+E4(I)*R23(I)+E6(I)*R33(I)
      A21(I)=E4(I)*R11(I)+E2(I)*R21(I)+DYZ(I)*R31(I)
      A22(I)=E4(I)*R12(I)+E2(I)*R22(I)+DYZ(I)*R32(I)
      A23(I)=E4(I)*R13(I)+E2(I)*R23(I)+DYZ(I)*R33(I)
      A31(I)=E6(I)*R11(I)+DYZ(I)*R21(I)+E3(I)*R31(I)
      A32(I)=E6(I)*R12(I)+DYZ(I)*R22(I)+E3(I)*R32(I)
      A33(I)=E6(I)*R13(I)+DYZ(I)*R23(I)+E3(I)*R33(I)
 200  CONTINUE
      DO 201 I=LFT,LLT
      E1(I)=R11(I)*A11(I)+R21(I)*A21(I)+R31(I)*A31(I)
      E2(I)=R12(I)*A12(I)+R22(I)*A22(I)+R32(I)*A32(I)
      E3(I)=R13(I)*A13(I)+R23(I)*A23(I)+R33(I)*A33(I)
      E4(I)=R11(I)*A12(I)+R21(I)*A22(I)+R31(I)*A32(I)
      DYZ(I)=R12(I)*A13(I)+R22(I)*A23(I)+R32(I)*A33(I)
      E6(I)=R11(I)*A13(I)+R21(I)*A23(I)+R31(I)*A33(I)
 201  CONTINUE
C
      RETURN
      END
Chd|====================================================================
Chd|  MROTENSNS                     source/materials/mat_share/mrotens.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MROTENSNS (LFT,LLT,
     .               DXX,DXY,DXZ,
     .               DYX,DYY,DYZ,
     .               DZX,DZY,DZZ,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT
      my_real
     .   DXY(*) ,DYX(*) ,DYZ(*) ,DZY(*) ,DZX(*) ,DXZ(*)
      my_real DXX(*),DYY(*),DZZ(*),
     .   R11(*), R12(*), R13(*), 
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   A11, A12, A13, 
     .   A21, A22, A23,
     .   A31, A32, A33
C-----------------------------------------------
      DO I=LFT,LLT
        A11 = DXX(I)*R11(I) + DXY(I)*R21(I) + DXZ(I)*R31(I)
        A12 = DXX(I)*R12(I) + DXY(I)*R22(I) + DXZ(I)*R32(I)
        A13 = DXX(I)*R13(I) + DXY(I)*R23(I) + DXZ(I)*R33(I)

        A21 = DYX(I)*R11(I) + DYY(I)*R21(I) + DYZ(I)*R31(I)
        A22 = DYX(I)*R12(I) + DYY(I)*R22(I) + DYZ(I)*R32(I)
        A23 = DYX(I)*R13(I) + DYY(I)*R23(I) + DYZ(I)*R33(I)

        A31 = DZX(I)*R11(I) + DZY(I)*R21(I) + DZZ(I)*R31(I)
        A32 = DZX(I)*R12(I) + DZY(I)*R22(I) + DZZ(I)*R32(I)
        A33 = DZX(I)*R13(I) + DZY(I)*R23(I) + DZZ(I)*R33(I)

        DXX(I) = R11(I)*A11 + R21(I)*A21 + R31(I)*A31
        DXY(I) = R11(I)*A12 + R21(I)*A22 + R31(I)*A32
        DXZ(I) = R11(I)*A13 + R21(I)*A23 + R31(I)*A33

        DYX(I) = R12(I)*A11 + R22(I)*A21 + R32(I)*A31
        DYY(I) = R12(I)*A12 + R22(I)*A22 + R32(I)*A32
        DYZ(I) = R12(I)*A13 + R22(I)*A23 + R32(I)*A33

        DZX(I) = R13(I)*A11 + R23(I)*A21 + R33(I)*A31
        DZY(I) = R13(I)*A12 + R23(I)*A22 + R33(I)*A32
        DZZ(I) = R13(I)*A13 + R23(I)*A23 + R33(I)*A33
      ENDDO
C
      RETURN
      END
