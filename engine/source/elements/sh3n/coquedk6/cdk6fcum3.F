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
Chd|  CDK6FCUM3                     source/elements/sh3n/coquedk6/cdk6fcum3.F
Chd|-- called by -----------
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDK6FCUM3(JFT,JLT,
     1              R11,R12,R13,R21,R22,R23,R31,R32,R33,
     1              F11,F12,F13,F21,F22,F23,F31,F32,F33,
     2              F14,F15,F16,F24,F25,F26,F34,F35,F36)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      my_real
     .   R11(*),R12(*),R13(*),R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*),
     .   F11(*), F12(*), F13(*),F21(*), F22(*), F23(*),
     .   F31(*), F32(*), F33(*),
     .   F14(*), F15(*), F16(*),F24(*), F25(*), F26(*),
     .   F34(*), F35(*), F36(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real LX,LY,LZ
C-----------------------------------------------
C   L o c a l Coordinate-> Global 
C-----------------------------------------------
C
      DO I=JFT,JLT
       LX =R11(I)*F11(I)+ R12(I)*F21(I)+R13(I)*F31(I)
       LY =R21(I)*F11(I)+ R22(I)*F21(I)+R23(I)*F31(I)
       LZ =R31(I)*F11(I)+ R32(I)*F21(I)+R33(I)*F31(I)
       F11(I) =LX
       F21(I) =LY
       F31(I) =LZ
       LX =R11(I)*F12(I)+ R12(I)*F22(I)+R13(I)*F32(I)
       LY =R21(I)*F12(I)+ R22(I)*F22(I)+R23(I)*F32(I)
       LZ =R31(I)*F12(I)+ R32(I)*F22(I)+R33(I)*F32(I)
       F12(I) =LX
       F22(I) =LY
       F32(I) =LZ
       LX =R11(I)*F13(I)+ R12(I)*F23(I)+R13(I)*F33(I)
       LY =R21(I)*F13(I)+ R22(I)*F23(I)+R23(I)*F33(I)
       LZ =R31(I)*F13(I)+ R32(I)*F23(I)+R33(I)*F33(I)
       F13(I) =LX
       F23(I) =LY
       F33(I) =LZ
      ENDDO
C
      DO I=JFT,JLT
       LX =R11(I)*F14(I)+ R12(I)*F24(I)+R13(I)*F34(I)
       LY =R21(I)*F14(I)+ R22(I)*F24(I)+R23(I)*F34(I)
       LZ =R31(I)*F14(I)+ R32(I)*F24(I)+R33(I)*F34(I)
       F14(I) =LX
       F24(I) =LY
       F34(I) =LZ
       LX =R11(I)*F15(I)+ R12(I)*F25(I)+R13(I)*F35(I)
       LY =R21(I)*F15(I)+ R22(I)*F25(I)+R23(I)*F35(I)
       LZ =R31(I)*F15(I)+ R32(I)*F25(I)+R33(I)*F35(I)
       F15(I) =LX
       F25(I) =LY
       F35(I) =LZ
       LX =R11(I)*F16(I)+ R12(I)*F26(I)+R13(I)*F36(I)
       LY =R21(I)*F16(I)+ R22(I)*F26(I)+R23(I)*F36(I)
       LZ =R31(I)*F16(I)+ R32(I)*F26(I)+R33(I)*F36(I)
       F16(I) =LX
       F26(I) =LY
       F36(I) =LZ
      ENDDO
C
      RETURN
      END
