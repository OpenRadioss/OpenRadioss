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
Chd|  CDKFCUM3                      source/elements/sh3n/coquedk/cdkfcum3.F
Chd|-- called by -----------
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDKFCUM3(JFT,JLT,PX2,PY2,PX3,PY3,
     1              R11,R12,R13,R21,R22,R23,R31,R32,R33,
     1              F11,F12,F13,F21,F22,F23,F31,F32,F33,
     2              M11,M12,M13,M21,M22,M23,M31,M32,M33)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      my_real
     .   PX2(*), PY2(*), PX3(*), PY3(*), 
     .   R11(*),R12(*),R13(*),R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*),
     .   F11(*), F12(*), F13(*),F21(*), F22(*), F23(*),
     .   F31(*), F32(*), F33(*),M11(*), M12(*), M13(*),
     .   M21(*), M22(*), M23(*),M31(*), M32(*), M33(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .     LX,LY,LZ
C-----------------------------------------------
C   L o c a l Coordinate-> Global 
C-----------------------------------------------
C
      DO I=JFT,JLT
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
C
       F11(I)=-F12(I)-F13(I)
       F21(I)=-F22(I)-F23(I)
       F31(I)=-F32(I)-F33(I)
      ENDDO
C
      DO I=JFT,JLT
       LX =R11(I)*M11(I)+ R12(I)*M21(I)
       LY =R21(I)*M11(I)+ R22(I)*M21(I)
       M31(I) =R31(I)*M11(I)+ R32(I)*M21(I)
       M11(I) =LX
       M21(I) =LY
       LX =R11(I)*M12(I)+ R12(I)*M22(I)
       LY =R21(I)*M12(I)+ R22(I)*M22(I)
       M32(I) =R31(I)*M12(I)+ R32(I)*M22(I)
       M12(I) =LX
       M22(I) =LY
       LX =R11(I)*M13(I)+ R12(I)*M23(I)
       LY =R21(I)*M13(I)+ R22(I)*M23(I)
       M33(I) =R31(I)*M13(I)+ R32(I)*M23(I)
       M13(I) =LX
       M23(I) =LY
      ENDDO
C
      RETURN
      END
