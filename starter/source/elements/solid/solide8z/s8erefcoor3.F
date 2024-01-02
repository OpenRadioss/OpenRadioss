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
Chd|  S8EREFCOOR3                   source/elements/solid/solide8z/s8erefcoor3.F
Chd|-- called by -----------
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EREFCOOR3(SAV,NPE,NEL,
     .     XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .     YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .     ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
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
      INTEGER , INTENT(IN) :: NPE,NEL
      DOUBLE PRECISION , DIMENSION(NEL,3*(NPE-1)),INTENT(IN) :: SAV     
      DOUBLE PRECISION , DIMENSION(MVSIZ), INTENT(OUT) ::
     .   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE1,N,N2,N3
      DOUBLE PRECISION 
     .   X0(MVSIZ,NPE), Y0(MVSIZ,NPE), Z0(MVSIZ,NPE)
C-----------------------------------------------
      DO I=1,NEL
       X0(I,NPE)=ZERO
       Y0(I,NPE)=ZERO
       Z0(I,NPE)=ZERO
      ENDDO
      NPE1=NPE-1
      DO N=1,NPE1
       N2 = N + NPE1
       N3 = N2 + NPE1
       DO I=1,NEL
        X0(I,N)=SAV(I,N)
        Y0(I,N)=SAV(I,N2)
        Z0(I,N)=SAV(I,N3)
       ENDDO
      ENDDO 
      XD1(1:NEL) =  X0(1:NEL,1)
      XD2(1:NEL) =  X0(1:NEL,2)
      XD3(1:NEL) =  X0(1:NEL,3)
      XD4(1:NEL) =  X0(1:NEL,4)
      XD5(1:NEL) =  X0(1:NEL,5)
      XD6(1:NEL) =  X0(1:NEL,6)
      XD7(1:NEL) =  X0(1:NEL,7)
      XD8(1:NEL) =  X0(1:NEL,8)
      YD1(1:NEL) =  Y0(1:NEL,1)
      YD2(1:NEL) =  Y0(1:NEL,2)
      YD3(1:NEL) =  Y0(1:NEL,3)
      YD4(1:NEL) =  Y0(1:NEL,4)
      YD5(1:NEL) =  Y0(1:NEL,5)
      YD6(1:NEL) =  Y0(1:NEL,6)
      YD7(1:NEL) =  Y0(1:NEL,7)
      YD8(1:NEL) =  Y0(1:NEL,8)
      ZD1(1:NEL) =  Z0(1:NEL,1)
      ZD2(1:NEL) =  Z0(1:NEL,2)
      ZD3(1:NEL) =  Z0(1:NEL,3)
      ZD4(1:NEL) =  Z0(1:NEL,4)
      ZD5(1:NEL) =  Z0(1:NEL,5)
      ZD6(1:NEL) =  Z0(1:NEL,6)
      ZD7(1:NEL) =  Z0(1:NEL,7)
      ZD8(1:NEL) =  Z0(1:NEL,8)
C
      RETURN
      END
      
