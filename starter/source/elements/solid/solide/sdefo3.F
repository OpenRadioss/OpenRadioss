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
!||    sdefo3     ../starter/source/elements/solid/solide/sdefo3.F
!||--- called by ------------------------------------------------------
!||    srefsta3   ../starter/source/elements/solid/solide/srefsta3.F
!||====================================================================
      SUBROUTINE SDEFO3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .   WXX, WYY, WZZ)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
      my_real
     .   VX17(MVSIZ), VY17(MVSIZ), VZ17(MVSIZ),
     .   VX28(MVSIZ), VY28(MVSIZ), VZ28(MVSIZ),
     .   VX35(MVSIZ), VY35(MVSIZ), VZ35(MVSIZ),
     .   VX46(MVSIZ), VY46(MVSIZ), VZ46(MVSIZ), DT1D2
      my_real 
     .   PXX2,PYY2,PZZ2,PXX2P,PYY2P,PZZ2P
      my_real
     .   AAA,BBB
C-----------------------------------------------
      DO I=LFT,LLT
       VX17(I)=VX1(I)-VX7(I)
       VX28(I)=VX2(I)-VX8(I)
       VX35(I)=VX3(I)-VX5(I)
       VX46(I)=VX4(I)-VX6(I)
       VY17(I)=VY1(I)-VY7(I)
       VY28(I)=VY2(I)-VY8(I)
       VY35(I)=VY3(I)-VY5(I)
       VY46(I)=VY4(I)-VY6(I)
       VZ17(I)=VZ1(I)-VZ7(I)
       VZ28(I)=VZ2(I)-VZ8(I)
       VZ35(I)=VZ3(I)-VZ5(I)
       VZ46(I)=VZ4(I)-VZ6(I)
      ENDDO
C                                                                     12
      DO I=LFT,LLT
        DXX(I)=PX1(I)*VX17(I)+PX2(I)*VX28(I)+
     .         PX3(I)*VX35(I)+PX4(I)*VX46(I)
        DYY(I)=PY1(I)*VY17(I)+PY2(I)*VY28(I)+
     .         PY3(I)*VY35(I)+PY4(I)*VY46(I)
        DZZ(I)=PZ1(I)*VZ17(I)+PZ2(I)*VZ28(I)+
     .         PZ3(I)*VZ35(I)+PZ4(I)*VZ46(I)
        DXY(I)=PY1(I)*VX17(I)+PY2(I)*VX28(I)+
     .         PY3(I)*VX35(I)+PY4(I)*VX46(I)
        DXZ(I)=PZ1(I)*VX17(I)+PZ2(I)*VX28(I)+
     .         PZ3(I)*VX35(I)+PZ4(I)*VX46(I)
        DYX(I)=PX1(I)*VY17(I)+PX2(I)*VY28(I)+
     .         PX3(I)*VY35(I)+PX4(I)*VY46(I)
        DYZ(I)=PZ1(I)*VY17(I)+PZ2(I)*VY28(I)+
     .         PZ3(I)*VY35(I)+PZ4(I)*VY46(I)
        DZX(I)=PX1(I)*VZ17(I)+PX2(I)*VZ28(I)+
     .         PX3(I)*VZ35(I)+PX4(I)*VZ46(I)
        DZY(I)=PY1(I)*VZ17(I)+PY2(I)*VZ28(I)+
     .         PY3(I)*VZ35(I)+PY4(I)*VZ46(I)
      ENDDO
C
      IF (JCVT/=0) THEN
       DO I=LFT,LLT
        D4(I)   = DXY(I)+DYX(I)
     .           -(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
        D5(I)   = DYZ(I)+DZY(I)
     .           -(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
        D6(I)   = DXZ(I)+DZX(I)
     .           -(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
        DXX(I)  = DXX(I)
     .           -HALF*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
        DYY(I)  = DYY(I)
     .           -HALF*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
        DZZ(I)  = DZZ(I)
     .           -HALF*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
        WXX(I)  = ZERO
        WYY(I)  = ZERO
        WZZ(I)  = ZERO
       ENDDO
      ELSE
       DO I=LFT,LLT
        D4(I)   = DXY(I)+DYX(I)
        D5(I)   = DYZ(I)+DZY(I)
        D6(I)   = DXZ(I)+DZX(I)
        DXX(I)  = DXX(I)
        DYY(I)  = DYY(I)
        DZZ(I)  = DZZ(I)
        WXX(I)  = ZERO
        WYY(I)  = ZERO
        WZZ(I)  = ZERO
c       WZZ(I)  =DT1D2*(DYX(I)-DXY(I))
c       WYY(I)  =DT1D2*(DXZ(I)-DZX(I))
c       WXX(I)  =DT1D2*(DZY(I)-DYZ(I))
       ENDDO
      ENDIF
C
      RETURN
      END
