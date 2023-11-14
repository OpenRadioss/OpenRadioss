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
Chd|  FDEFO3                        source/elements/solid/solide/fdefo3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FDEFO3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   VX1,     VX2,     VX3,     VX4,
     5   VX5,     VX6,     VX7,     VX8,
     6   VY1,     VY2,     VY3,     VY4,
     7   VY5,     VY6,     VY7,     VY8,
     8   VZ1,     VZ2,     VZ3,     VZ4,
     9   VZ5,     VZ6,     VZ7,     VZ8,
     A   DXX,     DXY,     DXZ,     DYX,
     B   DYY,     DYZ,     DZX,     DZY,
     C   DZZ,     D4,      D5,      D6,
     D   WXX,     WYY,     WZZ,     VXR,
     E   VYR,     VZR,     DD,      NEL,
     F   JHBE,    JCVT,    ISROT)
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
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: ISROT
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), 
     .   VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), 
     .   VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), 
     .   VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*), DD(*)
      my_real
     .   VXR(*),VYR(*),VZR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
      my_real
     .   VX17(MVSIZ), VY17(MVSIZ), VZ17(MVSIZ),
     .   VX28(MVSIZ), VY28(MVSIZ), VZ28(MVSIZ),
     .   VX35(MVSIZ), VY35(MVSIZ), VZ35(MVSIZ),
     .   VX46(MVSIZ), VY46(MVSIZ), VZ46(MVSIZ), 
     .    DT1D2, DT1D
      my_real 
     .   PXX2,PYY2,PZZ2,PXX2P,PYY2P,PZZ2P
      my_real
     .   AAA,BBB
C-----------------------------------------------   
       DO I=1,NEL
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
C                                                                     
       DO I=1,NEL
         DXX(I)=PX1(I)*VX17(I)+PX2(I)*VX28(I)+
     .    PX3(I)*VX35(I)+PX4(I)*VX46(I)
         DYY(I)=PY1(I)*VY17(I)+PY2(I)*VY28(I)+
     .    PY3(I)*VY35(I)+PY4(I)*VY46(I)
         DZZ(I)=PZ1(I)*VZ17(I)+PZ2(I)*VZ28(I)+
     .    PZ3(I)*VZ35(I)+PZ4(I)*VZ46(I)
         DXY(I)=PY1(I)*VX17(I)+PY2(I)*VX28(I)+
     .    PY3(I)*VX35(I)+PY4(I)*VX46(I)
         DXZ(I)=PZ1(I)*VX17(I)+PZ2(I)*VX28(I)+
     .    PZ3(I)*VX35(I)+PZ4(I)*VX46(I)
         DYX(I)=PX1(I)*VY17(I)+PX2(I)*VY28(I)+
     .    PX3(I)*VY35(I)+PX4(I)*VY46(I)
         DYZ(I)=PZ1(I)*VY17(I)+PZ2(I)*VY28(I)+
     .    PZ3(I)*VY35(I)+PZ4(I)*VY46(I)
         DZX(I)=PX1(I)*VZ17(I)+PX2(I)*VZ28(I)+
     .    PX3(I)*VZ35(I)+PX4(I)*VZ46(I)
         DZY(I)=PY1(I)*VZ17(I)+PY2(I)*VZ28(I)+
     .    PY3(I)*VZ35(I)+PY4(I)*VZ46(I)
       ENDDO
C
C
      DT1D2=HALF*DT1
      IF (ISCAU>0)DT1D2=DT1
C
      IF (JCVT > 0) THEN
       IF (IMPL_S==0.OR.IDYNA > 0) THEN
        DO I=1,NEL
          DXX(I)  = DXX(I)
     .           -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
          DYY(I)  = DYY(I)
     .           -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
          DZZ(I)  = DZZ(I)
     .           -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
          AAA = DT1D2*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
          DXY(I)  = DXY(I) - AAA
          DYX(I)  = DYX(I) - AAA
          D4(I)   = DXY(I)+DYX(I)
          AAA = DT1D2*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
          DYZ(I)  = DYZ(I) - AAA
          DZY(I)  = DZY(I) - AAA
          D5(I)   = DYZ(I)+DZY(I)
          AAA = DT1D2*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
          DXZ(I)  = DXZ(I) - AAA
          DZX(I)  = DZX(I) - AAA
          D6(I)   = DXZ(I)+DZX(I)
          WXX(I)=ZERO
          WYY(I)=ZERO
          WZZ(I)=ZERO
         ENDDO
C-------implicit static---------       
       ELSEIF (ILINE>0.AND.ISCAU==0) THEN
        DO I=1,NEL
         D4(I)   = DXY(I)+DYX(I)
         D5(I)   = DYZ(I)+DZY(I)
         D6(I)   = DXZ(I)+DZX(I)
         WXX(I)=ZERO
         WYY(I)=ZERO
         WZZ(I)=ZERO
        ENDDO
       ELSE
         DT1D=TWO*DT1D2
         DO I=1,NEL
          D4(I)   = DXY(I)+DYX(I)
     .             -DT1D*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
          D5(I)   = DYZ(I)+DZY(I)
     .             -DT1D*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
          D6(I)   = DXZ(I)+DZX(I)
     .             -DT1D*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
          DXX(I)  = DXX(I)
     .             -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
          DYY(I)  = DYY(I)
     .             -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
          DZZ(I)  = DZZ(I)
     .             -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
          WXX(I)=ZERO
          WYY(I)=ZERO
          WZZ(I)=ZERO
         ENDDO
       ENDIF ! IF (IMPL_S==0.OR.IDYNA>0)
C
      ELSEIF (JHBE >= 2) THEN
       DO I=1,NEL
        DXX(I)  = DXX(I)
     .           -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
        DYY(I)  = DYY(I)
     .           -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
        DZZ(I)  = DZZ(I)
     .           -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
        AAA = DT1D2*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
        DXY(I)  = DXY(I) - AAA
        DYX(I)  = DYX(I) - AAA
        D4(I)   = DXY(I)+DYX(I)
        AAA = DT1D2*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
        DYZ(I)  = DYZ(I) - AAA
        DZY(I)  = DZY(I) - AAA
        D5(I)   = DYZ(I)+DZY(I)
        AAA = DT1D2*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
        DXZ(I)  = DXZ(I) - AAA
        DZX(I)  = DZX(I) - AAA
        D6(I)   = DXZ(I)+DZX(I)
        PXX2 = PX1(I)*PX1(I)+PX2(I)*PX2(I)
     .        +PX3(I)*PX3(I)+PX4(I)*PX4(I)
        PYY2 = PY1(I)*PY1(I)+PY2(I)*PY2(I)
     .        +PY3(I)*PY3(I)+PY4(I)*PY4(I)
        PZZ2 = PZ1(I)*PZ1(I)+PZ2(I)*PZ2(I)
     .        +PZ3(I)*PZ3(I)+PZ4(I)*PZ4(I)
        WZZ(I)=DT1*(PYY2*DYX(I)-PXX2*DXY(I))/(PXX2+PYY2)
        WXX(I)=DT1*(PZZ2*DZY(I)-PYY2*DYZ(I))/(PYY2+PZZ2)
        WYY(I)=DT1*(PXX2*DXZ(I)-PZZ2*DZX(I))/(PZZ2+PXX2)
       ENDDO
C
      ELSE
       DO I=1,NEL
        D4(I)   = DXY(I)+DYX(I)
        D5(I)   = DYZ(I)+DZY(I)
        D6(I)   = DXZ(I)+DZX(I)
        DXX(I)  = DXX(I)
        DYY(I)  = DYY(I)
        DZZ(I)  = DZZ(I)
        WZZ(I)=DT1D2*(DYX(I)-DXY(I))
        WYY(I)=DT1D2*(DXZ(I)-DZX(I))
        WXX(I)=DT1D2*(DZY(I)-DYZ(I))
       ENDDO
      ENDIF  
C
       
      IF(ISROT > 0)THEN
       DO I=1,NEL
        DXY(I)=DXY(I) + VZR(I)
        DYX(I)=DYX(I) - VZR(I)
        DYZ(I)=DYZ(I) + VXR(I)
        DZY(I)=DZY(I) - VXR(I)
        DZX(I)=DZX(I) + VYR(I)
        DXZ(I)=DXZ(I) - VYR(I)
       ENDDO
       AAA = 0.
       BBB = DT1 - AAA*DT1
       DO I=1,NEL
        WXX(I)=AAA*WXX(I) + BBB*VXR(I)
        WYY(I)=AAA*WYY(I) + BBB*VYR(I)
        WZZ(I)=AAA*WZZ(I) + BBB*VZR(I)
       ENDDO
      ENDIF
C pour le calcul du pas du temps      
      DO I=1, NEL
        DD(I) = - DXX(I)- DYY(I) - DZZ(I) 
      ENDDO  
C 
      RETURN
      END SUBROUTINE FDEFO3
