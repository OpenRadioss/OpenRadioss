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
Chd|  S11DEFO3                      source/elements/solid/solide/s11defo3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE S11DEFO3(
     1   PM,         V,          VEUL,       X,
     2   IXS,        ALE_CONNECT,DXX,        DXY,
     3   DXZ,        DYX,        DYY,        DYZ,
     4   DZX,        DZY,        DZZ,        D4,
     5   D5,         D6,         WXX,        WYY,
     6   WZZ,        BUFMAT,     NEL,        NFT)
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER IXS(NIXS,*)
      my_real
     .   V(3,*), VEUL(LVEUL,*),X(3,*),PM(NPROPM,*),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*),BUFMAT(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II,IVOI,ML,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,J,IADBUF,IFLG,IAD2
      my_real
     . X1,Y1,Z1,VX1,VY1,VZ1,   
     . X2,Y2,Z2,VX2,VY2,VZ2,   
     . X3,Y3,Z3,VX3,VY3,VZ3,   
     . X4,Y4,Z4,VX4,VY4,VZ4,   
     . X5,Y5,Z5,VX5,VY5,VZ5,   
     . X6,Y6,Z6,VX6,VY6,VZ6,   
     . X7,Y7,Z7,VX7,VY7,VZ7,   
     . X8,Y8,Z8,VX8,VY8,VZ8,   
     . X17,X28,X35,X46,Y17,Y28,Y35,Y46,Z17,Z28,Z35,Z46,
     . VX17,VX28,VX35,VX46,VY17,VY28,VY35,VY46,VZ17,VZ28,VZ35,VZ46,
     . AJ1,AJ2,AJ3,AJ4,AJ5,AJ6,AJ7,AJ8,AJ9,
     . AJI1,AJI2,AJI3,AJI4,AJI5,AJI6,AJI7,AJI8,AJI9,
     . JAC_59_68,JAC_67_49,JAC_48_57,AJ12,AJ45,AJ78,
     . PX1, PX2, PX3, PX4,
     . PY1, PY2, PY3, PY4,
     . PZ1, PZ2, PZ3, PZ4, 
     . A17,A28,B17,B28,C17,C28,DT1D2,
     . DETT
C-----------------------------------------------
      DT1D2=HALF*DT1
      DO I=1,NEL
       II=I+NFT
c
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
       DO  J=1,6
        IVOI=ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
        ML   = 11
        IFLG = 6
        IF(IVOI/=0)ML=NINT(PM(19,IXS(1,IVOI)))
        IF(ML/=11)EXIT
        IF(ML==51)THEN
          IADBUF = NINT(PM(27,IXS(1,IVOI)))
          IFLG = NINT(BUFMAT(31+IADBUF-1)) 
          IF(IFLG<=1)EXIT
        ENDIF 
       ENDDO
       
       IF(ML==11 .OR. (ML==51.AND.(IFLG==6.OR.IFLG==3)))THEN  
         DXX(I)=ZERO
         DYY(I)=ZERO
         DZZ(I)=ZERO
         DXY(I)=ZERO
         DXZ(I)=ZERO
         DYX(I)=ZERO
         DYZ(I)=ZERO
         DZX(I)=ZERO
         DZY(I)=ZERO
         D4(I) =ZERO
         D5(I) =ZERO
         D6(I) =ZERO
         WZZ(I)=ZERO
         WYY(I)=ZERO
         WXX(I)=ZERO
       ELSE       
         NC1=IXS(2,IVOI)
         NC2=IXS(3,IVOI)
         NC3=IXS(4,IVOI)
         NC4=IXS(5,IVOI)
         NC5=IXS(6,IVOI)
         NC6=IXS(7,IVOI)
         NC7=IXS(8,IVOI)
         NC8=IXS(9,IVOI)
         X1 =X(1,NC1)
         Y1 =X(2,NC1)
         Z1 =X(3,NC1)
         VX1=V(1,NC1)
         VY1=V(2,NC1)
         VZ1=V(3,NC1)
         X2 =X(1,NC2)
         Y2 =X(2,NC2)
         Z2 =X(3,NC2)
         VX2=V(1,NC2)
         VY2=V(2,NC2)
         VZ2=V(3,NC2)
         X3 =X(1,NC3)
         Y3 =X(2,NC3)
         Z3 =X(3,NC3)
         VX3=V(1,NC3)
         VY3=V(2,NC3)
         VZ3=V(3,NC3)
         X4 =X(1,NC4)
         Y4 =X(2,NC4)
         Z4 =X(3,NC4)
         VX4=V(1,NC4)
         VY4=V(2,NC4)
         VZ4=V(3,NC4)
         X5 =X(1,NC5)
         Y5 =X(2,NC5)
         Z5 =X(3,NC5)
         VX5=V(1,NC5)
         VY5=V(2,NC5)
         VZ5=V(3,NC5)
         X6 =X(1,NC6)
         Y6 =X(2,NC6)
         Z6 =X(3,NC6)
         VX6=V(1,NC6)
         VY6=V(2,NC6)
         VZ6=V(3,NC6)
         X7 =X(1,NC7)
         Y7 =X(2,NC7)
         Z7 =X(3,NC7)
         VX7=V(1,NC7)
         VY7=V(2,NC7)
         VZ7=V(3,NC7)
         X8 =X(1,NC8)
         Y8 =X(2,NC8)
         Z8 =X(3,NC8)
         VX8 =V(1,NC8)
         VY8 =V(2,NC8)
         VZ8 =V(3,NC8)
         X17=X7-X1
         X28=X8-X2
         X35=X5-X3
         X46=X6-X4
         Y17=Y7-Y1
         Y28=Y8-Y2
         Y35=Y5-Y3
         Y46=Y6-Y4
         Z17=Z7-Z1
         Z28=Z8-Z2
         Z35=Z5-Z3
         Z46=Z6-Z4
         AJ1=X17+X28-X35-X46
         AJ2=Y17+Y28-Y35-Y46
         AJ3=Z17+Z28-Z35-Z46
         A17=X17+X46
         A28=X28+X35
         B17=Y17+Y46
         B28=Y28+Y35
         C17=Z17+Z46
         C28=Z28+Z35
         AJ4=A17+A28
         AJ5=B17+B28
         AJ6=C17+C28
         AJ7=A17-A28
         AJ8=B17-B28
         AJ9=C17-C28
         JAC_59_68=AJ5*AJ9-AJ6*AJ8
         JAC_67_49=AJ6*AJ7-AJ4*AJ9
         JAC_48_57=AJ4*AJ8-AJ5*AJ7
         DETT=ONE/(AJ1*JAC_59_68+AJ2*JAC_67_49+AJ3*JAC_48_57)
         AJI1=DETT*JAC_59_68
         AJI4=DETT*JAC_67_49
         AJI7=DETT*JAC_48_57
         AJI2=DETT*(-AJ2*AJ9+AJ3*AJ8)
         AJI5=DETT*( AJ1*AJ9-AJ3*AJ7)
         AJI8=DETT*(-AJ1*AJ8+AJ2*AJ7)
         AJI3=DETT*( AJ2*AJ6-AJ3*AJ5)
         AJI6=DETT*(-AJ1*AJ6+AJ3*AJ4)
         AJI9=DETT*( AJ1*AJ5-AJ2*AJ4)
         AJ12=AJI1-AJI2
         AJ45=AJI4-AJI5
         AJ78=AJI7-AJI8
         PX3= AJ12+AJI3
         PY3= AJ45+AJI6
         PZ3= AJ78+AJI9
         PX4= AJ12-AJI3
         PY4= AJ45-AJI6
         PZ4= AJ78-AJI9
         AJ12=AJI1+AJI2
         AJ45=AJI4+AJI5
         AJ78=AJI7+AJI8
         PX1=-AJ12-AJI3
         PY1=-AJ45-AJI6
         PZ1=-AJ78-AJI9
         PX2=-AJ12+AJI3
         PY2=-AJ45+AJI6
         PZ2=-AJ78+AJI9
         VX17=VX1-VX7
         VX28=VX2-VX8
         VX35=VX3-VX5
         VX46=VX4-VX6
         VY17=VY1-VY7
         VY28=VY2-VY8
         VY35=VY3-VY5
         VY46=VY4-VY6
         VZ17=VZ1-VZ7
         VZ28=VZ2-VZ8
         VZ35=VZ3-VZ5
         VZ46=VZ4-VZ6
         DXX(I)=PX1*VX17+PX2*VX28+PX3*VX35+PX4*VX46
         DYY(I)=PY1*VY17+PY2*VY28+PY3*VY35+PY4*VY46
         DZZ(I)=PZ1*VZ17+PZ2*VZ28+PZ3*VZ35+PZ4*VZ46
         DXY(I)=PY1*VX17+PY2*VX28+PY3*VX35+PY4*VX46
         DXZ(I)=PZ1*VX17+PZ2*VX28+PZ3*VX35+PZ4*VX46
         DYX(I)=PX1*VY17+PX2*VY28+PX3*VY35+PX4*VY46
         DYZ(I)=PZ1*VY17+PZ2*VY28+PZ3*VY35+PZ4*VY46
         DZX(I)=PX1*VZ17+PX2*VZ28+PX3*VZ35+PX4*VZ46
         DZY(I)=PY1*VZ17+PY2*VZ28+PY3*VZ35+PY4*VZ46
         D4(I) =DXY(I)+DYX(I)
         D5(I) =DYZ(I)+DZY(I)
         D6(I) =DXZ(I)+DZX(I)
         WZZ(I)=DT1D2*(DYX(I)-DXY(I))
         WYY(I)=DT1D2*(DXZ(I)-DZX(I))
         WXX(I)=DT1D2*(DZY(I)-DYZ(I))
       ENDIF
      ENDDO
C
      RETURN
      END
