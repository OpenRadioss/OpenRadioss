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
Chd|  S8ZLKEP3                      source/elements/solid/solide8z/s8zlkep3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        S8ZKED3                       source/elements/solid/solide8z/s8zked3.F
Chd|====================================================================
      SUBROUTINE S8ZLKEP3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   K11,     K12,     K13,     K14,
     5   K15,     K16,     K17,     K18,
     6   K22,     K23,     K24,     K25,
     7   K26,     K27,     K28,     K33,
     8   K34,     K35,     K36,     K37,
     9   K38,     K44,     K45,     K46,
     A   K47,     K48,     K55,     K56,
     B   K57,     K58,     K66,     K67,
     C   K68,     K77,     K78,     K88,
     D   HH,      VOL,     FAC,     ICP,
     E   NEL)
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER ICP
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)    ,K15(9,*)   ,
     .   K16(9,*)   ,K17(9,*)   ,K18(9,*)    ,K22(9,*)    ,K23(9,*)   ,
     .   K24(9,*)   ,K25(9,*)   ,K26(9,*)    ,K27(9,*)    ,K28(9,*)   ,
     .   K33(9,*)   ,K34(9,*)   ,K35(9,*)    ,K36(9,*)    ,K37(9,*)   ,
     .   K38(9,*)   ,K44(9,*)   ,K45(9,*)    ,K46(9,*)    ,K47(9,*)   ,
     .   K48(9,*)   ,K55(9,*)   ,K56(9,*)    ,K57(9,*)    ,K58(9,*)   ,
     .   K66(9,*)   ,K67(9,*)   ,K68(9,*)    ,K77(9,*)    ,K78(9,*)   ,
     .   K88(9,*)   ,HH(2,*)    ,VOL(*)      ,FAC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IS
C     REAL
      my_real
     .   TT,TV,DD(3,3,MVSIZ),
     .   PX5(MVSIZ), PX6(MVSIZ), PX7(MVSIZ), PX8(MVSIZ),  
     .   PY5(MVSIZ), PY6(MVSIZ), PY7(MVSIZ), PY8(MVSIZ),  
     .   PZ5(MVSIZ), PZ6(MVSIZ), PZ7(MVSIZ), PZ8(MVSIZ)  
C-----------------------------------------------
       IF (ICP==1) THEN
        DO I=1,NEL
         TV=(HH(1,I)+TWO_THIRD*HH(2,I))*VOL(I)
         TT=TV
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(3,3,I)=TT
         DD(1,2,I)=TV
         DD(1,3,I)=TV
         DD(2,1,I)=TV
         DD(2,3,I)=TV
         DD(3,1,I)=TV
         DD(3,2,I)=TV
        ENDDO
       ELSE
        DO I=1,NEL
         TV=(HH(1,I)*FAC(I)+TWO_THIRD*HH(2,I)*(ONE-FAC(I)))*VOL(I)
         TT=TV
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(3,3,I)=TT
         DD(1,2,I)=TV
         DD(1,3,I)=TV
         DD(2,1,I)=TV
         DD(2,3,I)=TV
         DD(3,1,I)=TV
         DD(3,2,I)=TV
        ENDDO
       END IF !(ICP==1) THEN
C       
       DO I=1,NEL
         PX5(I)=-PX3(I)
         PY5(I)=-PY3(I)
         PZ5(I)=-PZ3(I)
         PX6(I)=-PX4(I)
         PY6(I)=-PY4(I)
         PZ6(I)=-PZ4(I)
         PX7(I)=-PX1(I)
         PY7(I)=-PY1(I)
         PZ7(I)=-PZ1(I)
         PX8(I)=-PX2(I)
         PY8(I)=-PY2(I)
         PZ8(I)=-PZ2(I)
       ENDDO
C  -------symm(diag)------
      IS =1
C      
       CALL S8ZKED3(1,NEL,PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, DD, K11,IS)
       CALL S8ZKED3(1,NEL,PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, DD, K22,IS)
       CALL S8ZKED3(1,NEL,PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, DD, K33,IS)
       CALL S8ZKED3(1,NEL,PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, DD, K44,IS)
       CALL S8ZKED3(1,NEL,PX5, PY5 ,PZ5 ,PX5, PY5, PZ5, DD, K55,IS)
       CALL S8ZKED3(1,NEL,PX6, PY6 ,PZ6 ,PX6, PY6, PZ6, DD, K66,IS)
       CALL S8ZKED3(1,NEL,PX7, PY7 ,PZ7 ,PX7, PY7, PZ7, DD, K77,IS)
       CALL S8ZKED3(1,NEL,PX8, PY8 ,PZ8 ,PX8, PY8, PZ8, DD, K88,IS)
C  -------non symm------
      IS =0
C      
       CALL S8ZKED3(1,NEL,PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, DD, K12,IS)
       CALL S8ZKED3(1,NEL,PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, DD, K13,IS)
       CALL S8ZKED3(1,NEL,PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, DD, K14,IS)
       CALL S8ZKED3(1,NEL,PX1, PY1 ,PZ1 ,PX5, PY5, PZ5, DD, K15,IS)
       CALL S8ZKED3(1,NEL,PX1, PY1 ,PZ1 ,PX6, PY6, PZ6, DD, K16,IS)
       CALL S8ZKED3(1,NEL,PX1, PY1 ,PZ1 ,PX7, PY7, PZ7, DD, K17,IS)
       CALL S8ZKED3(1,NEL,PX1, PY1 ,PZ1 ,PX8, PY8, PZ8, DD, K18,IS)
       CALL S8ZKED3(1,NEL,PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, DD, K23,IS)
       CALL S8ZKED3(1,NEL,PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, DD, K24,IS)
       CALL S8ZKED3(1,NEL,PX2, PY2 ,PZ2 ,PX5, PY5, PZ5, DD, K25,IS)
       CALL S8ZKED3(1,NEL,PX2, PY2 ,PZ2 ,PX6, PY6, PZ6, DD, K26,IS)
       CALL S8ZKED3(1,NEL,PX2, PY2 ,PZ2 ,PX7, PY7, PZ7, DD, K27,IS)
       CALL S8ZKED3(1,NEL,PX2, PY2 ,PZ2 ,PX8, PY8, PZ8, DD, K28,IS)
       CALL S8ZKED3(1,NEL,PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, DD, K34,IS)
       CALL S8ZKED3(1,NEL,PX3, PY3 ,PZ3 ,PX5, PY5, PZ5, DD, K35,IS)
       CALL S8ZKED3(1,NEL,PX3, PY3 ,PZ3 ,PX6, PY6, PZ6, DD, K36,IS)
       CALL S8ZKED3(1,NEL,PX3, PY3 ,PZ3 ,PX7, PY7, PZ7, DD, K37,IS)
       CALL S8ZKED3(1,NEL,PX3, PY3 ,PZ3 ,PX8, PY8, PZ8, DD, K38,IS)
       CALL S8ZKED3(1,NEL,PX4, PY4 ,PZ4 ,PX5, PY5, PZ5, DD, K45,IS)
       CALL S8ZKED3(1,NEL,PX4, PY4 ,PZ4 ,PX6, PY6, PZ6, DD, K46,IS)
       CALL S8ZKED3(1,NEL,PX4, PY4 ,PZ4 ,PX7, PY7, PZ7, DD, K47,IS)
       CALL S8ZKED3(1,NEL,PX4, PY4 ,PZ4 ,PX8, PY8, PZ8, DD, K48,IS)
       CALL S8ZKED3(1,NEL,PX5, PY5 ,PZ5 ,PX6, PY6, PZ6, DD, K56,IS)
       CALL S8ZKED3(1,NEL,PX5, PY5 ,PZ5 ,PX7, PY7, PZ7, DD, K57,IS)
       CALL S8ZKED3(1,NEL,PX5, PY5 ,PZ5 ,PX8, PY8, PZ8, DD, K58,IS)
       CALL S8ZKED3(1,NEL,PX6, PY6 ,PZ6 ,PX7, PY7, PZ7, DD, K67,IS)
       CALL S8ZKED3(1,NEL,PX6, PY6 ,PZ6 ,PX8, PY8, PZ8, DD, K68,IS)
       CALL S8ZKED3(1,NEL,PX7, PY7 ,PZ7 ,PX8, PY8, PZ8, DD, K78,IS)
C
C   ---------------
      RETURN
      END
