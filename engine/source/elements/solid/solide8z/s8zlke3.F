Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        S8ZKEB3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKEBG3                      source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKED3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKEG3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKEG33                      source/elements/solid/solide8z/s8zlke3.F
Chd|====================================================================
      SUBROUTINE S8ZLKE3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX5, PX6, PX7, PX8,
     .   PY5, PY6, PY7, PY8,
     .   PZ5, PZ6, PZ7, PZ8,
     .   PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .   PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .   PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .   PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .   PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .   PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .   BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .   BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .   BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .   BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .   BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .   BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,
     .   DD ,GG ,ICP,OFF,IKSUP ,DG  ,G33 )
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
      INTEGER ICP,IKSUP
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)    ,K15(9,*)   ,
     .   K16(9,*)   ,K17(9,*)   ,K18(9,*)    ,K22(9,*)    ,K23(9,*)   ,
     .   K24(9,*)   ,K25(9,*)   ,K26(9,*)    ,K27(9,*)    ,K28(9,*)   ,
     .   K33(9,*)   ,K34(9,*)   ,K35(9,*)    ,K36(9,*)    ,K37(9,*)   ,
     .   K38(9,*)   ,K44(9,*)   ,K45(9,*)    ,K46(9,*)    ,K47(9,*)   ,
     .   K48(9,*)   ,K55(9,*)   ,K56(9,*)    ,K57(9,*)    ,K58(9,*)   ,
     .   K66(9,*)   ,K67(9,*)   ,K68(9,*)    ,K77(9,*)    ,K78(9,*)   ,
     .   K88(9,*)   ,DD(3,3,*),OFF(*),GG(*),DG(9,*)  ,G33(9,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IS
C     REAL
C-----------------------------------------------
C  -------symm(diag)------
      IS =1
      IF (ICP==1) THEN
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, DD, K11,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, DD, K22,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, DD, K33,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, DD, K44,IS)
       CALL S8ZKED3(LFT,LLT,PX5, PY5 ,PZ5 ,PX5, PY5, PZ5, DD, K55,IS)
       CALL S8ZKED3(LFT,LLT,PX6, PY6 ,PZ6 ,PX6, PY6, PZ6, DD, K66,IS)
       CALL S8ZKED3(LFT,LLT,PX7, PY7 ,PZ7 ,PX7, PY7, PZ7, DD, K77,IS)
       CALL S8ZKED3(LFT,LLT,PX8, PY8 ,PZ8 ,PX8, PY8, PZ8, DD, K88,IS)
      ELSE
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX1, PY1, PZ1,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,DD,K11,IS)
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX2, PY2, PZ2,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,DD,K22,IS)
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX3, PY3, PZ3,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,DD,K33,IS)
       CALL S8ZKEB3(LFT,LLT,PX4, PY4 ,PZ4 ,PX4, PY4, PZ4,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,DD,K44,IS)
       CALL S8ZKEB3(LFT,LLT,PX5, PY5 ,PZ5 ,PX5, PY5, PZ5,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K55,IS)
       CALL S8ZKEB3(LFT,LLT,PX6, PY6 ,PZ6 ,PX6, PY6, PZ6,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K66,IS)
       CALL S8ZKEB3(LFT,LLT,PX7, PY7 ,PZ7 ,PX7, PY7, PZ7,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,DD,K77,IS)
       CALL S8ZKEB3(LFT,LLT,PX8, PY8 ,PZ8 ,PX8, PY8, PZ8,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,DD,K88,IS)
      ENDIF
      IF (IKSUP==0) THEN
       CALL S8ZKEG3(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,GG,K11,IS)
       CALL S8ZKEG3(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,GG,K22,IS)
       CALL S8ZKEG3(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,GG,K33,IS)
       CALL S8ZKEG3(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,GG,K44,IS)
       CALL S8ZKEG3(LFT,LLT,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K55,IS)
       CALL S8ZKEG3(LFT,LLT,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K66,IS)
       CALL S8ZKEG3(LFT,LLT,PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,GG,K77,IS)
       CALL S8ZKEG3(LFT,LLT,PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,GG,K88,IS)
      ENDIF
C  -------non symm------
      IS =0
      IF (ICP==1) THEN
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, DD, K12,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, DD, K13,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, DD, K14,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX5, PY5, PZ5, DD, K15,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX6, PY6, PZ6, DD, K16,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX7, PY7, PZ7, DD, K17,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX8, PY8, PZ8, DD, K18,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, DD, K23,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, DD, K24,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX5, PY5, PZ5, DD, K25,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX6, PY6, PZ6, DD, K26,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX7, PY7, PZ7, DD, K27,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX8, PY8, PZ8, DD, K28,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, DD, K34,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX5, PY5, PZ5, DD, K35,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX6, PY6, PZ6, DD, K36,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX7, PY7, PZ7, DD, K37,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX8, PY8, PZ8, DD, K38,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX5, PY5, PZ5, DD, K45,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX6, PY6, PZ6, DD, K46,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX7, PY7, PZ7, DD, K47,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX8, PY8, PZ8, DD, K48,IS)
       CALL S8ZKED3(LFT,LLT,PX5, PY5 ,PZ5 ,PX6, PY6, PZ6, DD, K56,IS)
       CALL S8ZKED3(LFT,LLT,PX5, PY5 ,PZ5 ,PX7, PY7, PZ7, DD, K57,IS)
       CALL S8ZKED3(LFT,LLT,PX5, PY5 ,PZ5 ,PX8, PY8, PZ8, DD, K58,IS)
       CALL S8ZKED3(LFT,LLT,PX6, PY6 ,PZ6 ,PX7, PY7, PZ7, DD, K67,IS)
       CALL S8ZKED3(LFT,LLT,PX6, PY6 ,PZ6 ,PX8, PY8, PZ8, DD, K68,IS)
       CALL S8ZKED3(LFT,LLT,PX7, PY7 ,PZ7 ,PX8, PY8, PZ8, DD, K78,IS)
      ELSE
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX2, PY2, PZ2,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,DD,K12,IS)
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX3, PY3, PZ3,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,DD,K13,IS)
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX4, PY4, PZ4,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,DD,K14,IS)
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX5, PY5, PZ5,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K15,IS)
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX6, PY6, PZ6,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K16,IS)
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX7, PY7, PZ7,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,DD,K17,IS)
       CALL S8ZKEB3(LFT,LLT,PX1, PY1 ,PZ1 ,PX8, PY8, PZ8,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,DD,K18,IS)
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX3, PY3, PZ3,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,DD,K23,IS)
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX4, PY4, PZ4,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,DD,K24,IS)
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX5, PY5, PZ5,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K25,IS)
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX6, PY6, PZ6,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K26,IS)
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX7, PY7, PZ7,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,DD,K27,IS)
       CALL S8ZKEB3(LFT,LLT,PX2, PY2 ,PZ2 ,PX8, PY8, PZ8,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,DD,K28,IS)
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX4, PY4, PZ4,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,DD,K34,IS)
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX5, PY5, PZ5,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K35,IS)
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX6, PY6, PZ6,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K36,IS)
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX7, PY7, PZ7,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,DD,K37,IS)
       CALL S8ZKEB3(LFT,LLT,PX3, PY3 ,PZ3 ,PX8, PY8, PZ8,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,DD,K38,IS)
       CALL S8ZKEB3(LFT,LLT,PX4, PY4 ,PZ4 ,PX5, PY5, PZ5,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,DD,K45,IS)
       CALL S8ZKEB3(LFT,LLT,PX4, PY4 ,PZ4 ,PX6, PY6, PZ6,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K46,IS)
       CALL S8ZKEB3(LFT,LLT,PX4, PY4 ,PZ4 ,PX7, PY7, PZ7,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,DD,K47,IS)
       CALL S8ZKEB3(LFT,LLT,PX4, PY4 ,PZ4 ,PX8, PY8, PZ8,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,DD,K48,IS)
       CALL S8ZKEB3(LFT,LLT,PX5, PY5 ,PZ5 ,PX6, PY6, PZ6,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,DD,K56,IS)
       CALL S8ZKEB3(LFT,LLT,PX5, PY5 ,PZ5 ,PX7, PY7, PZ7,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,DD,K57,IS)
       CALL S8ZKEB3(LFT,LLT,PX5, PY5 ,PZ5 ,PX8, PY8, PZ8,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,DD,K58,IS)
       CALL S8ZKEB3(LFT,LLT,PX6, PY6 ,PZ6 ,PX7, PY7, PZ7,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,DD,K67,IS)
       CALL S8ZKEB3(LFT,LLT,PX6, PY6 ,PZ6 ,PX8, PY8, PZ8,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,DD,K68,IS)
       CALL S8ZKEB3(LFT,LLT,PX7, PY7 ,PZ7 ,PX8, PY8, PZ8,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,DD,K78,IS)
      ENDIF
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IKSUP==0) THEN
       CALL S8ZKEG3(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,GG,K12,IS)
       CALL S8ZKEG3(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,GG,K13,IS)
       CALL S8ZKEG3(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,GG,K14,IS)
       CALL S8ZKEG3(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K15,IS)
       CALL S8ZKEG3(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K16,IS)
       CALL S8ZKEG3(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,GG,K17,IS)
       CALL S8ZKEG3(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,GG,K18,IS)
       CALL S8ZKEG3(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,GG,K23,IS)
       CALL S8ZKEG3(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,GG,K24,IS)
       CALL S8ZKEG3(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K25,IS)
       CALL S8ZKEG3(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K26,IS)
       CALL S8ZKEG3(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,GG,K27,IS)
       CALL S8ZKEG3(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,GG,K28,IS)
       CALL S8ZKEG3(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,GG,K34,IS)
       CALL S8ZKEG3(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K35,IS)
       CALL S8ZKEG3(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K36,IS)
       CALL S8ZKEG3(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,GG,K37,IS)
       CALL S8ZKEG3(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,GG,K38,IS)
       CALL S8ZKEG3(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,GG,K45,IS)
       CALL S8ZKEG3(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K46,IS)
       CALL S8ZKEG3(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,GG,K47,IS)
       CALL S8ZKEG3(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,GG,K48,IS)
       CALL S8ZKEG3(LFT,LLT,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,GG,K56,IS)
       CALL S8ZKEG3(LFT,LLT,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,GG,K57,IS)
       CALL S8ZKEG3(LFT,LLT,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,GG,K58,IS)
       CALL S8ZKEG3(LFT,LLT,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,GG,K67,IS)
       CALL S8ZKEG3(LFT,LLT,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,GG,K68,IS)
       CALL S8ZKEG3(LFT,LLT,PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,GG,K78,IS)
      ELSE
       CALL S8ZKEG33(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,G33,K12,IS)
       CALL S8ZKEG33(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,G33,K13,IS)
       CALL S8ZKEG33(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,G33,K14,IS)
       CALL S8ZKEG33(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,G33,K15,IS)
       CALL S8ZKEG33(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,G33,K16,IS)
       CALL S8ZKEG33(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,G33,K17,IS)
       CALL S8ZKEG33(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,G33,K18,IS)
       CALL S8ZKEG33(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,G33,K23,IS)
       CALL S8ZKEG33(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,G33,K24,IS)
       CALL S8ZKEG33(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,G33,K25,IS)
       CALL S8ZKEG33(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,G33,K26,IS)
       CALL S8ZKEG33(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,G33,K27,IS)
       CALL S8ZKEG33(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,G33,K28,IS)
       CALL S8ZKEG33(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,G33,K34,IS)
       CALL S8ZKEG33(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,G33,K35,IS)
       CALL S8ZKEG33(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,G33,K36,IS)
       CALL S8ZKEG33(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,G33,K37,IS)
       CALL S8ZKEG33(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,G33,K38,IS)
       CALL S8ZKEG33(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,G33,K45,IS)
       CALL S8ZKEG33(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,G33,K46,IS)
       CALL S8ZKEG33(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,G33,K47,IS)
       CALL S8ZKEG33(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,G33,K48,IS)
       CALL S8ZKEG33(LFT,LLT,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,G33,K56,IS)
       CALL S8ZKEG33(LFT,LLT,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,G33,K57,IS)
       CALL S8ZKEG33(LFT,LLT,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,G33,K58,IS)
       CALL S8ZKEG33(LFT,LLT,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,G33,K67,IS)
       CALL S8ZKEG33(LFT,LLT,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,G33,K68,IS)
       CALL S8ZKEG33(LFT,LLT,PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,G33,K78,IS)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       CALL S8ZKEBG3(LFT,LLT,PX1, PY1 ,PZ1 ,PX2, PY2, PZ2,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                   PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,DG,K12,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX1, PY1 ,PZ1 ,PX3, PY3, PZ3,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                   PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,DG,K13,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX1, PY1 ,PZ1 ,PX4, PY4, PZ4,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                   PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,DG,K14,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX1, PY1 ,PZ1 ,PX5, PY5, PZ5,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                   PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,DG,K15,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX1, PY1 ,PZ1 ,PX6, PY6, PZ6,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                   PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,DG,K16,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX1, PY1 ,PZ1 ,PX7, PY7, PZ7,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                   PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,DG,K17,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX1, PY1 ,PZ1 ,PX8, PY8, PZ8,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                   PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,DG,K18,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX2, PY2 ,PZ2 ,PX3, PY3, PZ3,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                   PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,DG,K23,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX2, PY2 ,PZ2 ,PX4, PY4, PZ4,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                   PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,DG,K24,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX2, PY2 ,PZ2 ,PX5, PY5, PZ5,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                   PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,DG,K25,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX2, PY2 ,PZ2 ,PX6, PY6, PZ6,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                   PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,DG,K26,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX2, PY2 ,PZ2 ,PX7, PY7, PZ7,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                   PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,DG,K27,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX2, PY2 ,PZ2 ,PX8, PY8, PZ8,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                   PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,DG,K28,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX3, PY3 ,PZ3 ,PX4, PY4, PZ4,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                   PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,DG,K34,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX3, PY3 ,PZ3 ,PX5, PY5, PZ5,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                   PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,DG,K35,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX3, PY3 ,PZ3 ,PX6, PY6, PZ6,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                   PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,DG,K36,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX3, PY3 ,PZ3 ,PX7, PY7, PZ7,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                   PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,DG,K37,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX3, PY3 ,PZ3 ,PX8, PY8, PZ8,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                   PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,DG,K38,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX4, PY4 ,PZ4 ,PX5, PY5, PZ5,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                   PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,DG,K45,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX4, PY4 ,PZ4 ,PX6, PY6, PZ6,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                   PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,DG,K46,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX4, PY4 ,PZ4 ,PX7, PY7, PZ7,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                   PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,DG,K47,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX4, PY4 ,PZ4 ,PX8, PY8, PZ8,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                   PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,DG,K48,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX5, PY5 ,PZ5 ,PX6, PY6, PZ6,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                   PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,DG,K56,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX5, PY5 ,PZ5 ,PX7, PY7, PZ7,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                   PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,DG,K57,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX5, PY5 ,PZ5 ,PX8, PY8, PZ8,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                   PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,DG,K58,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX6, PY6 ,PZ6 ,PX7, PY7, PZ7,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .                   PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,DG,K67,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX6, PY6 ,PZ6 ,PX8, PY8, PZ8,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .                   PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,DG,K68,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX7, PY7 ,PZ7 ,PX8, PY8, PZ8,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,
     .                   PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,DG,K78,IS,ICP)
C  -------symm(diag)------
      IS =1
c       IS =0!..DG is non-symmetric
       CALL S8ZKEBG3(LFT,LLT,PX1, PY1 ,PZ1 ,PX1, PY1, PZ1,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      BXY1, BYX1 ,BXZ1,BZX1 ,BYZ1,BZY1,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                   PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,DG,K11,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX2, PY2 ,PZ2 ,PX2, PY2, PZ2,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      BXY2, BYX2 ,BXZ2,BZX2 ,BYZ2,BZY2,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                   PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,DG,K22,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX3, PY3 ,PZ3 ,PX3, PY3, PZ3,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      BXY3, BYX3 ,BXZ3,BZX3 ,BYZ3,BZY3,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                   PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,DG,K33,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX4, PY4 ,PZ4 ,PX4, PY4, PZ4,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      BXY4, BYX4 ,BXZ4,BZX4 ,BYZ4,BZY4,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                   PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,DG,K44,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX5, PY5 ,PZ5 ,PX5, PY5, PZ5,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      BXY5, BYX5 ,BXZ5,BZX5 ,BYZ5,BZY5,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                   PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,DG,K55,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX6, PY6 ,PZ6 ,PX6, PY6, PZ6,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      BXY6, BYX6 ,BXZ6,BZX6 ,BYZ6,BZY6,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .                   PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,DG,K66,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX7, PY7 ,PZ7 ,PX7, PY7, PZ7,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      BXY7, BYX7 ,BXZ7,BZX7 ,BYZ7,BZY7,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,
     .                   PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,DG,K77,IS,ICP)
       CALL S8ZKEBG3(LFT,LLT,PX8, PY8 ,PZ8 ,PX8, PY8, PZ8,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      BXY8, BYX8 ,BXZ8,BZX8 ,BYZ8,BZY8,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,
     .                   PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,DG,K88,IS,ICP)
       CALL S8ZKEG33(LFT,LLT,PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,
     .                      PXY1, PYX1 ,PXZ1,PZX1 ,PYZ1,PZY1,G33,K11,IS)
       CALL S8ZKEG33(LFT,LLT,PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,
     .                      PXY2, PYX2 ,PXZ2,PZX2 ,PYZ2,PZY2,G33,K22,IS)
       CALL S8ZKEG33(LFT,LLT,PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,
     .                      PXY3, PYX3 ,PXZ3,PZX3 ,PYZ3,PZY3,G33,K33,IS)
       CALL S8ZKEG33(LFT,LLT,PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,
     .                      PXY4, PYX4 ,PXZ4,PZX4 ,PYZ4,PZY4,G33,K44,IS)
       CALL S8ZKEG33(LFT,LLT,PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,
     .                      PXY5, PYX5 ,PXZ5,PZX5 ,PYZ5,PZY5,G33,K55,IS)
       CALL S8ZKEG33(LFT,LLT,PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,
     .                      PXY6, PYX6 ,PXZ6,PZX6 ,PYZ6,PZY6,G33,K66,IS)
       CALL S8ZKEG33(LFT,LLT,PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,
     .                      PXY7, PYX7 ,PXZ7,PZX7 ,PYZ7,PZY7,G33,K77,IS)
       CALL S8ZKEG33(LFT,LLT,PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,
     .                      PXY8, PYX8 ,PXZ8,PZX8 ,PYZ8,PZY8,G33,K88,IS)
      ENDIF
C   ---------------
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKED3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- called by -----------
Chd|        S10CUMG3                      source/elements/solid/solide10/s10cumg3.F
Chd|        S20CUMG3                      source/elements/solid/solide20/s20cumg3.F
Chd|        S4CUMG3                       source/elements/solid/solide4/s4cumg3.F
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZLKEP3                      source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKED3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, DD, KIJ,IS)
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
      INTEGER LFT,LLT,IS
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),
     .   DD(3,3,*),KIJ(3,3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
      IF (IS==1) THEN
       DO I=LFT,LLT
        KIJ(1,1,I)=KIJ(1,1,I)+PXI(I)*DD(1,1,I)*PXJ(I)
        KIJ(2,2,I)=KIJ(2,2,I)+PYI(I)*DD(2,2,I)*PYJ(I)
        KIJ(3,3,I)=KIJ(3,3,I)+PZI(I)*DD(3,3,I)*PZJ(I)
        KIJ(1,2,I)=KIJ(1,2,I)+PXI(I)*DD(1,2,I)*PYJ(I)
        KIJ(1,3,I)=KIJ(1,3,I)+PXI(I)*DD(1,3,I)*PZJ(I)
        KIJ(2,3,I)=KIJ(2,3,I)+PYI(I)*DD(2,3,I)*PZJ(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
        KIJ(1,1,I)=KIJ(1,1,I)+PXI(I)*DD(1,1,I)*PXJ(I)
        KIJ(2,2,I)=KIJ(2,2,I)+PYI(I)*DD(2,2,I)*PYJ(I)
        KIJ(3,3,I)=KIJ(3,3,I)+PZI(I)*DD(3,3,I)*PZJ(I)
        KIJ(1,2,I)=KIJ(1,2,I)+PXI(I)*DD(1,2,I)*PYJ(I)
        KIJ(2,1,I)=KIJ(2,1,I)+PXJ(I)*DD(2,1,I)*PYI(I)
        KIJ(1,3,I)=KIJ(1,3,I)+PXI(I)*DD(1,3,I)*PZJ(I)
        KIJ(3,1,I)=KIJ(3,1,I)+PXJ(I)*DD(3,1,I)*PZI(I)
        KIJ(2,3,I)=KIJ(2,3,I)+PYI(I)*DD(2,3,I)*PZJ(I)
        KIJ(3,2,I)=KIJ(3,2,I)+PYJ(I)*DD(3,2,I)*PZI(I)
       ENDDO
      ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKEG3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- called by -----------
Chd|        S10CUMG3                      source/elements/solid/solide10/s10cumg3.F
Chd|        S20CUMG3                      source/elements/solid/solide20/s20cumg3.F
Chd|        S4CUMG3                       source/elements/solid/solide4/s4cumg3.F
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKEG3(LFT,LLT,
     .   PXYI, PYXI ,PXZI,PZXI ,PYZI,PZYI,
     .   PXYJ, PYXJ ,PXZJ,PZXJ ,PYZJ,PZYJ, G, KIJ,IS)
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
      INTEGER LFT,LLT,IS
C     REAL
      my_real
     .   PXYI(*), PYXI(*) ,PXZI(*),PZXI(*) ,PYZI(*),PZYI(*),
     .   PXYJ(*), PYXJ(*) ,PXZJ(*),PZXJ(*) ,PYZJ(*),PZYJ(*),
     .   G(*), KIJ(3,3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IS==1) THEN
       DO I=LFT,LLT
        KIJ(1,1,I)=KIJ(1,1,I)+G(I)*(PXYI(I)*PXYJ(I)+PXZI(I)*PXZJ(I))
        KIJ(2,2,I)=KIJ(2,2,I)+G(I)*(PYXI(I)*PYXJ(I)+PYZI(I)*PYZJ(I))
        KIJ(3,3,I)=KIJ(3,3,I)+G(I)*(PZXI(I)*PZXJ(I)+PZYI(I)*PZYJ(I))
        KIJ(1,2,I)=KIJ(1,2,I)+G(I)*PXYI(I)*PYXJ(I)
        KIJ(1,3,I)=KIJ(1,3,I)+G(I)*PXZI(I)*PZXJ(I)
        KIJ(2,3,I)=KIJ(2,3,I)+G(I)*PYZI(I)*PZYJ(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
        KIJ(1,1,I)=KIJ(1,1,I)+G(I)*(PXYI(I)*PXYJ(I)+PXZI(I)*PXZJ(I))
        KIJ(2,2,I)=KIJ(2,2,I)+G(I)*(PYXI(I)*PYXJ(I)+PYZI(I)*PYZJ(I))
        KIJ(3,3,I)=KIJ(3,3,I)+G(I)*(PZXI(I)*PZXJ(I)+PZYI(I)*PZYJ(I))
        KIJ(1,2,I)=KIJ(1,2,I)+G(I)*PXYI(I)*PYXJ(I)
        KIJ(2,1,I)=KIJ(2,1,I)+G(I)*PXYJ(I)*PYXI(I)
        KIJ(1,3,I)=KIJ(1,3,I)+G(I)*PXZI(I)*PZXJ(I)
        KIJ(3,1,I)=KIJ(3,1,I)+G(I)*PXZJ(I)*PZXI(I)
        KIJ(2,3,I)=KIJ(2,3,I)+G(I)*PYZI(I)*PZYJ(I)
        KIJ(3,2,I)=KIJ(3,2,I)+G(I)*PYZJ(I)*PZYI(I)
       ENDDO
      ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKEB3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- called by -----------
Chd|        S6CLKE3                       source/elements/thickshell/solide6c/s6clke3.F
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKEB3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ,
     .   PXYI, PYXI ,PXZI,PZXI ,PYZI,PZYI,
     .   PXYJ, PYXJ ,PXZJ,PZXJ ,PYZJ,PZYJ, DD, KIJ,IS)
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
      INTEGER LFT,LLT,IS
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),
     .   PXYI(*), PYXI(*) ,PXZI(*),PZXI(*) ,PYZI(*),PZYI(*),
     .   PXYJ(*), PYXJ(*) ,PXZJ(*),PZXJ(*) ,PYZJ(*),PZYJ(*),
     .   DD(3,3,*), KIJ(3,3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP
C     REAL
      my_real
     .   BI(3,3,MVSIZ),BJ(3,3,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IS==1) THEN
       DO I=LFT,LLT
        BI(1,1,I)=PXI(I)
        BI(2,2,I)=PYI(I)
        BI(3,3,I)=PZI(I)
        BI(1,2,I)=PYXI(I)
        BI(2,1,I)=PXYI(I)
        BI(1,3,I)=PZXI(I)
        BI(3,1,I)=PXZI(I)
        BI(2,3,I)=PZYI(I)
        BI(3,2,I)=PYZI(I)
       ENDDO
       DO I=1,3
       DO J=I,3
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+BI(1,I,EP)*(DD(1,1,EP)*BI(1,J,EP)+
     1                  DD(1,2,EP)*BI(2,J,EP)+DD(1,3,EP)*BI(3,J,EP))+
     2                BI(2,I,EP)*(DD(2,1,EP)*BI(1,J,EP)+
     3                  DD(2,2,EP)*BI(2,J,EP)+DD(2,3,EP)*BI(3,J,EP))+
     4                BI(3,I,EP)*(DD(3,1,EP)*BI(1,J,EP)+
     5                  DD(3,2,EP)*BI(2,J,EP)+DD(3,3,EP)*BI(3,J,EP))
        ENDDO
       ENDDO
       ENDDO
      ELSE
       DO I=LFT,LLT
        BI(1,1,I)=PXI(I)
        BI(2,2,I)=PYI(I)
        BI(3,3,I)=PZI(I)
        BI(1,2,I)=PYXI(I)
        BI(2,1,I)=PXYI(I)
        BI(1,3,I)=PZXI(I)
        BI(3,1,I)=PXZI(I)
        BI(2,3,I)=PZYI(I)
        BI(3,2,I)=PYZI(I)
       ENDDO
       DO I=LFT,LLT
        BJ(1,1,I)=PXJ(I)
        BJ(2,2,I)=PYJ(I)
        BJ(3,3,I)=PZJ(I)
        BJ(1,2,I)=PYXJ(I)
        BJ(2,1,I)=PXYJ(I)
        BJ(1,3,I)=PZXJ(I)
        BJ(3,1,I)=PXZJ(I)
        BJ(2,3,I)=PZYJ(I)
        BJ(3,2,I)=PYZJ(I)
       ENDDO
       DO I=1,3
       DO J=1,3
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+BI(1,I,EP)*(DD(1,1,EP)*BJ(1,J,EP)+
     1                  DD(1,2,EP)*BJ(2,J,EP)+DD(1,3,EP)*BJ(3,J,EP))+
     2                BI(2,I,EP)*(DD(2,1,EP)*BJ(1,J,EP)+
     3                  DD(2,2,EP)*BJ(2,J,EP)+DD(2,3,EP)*BJ(3,J,EP))+
     4                BI(3,I,EP)*(DD(3,1,EP)*BJ(1,J,EP)+
     5                  DD(3,2,EP)*BJ(2,J,EP)+DD(3,3,EP)*BJ(3,J,EP))
        ENDDO
       ENDDO
       ENDDO
      ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKEG33                      source/elements/solid/solide8z/s8zlke3.F
Chd|-- called by -----------
Chd|        S10CUMG3                      source/elements/solid/solide10/s10cumg3.F
Chd|        S20CUMG3                      source/elements/solid/solide20/s20cumg3.F
Chd|        S4CUMG3                       source/elements/solid/solide4/s4cumg3.F
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKEG33(LFT,LLT,
     .   PXYI, PYXI ,PXZI,PZXI ,PYZI,PZYI,
     .   PXYJ, PYXJ ,PXZJ,PZXJ ,PYZJ,PZYJ, G33, KIJ,IS)
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
      INTEGER LFT,LLT,IS
C     REAL
      my_real
     .   PXYI(*), PYXI(*) ,PXZI(*),PZXI(*) ,PYZI(*),PZYI(*),
     .   PXYJ(*), PYXJ(*) ,PXZJ(*),PZXJ(*) ,PYZJ(*),PZYJ(*),
     .   G33(3,3,*), KIJ(3,3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP
C     REAL
      my_real
     .   BI(3,3,MVSIZ),BJ(3,3,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IS==1) THEN
       DO I=LFT,LLT
        BI(1,1,I)=PXYI(I)
        BI(1,2,I)=PYXI(I)
        BI(1,3,I)=ZERO
        BI(2,1,I)=ZERO
        BI(2,2,I)=PYZI(I)
        BI(2,3,I)=PZYI(I)
        BI(3,1,I)=PXZI(I)
        BI(3,2,I)=ZERO
        BI(3,3,I)=PZXI(I)
       ENDDO
       DO I=1,3
       DO J=I,3
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+BI(1,I,EP)*(G33(1,1,EP)*BI(1,J,EP)+
     1                  G33(1,2,EP)*BI(2,J,EP)+G33(1,3,EP)*BI(3,J,EP))+
     2                BI(2,I,EP)*(G33(2,1,EP)*BI(1,J,EP)+
     3                  G33(2,2,EP)*BI(2,J,EP)+G33(2,3,EP)*BI(3,J,EP))+
     4                BI(3,I,EP)*(G33(3,1,EP)*BI(1,J,EP)+
     5                  G33(3,2,EP)*BI(2,J,EP)+G33(3,3,EP)*BI(3,J,EP))
        ENDDO
       ENDDO
       ENDDO
      ELSE
       DO I=LFT,LLT
        BI(1,1,I)=PXYI(I)
        BI(1,2,I)=PYXI(I)
        BI(1,3,I)=ZERO
        BI(2,1,I)=ZERO
        BI(2,2,I)=PYZI(I)
        BI(2,3,I)=PZYI(I)
        BI(3,1,I)=PXZI(I)
        BI(3,2,I)=ZERO
        BI(3,3,I)=PZXI(I)
       ENDDO
       DO I=LFT,LLT
        BJ(1,1,I)=PXYJ(I)
        BJ(1,2,I)=PYXJ(I)
        BJ(1,3,I)=ZERO
        BJ(2,1,I)=ZERO
        BJ(2,2,I)=PYZJ(I)
        BJ(2,3,I)=PZYJ(I)
        BJ(3,1,I)=PXZJ(I)
        BJ(3,2,I)=ZERO
        BJ(3,3,I)=PZXJ(I)
       ENDDO
       DO I=1,3
       DO J=1,3
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+BI(1,I,EP)*(G33(1,1,EP)*BJ(1,J,EP)+
     1                  G33(1,2,EP)*BJ(2,J,EP)+G33(1,3,EP)*BJ(3,J,EP))+
     2                BI(2,I,EP)*(G33(2,1,EP)*BJ(1,J,EP)+
     3                  G33(2,2,EP)*BJ(2,J,EP)+G33(2,3,EP)*BJ(3,J,EP))+
     4                BI(3,I,EP)*(G33(3,1,EP)*BJ(1,J,EP)+
     5                  G33(3,2,EP)*BJ(2,J,EP)+G33(3,3,EP)*BJ(3,J,EP))
        ENDDO
       ENDDO
       ENDDO
      ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKEBG3                      source/elements/solid/solide8z/s8zlke3.F
Chd|-- called by -----------
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKEBG3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ,
     .   BXYI, BYXI ,BXZI,BZXI ,BYZI,BZYI,
     .   BXYJ, BYXJ ,BXZJ,BZXJ ,BYZJ,BZYJ,
     .   PXYI, PYXI ,PXZI,PZXI ,PYZI,PZYI,
     .   PXYJ, PYXJ ,PXZJ,PZXJ ,PYZJ,PZYJ, DG, KIJ,IS,ICP)
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
      INTEGER LFT,LLT,IS,ICP
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),
     .   BXYI(*), BYXI(*) ,BXZI(*),BZXI(*) ,BYZI(*),BZYI(*),
     .   BXYJ(*), BYXJ(*) ,BXZJ(*),BZXJ(*) ,BYZJ(*),BZYJ(*),
     .   PXYI(*), PYXI(*) ,PXZI(*),PZXI(*) ,PYZI(*),PZYI(*),
     .   PXYJ(*), PYXJ(*) ,PXZJ(*),PZXJ(*) ,PYZJ(*),PZYJ(*),
     .   DG(3,3,*), KIJ(3,3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP
C     REAL
      my_real
     .   BI(3,3,MVSIZ),BJ(3,3,MVSIZ),CI(3,3,MVSIZ),CJ(3,3,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IS==1) THEN
       IF (ICP==1) THEN
        DO I=LFT,LLT
         BI(1,1,I)=PXI(I)
         BI(2,2,I)=PYI(I)
         BI(3,3,I)=PZI(I)
         BI(1,2,I)=ZERO
         BI(2,1,I)=ZERO
         BI(1,3,I)=ZERO
         BI(3,1,I)=ZERO
         BI(2,3,I)=ZERO
         BI(3,2,I)=ZERO
        ENDDO
       ELSE
        DO I=LFT,LLT
         BI(1,1,I)=PXI(I)
         BI(2,2,I)=PYI(I)
         BI(3,3,I)=PZI(I)
         BI(1,2,I)=BYXI(I)
         BI(2,1,I)=BXYI(I)
         BI(1,3,I)=BZXI(I)
         BI(3,1,I)=BXZI(I)
         BI(2,3,I)=BZYI(I)
         BI(3,2,I)=BYZI(I)
        ENDDO
       ENDIF!.. icp =1
       DO I=LFT,LLT
        CI(1,1,I)=PXYI(I)
        CI(1,2,I)=PYXI(I)
        CI(1,3,I)=ZERO
        CI(2,1,I)=ZERO
        CI(2,2,I)=PYZI(I)
        CI(2,3,I)=PZYI(I)
        CI(3,1,I)=PXZI(I)
        CI(3,2,I)=ZERO
        CI(3,3,I)=PZXI(I)
       ENDDO
C------use tempo CJ----
       DO I=1,3
       DO J=1,3
        DO EP=LFT,LLT
          CJ(I,J,EP)=ZERO
        ENDDO
       ENDDO
       ENDDO
C       
       DO I=1,3
       DO J=1,3
        DO EP=LFT,LLT
          CJ(I,J,EP)=CJ(I,J,EP)+BI(1,I,EP)*(DG(1,1,EP)*CI(1,J,EP)+
     1                  DG(1,2,EP)*CI(2,J,EP)+DG(1,3,EP)*CI(3,J,EP))+
     2                BI(2,I,EP)*(DG(2,1,EP)*CI(1,J,EP)+
     3                  DG(2,2,EP)*CI(2,J,EP)+DG(2,3,EP)*CI(3,J,EP))+
     4                BI(3,I,EP)*(DG(3,1,EP)*CI(1,J,EP)+
     5                  DG(3,2,EP)*CI(2,J,EP)+DG(3,3,EP)*CI(3,J,EP))
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,3
       DO J=I,3
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+CJ(I,J,EP)+CJ(J,I,EP)
        ENDDO
       ENDDO
       ENDDO
      ELSE ! IS = 0 -- non-symmetry
       IF (ICP==1) THEN
        DO I=LFT,LLT
         BI(1,1,I)=PXI(I)
         BI(2,2,I)=PYI(I)
         BI(3,3,I)=PZI(I)
         BI(1,2,I)=ZERO
         BI(2,1,I)=ZERO
         BI(1,3,I)=ZERO
         BI(3,1,I)=ZERO
         BI(2,3,I)=ZERO
         BI(3,2,I)=ZERO
        ENDDO
        DO I=LFT,LLT
         BJ(1,1,I)=PXJ(I)
         BJ(2,2,I)=PYJ(I)
         BJ(3,3,I)=PZJ(I)
         BJ(1,2,I)=ZERO
         BJ(2,1,I)=ZERO
         BJ(1,3,I)=ZERO
         BJ(3,1,I)=ZERO
         BJ(2,3,I)=ZERO
         BJ(3,2,I)=ZERO
        ENDDO
       ELSE ! .. icp = 0
        DO I=LFT,LLT
         BI(1,1,I)=PXI(I)
         BI(2,2,I)=PYI(I)
         BI(3,3,I)=PZI(I)
         BI(1,2,I)=BYXI(I)
         BI(2,1,I)=BXYI(I)
         BI(1,3,I)=BZXI(I)
         BI(3,1,I)=BXZI(I)
         BI(2,3,I)=BZYI(I)
         BI(3,2,I)=BYZI(I)
        ENDDO
        DO I=LFT,LLT
         BJ(1,1,I)=PXJ(I)
         BJ(2,2,I)=PYJ(I)
         BJ(3,3,I)=PZJ(I)
         BJ(1,2,I)=BYXJ(I)
         BJ(2,1,I)=BXYJ(I)
         BJ(1,3,I)=BZXJ(I)
         BJ(3,1,I)=BXZJ(I)
         BJ(2,3,I)=BZYJ(I)
         BJ(3,2,I)=BYZJ(I)
        ENDDO
       ENDIF
       DO I=LFT,LLT
        CI(1,1,I)=PXYI(I)
        CI(1,2,I)=PYXI(I)
        CI(1,3,I)=ZERO
        CI(2,1,I)=ZERO
        CI(2,2,I)=PYZI(I)
        CI(2,3,I)=PZYI(I)
        CI(3,1,I)=PXZI(I)
        CI(3,2,I)=ZERO
        CI(3,3,I)=PZXI(I)
       ENDDO
       DO I=LFT,LLT
        CJ(1,1,I)=PXYJ(I)
        CJ(1,2,I)=PYXJ(I)
        CJ(1,3,I)=ZERO
        CJ(2,1,I)=ZERO
        CJ(2,2,I)=PYZJ(I)
        CJ(2,3,I)=PZYJ(I)
        CJ(3,1,I)=PXZJ(I)
        CJ(3,2,I)=ZERO
        CJ(3,3,I)=PZXJ(I)
       ENDDO
       DO I=1,3
       DO J=1,3
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+BI(1,I,EP)*(DG(1,1,EP)*CJ(1,J,EP)+
     1                  DG(1,2,EP)*CJ(2,J,EP)+DG(1,3,EP)*CJ(3,J,EP))+
     2                BI(2,I,EP)*(DG(2,1,EP)*CJ(1,J,EP)+
     3                  DG(2,2,EP)*CJ(2,J,EP)+DG(2,3,EP)*CJ(3,J,EP))+
     4                BI(3,I,EP)*(DG(3,1,EP)*CJ(1,J,EP)+
     5                  DG(3,2,EP)*CJ(2,J,EP)+DG(3,3,EP)*CJ(3,J,EP))+
candr.. transpose of DG must be taken here
c$$$     6                CI(1,I,EP)*(DG(1,1,EP)*BJ(1,J,EP)+
c$$$     7                  DG(1,2,EP)*BJ(2,J,EP)+DG(1,3,EP)*BJ(3,J,EP))+
c$$$     8                CI(2,I,EP)*(DG(2,1,EP)*BJ(1,J,EP)+
c$$$     9                  DG(2,2,EP)*BJ(2,J,EP)+DG(2,3,EP)*BJ(3,J,EP))+
c$$$     A                CI(3,I,EP)*(DG(3,1,EP)*BJ(1,J,EP)+
c$$$     B                  DG(3,2,EP)*BJ(2,J,EP)+DG(3,3,EP)*BJ(3,J,EP))
     6                CI(1,I,EP)*(DG(1,1,EP)*BJ(1,J,EP)+
     7                  DG(2,1,EP)*BJ(2,J,EP)+DG(3,1,EP)*BJ(3,J,EP))+
     8                CI(2,I,EP)*(DG(1,2,EP)*BJ(1,J,EP)+
     9                  DG(2,2,EP)*BJ(2,J,EP)+DG(3,2,EP)*BJ(3,J,EP))+
     A                CI(3,I,EP)*(DG(1,3,EP)*BJ(1,J,EP)+
     B                  DG(2,3,EP)*BJ(2,J,EP)+DG(3,3,EP)*BJ(3,J,EP))

        ENDDO
       ENDDO
       ENDDO
      ENDIF
C 
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKEBG1                      source/elements/solid/solide8z/s8zlke3.F
Chd|-- called by -----------
Chd|        S10CUMG3                      source/elements/solid/solide10/s10cumg3.F
Chd|        S20CUMG3                      source/elements/solid/solide20/s20cumg3.F
Chd|        S4CUMG3                       source/elements/solid/solide4/s4cumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKEBG1(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, DG, KIJ,IS)
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
      INTEGER LFT,LLT,IS
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),
     .   DG(3,3,*), KIJ(3,3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EP
C     REAL
      my_real
     .   BI(3,MVSIZ),BJ(3,MVSIZ),CI(3,3,MVSIZ),CJ(3,3,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (IS==1) THEN
        DO I=LFT,LLT
         BI(1,I)=PXI(I)
         BI(2,I)=PYI(I)
         BI(3,I)=PZI(I)
        ENDDO
       DO I=LFT,LLT
        CI(1,1,I)=PYI(I)
        CI(1,2,I)=PXI(I)
        CI(1,3,I)=ZERO
        CI(2,1,I)=ZERO
        CI(2,2,I)=PZI(I)
        CI(2,3,I)=PYI(I)
        CI(3,1,I)=PZI(I)
        CI(3,2,I)=ZERO
        CI(3,3,I)=PXI(I)
       ENDDO
C------use tempo CJ----
       DO I=1,3
       DO J=1,3
        DO EP=LFT,LLT
          CJ(I,J,EP)=ZERO
        ENDDO
       ENDDO
       ENDDO
C
       DO J=1,3
        DO EP=LFT,LLT
          CJ(1,J,EP)=CJ(1,J,EP)+BI(1,EP)*(DG(1,1,EP)*CI(1,J,EP)+
     1                  DG(1,2,EP)*CI(2,J,EP)+DG(1,3,EP)*CI(3,J,EP))
          CJ(2,J,EP)=CJ(2,J,EP)+BI(2,EP)*(DG(2,1,EP)*CI(1,J,EP)+
     3                  DG(2,2,EP)*CI(2,J,EP)+DG(2,3,EP)*CI(3,J,EP))
          CJ(3,J,EP)=CJ(3,J,EP)+BI(3,EP)*(DG(3,1,EP)*CI(1,J,EP)+
     5                  DG(3,2,EP)*CI(2,J,EP)+DG(3,3,EP)*CI(3,J,EP))
        ENDDO
       ENDDO
C       
       DO I=1,3
       DO J=I,3
        DO EP=LFT,LLT
          KIJ(I,J,EP)=KIJ(I,J,EP)+CJ(I,J,EP)+CJ(J,I,EP)
        ENDDO
       ENDDO
       ENDDO
      ELSE
        DO I=LFT,LLT
         BI(1,I)=PXI(I)
         BI(2,I)=PYI(I)
         BI(3,I)=PZI(I)
        ENDDO
        DO I=LFT,LLT
         BJ(1,I)=PXJ(I)
         BJ(2,I)=PYJ(I)
         BJ(3,I)=PZJ(I)
        ENDDO
       DO I=LFT,LLT
        CI(1,1,I)=PYI(I)
        CI(1,2,I)=PXI(I)
        CI(1,3,I)=ZERO
        CI(2,1,I)=ZERO
        CI(2,2,I)=PZI(I)
        CI(2,3,I)=PYI(I)
        CI(3,1,I)=PZI(I)
        CI(3,2,I)=ZERO
        CI(3,3,I)=PXI(I)
       ENDDO
       DO I=LFT,LLT
        CJ(1,1,I)=PYJ(I)
        CJ(1,2,I)=PXJ(I)
        CJ(1,3,I)=ZERO
        CJ(2,1,I)=ZERO
        CJ(2,2,I)=PZJ(I)
        CJ(2,3,I)=PYJ(I)
        CJ(3,1,I)=PZJ(I)
        CJ(3,2,I)=ZERO
        CJ(3,3,I)=PXJ(I)
       ENDDO
C       
       DO J=1,3
        DO EP=LFT,LLT
          KIJ(1,J,EP)=KIJ(1,J,EP)+BI(1,EP)*(DG(1,1,EP)*CJ(1,J,EP)+
     1                  DG(1,2,EP)*CJ(2,J,EP)+DG(1,3,EP)*CJ(3,J,EP))
          KIJ(2,J,EP)=KIJ(2,J,EP)+BI(2,EP)*(DG(2,1,EP)*CJ(1,J,EP)+
     3                  DG(2,2,EP)*CJ(2,J,EP)+DG(2,3,EP)*CJ(3,J,EP))
          KIJ(3,J,EP)=KIJ(3,J,EP)+BI(3,EP)*(DG(3,1,EP)*CJ(1,J,EP)+
     5                  DG(3,2,EP)*CJ(2,J,EP)+DG(3,3,EP)*CJ(3,J,EP))
        ENDDO
       ENDDO
C
       DO I=1,3
        DO EP=LFT,LLT
          KIJ(I,1,EP)=KIJ(I,1,EP)+BJ(1,EP)*(DG(1,1,EP)*CI(1,I,EP)+
     1                  DG(1,2,EP)*CI(2,I,EP)+DG(1,3,EP)*CI(3,I,EP))
          KIJ(I,2,EP)=KIJ(I,2,EP)+BJ(2,EP)*(DG(1,2,EP)*CI(1,I,EP)+
     3                  DG(2,2,EP)*CI(2,I,EP)+DG(2,3,EP)*CI(3,I,EP))
          KIJ(I,3,EP)=KIJ(I,3,EP)+BJ(3,EP)*(DG(1,3,EP)*CI(1,I,EP)+
     5                  DG(3,2,EP)*CI(2,I,EP)+DG(3,3,EP)*CI(3,I,EP))
        ENDDO
       ENDDO
      ENDIF
C 
      RETURN
      END
