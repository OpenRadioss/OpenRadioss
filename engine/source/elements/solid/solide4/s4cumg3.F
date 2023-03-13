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
Chd|  S4CUMG3                       source/elements/solid/solide4/s4cumg3.F
Chd|-- called by -----------
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|-- calls ---------------
Chd|        S8ZKEBG1                      source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKED3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKEG3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKEG33                      source/elements/solid/solide8z/s8zlke3.F
Chd|====================================================================
      SUBROUTINE S4CUMG3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   K11 ,K12  ,K13  ,K14  ,K22  ,
     .   K23 ,K24  ,K33  ,K34  , K44 ,
     .   DD  ,GG   ,DG   ,G33  ,IKSUP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKSUP
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   K11(9,*) ,K12(9,*)  ,K13(9,*)  ,K14(9,*)  ,K22(9,*)  ,
     .   K23(9,*) ,K24(9,*)  ,K33(9,*)  ,K34(9,*)  ,K44(9,*)  ,
     .   DD(3,3,*),GG(*),DG(9,*)  ,G33(9,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IS
      my_real
     .   TT,TV
C-----------------------------------------------
C  -------symm(diag)------
      IS =1
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, DD, K11,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, DD, K22,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, DD, K33,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, DD, K44,IS)
C
      IF (IKSUP==0) THEN
       CALL S8ZKEG3(LFT,LLT,PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,
     .                      PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,GG,K11,IS) 
       CALL S8ZKEG3(LFT,LLT,PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,
     .                      PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,GG,K22,IS) 
       CALL S8ZKEG3(LFT,LLT,PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,
     .                      PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,GG,K33,IS) 
       CALL S8ZKEG3(LFT,LLT,PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,
     .                      PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,GG,K44,IS) 
      ENDIF 
C  -------non symm------
      IS =0
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, DD, K12,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, DD, K13,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, DD, K14,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, DD, K23,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, DD, K24,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, DD, K34,IS)
C
      IF (IKSUP==0) THEN
       CALL S8ZKEG3(LFT,LLT,PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,
     .                      PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,GG,K12,IS) 
       CALL S8ZKEG3(LFT,LLT,PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,
     .                      PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,GG,K13,IS) 
       CALL S8ZKEG3(LFT,LLT,PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,
     .                      PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,GG,K14,IS) 
       CALL S8ZKEG3(LFT,LLT,PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,
     .                      PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,GG,K23,IS) 
       CALL S8ZKEG3(LFT,LLT,PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,
     .                      PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,GG,K24,IS) 
       CALL S8ZKEG3(LFT,LLT,PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,
     .                      PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,GG,K34,IS) 
      ELSE 
       CALL S8ZKEG33(LFT,LLT,PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,
     .                      PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,G33,K12,IS) 
       CALL S8ZKEG33(LFT,LLT,PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,
     .                      PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,G33,K13,IS) 
       CALL S8ZKEG33(LFT,LLT,PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,
     .                      PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,G33,K14,IS) 
       CALL S8ZKEG33(LFT,LLT,PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,
     .                      PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,G33,K23,IS) 
       CALL S8ZKEG33(LFT,LLT,PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,
     .                      PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,G33,K24,IS) 
       CALL S8ZKEG33(LFT,LLT,PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,
     .                      PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,G33,K34,IS) 
C
       CALL S8ZKEBG1(LFT,LLT,PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, DG, K12,IS)
       CALL S8ZKEBG1(LFT,LLT,PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, DG, K13,IS)
       CALL S8ZKEBG1(LFT,LLT,PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, DG, K14,IS)
       CALL S8ZKEBG1(LFT,LLT,PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, DG, K23,IS)
       CALL S8ZKEBG1(LFT,LLT,PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, DG, K24,IS)
       CALL S8ZKEBG1(LFT,LLT,PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, DG, K34,IS)
       IS =1
       CALL S8ZKEBG1(LFT,LLT,PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, DG, K11,IS)
       CALL S8ZKEBG1(LFT,LLT,PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, DG, K22,IS)
       CALL S8ZKEBG1(LFT,LLT,PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, DG, K33,IS)
       CALL S8ZKEBG1(LFT,LLT,PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, DG, K44,IS)
C       
       CALL S8ZKEG33(LFT,LLT,PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,
     .                      PY1, PX1 ,PZ1,PX1 ,PZ1,PY1,G33,K11,IS) 
       CALL S8ZKEG33(LFT,LLT,PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,
     .                      PY2, PX2 ,PZ2,PX2 ,PZ2,PY2,G33,K22,IS) 
       CALL S8ZKEG33(LFT,LLT,PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,
     .                      PY3, PX3 ,PZ3,PX3 ,PZ3,PY3,G33,K33,IS) 
       CALL S8ZKEG33(LFT,LLT,PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,
     .                      PY4, PX4 ,PZ4,PX4 ,PZ4,PY4,G33,K44,IS) 
      ENDIF 
C
      RETURN
      END
