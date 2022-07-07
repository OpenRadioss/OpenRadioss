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
Chd|  S10CUMG3                      source/elements/solid/solide10/s10cumg3.F
Chd|-- called by -----------
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|-- calls ---------------
Chd|        S8ZKEBG1                      source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKED3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKEG3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKEG33                      source/elements/solid/solide8z/s8zlke3.F
Chd|====================================================================
      SUBROUTINE S10CUMG3(
     .   PX, PY, PZ, DD ,GG ,DG ,G33,IKSUP, 
     .   K11,K12,K13,K14,K15,K16,K17,K18,K19,K10,
     .   K22,K23,K24,K25,K26,K27,K28,K29,K20,K33,
     .   K34,K35,K36,K37,K38,K39,K30,K44,K45,K46,
     .   K47,K48,K49,K40,K55,K56,K57,K58,K59,K50,
     .   K66,K67,K68,K69,K60,K77,K78,K79,K70,K88,
     .   K89,K80,K99,K90,K00)
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
      INTEGER IKSUP
C     REAL
      my_real
     .  PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .   DD(3,3,*),GG(*),DG(9,*)  ,G33(9,*)
      my_real
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)    ,K15(9,*)   ,
     .   K16(9,*)   ,K17(9,*)   ,K18(9,*)    ,K19(9,*)    ,K10(9,*)   ,
     .   K22(9,*)   ,K23(9,*)   ,K24(9,*)    ,K25(9,*)    ,K26(9,*)   ,
     .   K27(9,*)   ,K28(9,*)   ,K29(9,*)    ,K20(9,*)    ,K33(9,*)   ,
     .   K34(9,*)   ,K35(9,*)   ,K36(9,*)    ,K37(9,*)    ,K38(9,*)   ,
     .   K39(9,*)   ,K30(9,*)   ,K44(9,*)   ,K45(9,*)    ,K46(9,*)    ,
     .   K47(9,*)   ,K48(9,*)   ,K49(9,*)   ,K40(9,*)    ,K55(9,*)   ,
     .   K56(9,*)   ,K57(9,*)   ,K58(9,*)   ,K59(9,*)    ,K50(9,*)   ,
     .   K66(9,*)   ,K67(9,*)   ,K68(9,*)   ,K69(9,*)    ,K60(9,*)   ,
     .   K77(9,*)   ,K78(9,*)   ,K79(9,*)   ,K70(9,*)    ,K88(9,*)   ,
     .   K89(9,*)   ,K80(9,*)   ,K99(9,*)   ,K90(9,*)    ,K00(9,*)   
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
C                                                                     12
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,1),PY(1,1),PZ(1,1),
     .             DD, K11,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,2),PY(1,2),PZ(1,2),
     .             DD, K22,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,3),PY(1,3),PZ(1,3),
     .             DD, K33,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,4),PY(1,4),PZ(1,4),
     .             DD, K44,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,5),PY(1,5),PZ(1,5),
     .             DD, K55,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,6),PY(1,6),PZ(1,6),
     .             DD, K66,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,7),PY(1,7),PZ(1,7),
     .             DD, K77,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,8),PY(1,8),PZ(1,8),
     .             DD, K88,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,9),PY(1,9),PZ(1,9),
     .             DD, K99,IS)
       CALL S8ZKED3(LFT,LLT,
     .            PX(1,10),PY(1,10),PZ(1,10),PX(1,10),PY(1,10),PZ(1,10),
     .             DD, K00,IS)
C
      IF (IKSUP.EQ.0) THEN
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             GG,K11,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             GG,K22,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             GG,K33,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             GG,K44,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K55,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K66,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K77,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K88,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K99,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K00,IS) 
      ENDIF 
C  -------non symm------
      IS =0
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,2),PY(1,2),PZ(1,2),
     .              DD, K12,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,3),PY(1,3),PZ(1,3),
     .              DD, K13,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,4),PY(1,4),PZ(1,4),
     .              DD, K14,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,5),PY(1,5),PZ(1,5),
     .              DD, K15,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K16,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K17,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K18,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K19,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K10,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,3),PY(1,3),PZ(1,3),
     .              DD, K23,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,4),PY(1,4),PZ(1,4),
     .              DD, K24,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,5),PY(1,5),PZ(1,5),
     .              DD, K25,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K26,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K27,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K28,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K29,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K20,IS)
      CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,4),PY(1,4),PZ(1,4),
     .              DD, K34,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,5),PY(1,5),PZ(1,5),
     .              DD, K35,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K36,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K37,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K38,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K39,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K30,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,5),PY(1,5),PZ(1,5),
     .              DD, K45,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K46,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K47,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K48,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K49,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K40,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K56,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K57,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K58,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K59,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K50,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K67,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K68,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K69,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K60,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K78,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K79,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K70,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,8),PY(1,8),PZ(1,8),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K89,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K80,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K90,IS)
C
      IF (IKSUP.EQ.0) THEN
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             GG,K12,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             GG,K13,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             GG,K14,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K15,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K16,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K17,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K18,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K19,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K10,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             GG,K23,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             GG,K24,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K25,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K26,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K27,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K28,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K29,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K20,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             GG,K34,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K35,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K36,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K37,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K38,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K39,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K30,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K45,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K46,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K47,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K48,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K49,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K40,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K56,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K57,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K58,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K59,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K50,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K67,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K68,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K69,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K60,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K78,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K79,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K70,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K89,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K80,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             GG,K90,IS) 
      ELSE 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             G33,K12,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             G33,K13,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             G33,K14,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K15,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K16,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K17,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K18,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K19,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K10,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             G33,K23,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             G33,K24,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K25,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K26,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K27,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K28,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K29,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K20,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             G33,K34,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K35,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K36,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K37,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K38,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K39,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K30,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K45,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K46,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K47,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K48,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K49,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K40,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K56,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K57,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K58,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K59,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K50,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K67,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K68,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K69,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K60,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K78,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K79,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K70,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K89,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K80,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K90,IS) 
C
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,2),PY(1,2),PZ(1,2),
     .              DG, K12,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,3),PY(1,3),PZ(1,3),
     .              DG, K13,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,4),PY(1,4),PZ(1,4),
     .              DG, K14,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,5),PY(1,5),PZ(1,5),
     .              DG, K15,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K16,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K17,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K18,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K19,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K10,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,3),PY(1,3),PZ(1,3),
     .              DG, K23,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,4),PY(1,4),PZ(1,4),
     .              DG, K24,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,5),PY(1,5),PZ(1,5),
     .              DG, K25,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K26,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K27,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K28,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K29,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K20,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,4),PY(1,4),PZ(1,4),
     .              DG, K34,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,5),PY(1,5),PZ(1,5),
     .              DG, K35,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K36,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K37,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K38,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K39,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K30,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,5),PY(1,5),PZ(1,5),
     .              DG, K45,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K46,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K47,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K48,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K49,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K40,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K56,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K57,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K58,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K59,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K50,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K67,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K68,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K69,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K60,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K78,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K79,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K70,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,8),PY(1,8),PZ(1,8),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K89,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K80,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K90,IS)
C     
       IS =1
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,1),PY(1,1),PZ(1,1),
     .             DG, K11,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,2),PY(1,2),PZ(1,2),
     .             DG, K22,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,3),PY(1,3),PZ(1,3),
     .             DG, K33,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,4),PY(1,4),PZ(1,4),
     .             DG, K44,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,5),PY(1,5),PZ(1,5),
     .             DG, K55,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,6),PY(1,6),PZ(1,6),
     .             DG, K66,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,7),PY(1,7),PZ(1,7),
     .             DG, K77,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,8),PY(1,8),PZ(1,8),
     .             DG, K88,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,9),PY(1,9),PZ(1,9),
     .             DG, K99,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .            PX(1,10),PY(1,10),PZ(1,10),PX(1,10),PY(1,10),PZ(1,10),
     .             DG, K00,IS)
C     
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             G33,K11,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             G33,K22,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             G33,K33,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             G33,K44,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K55,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K66,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K77,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K88,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K99,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .            PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .             G33,K00,IS) 
      ENDIF 
C
      RETURN
      END
