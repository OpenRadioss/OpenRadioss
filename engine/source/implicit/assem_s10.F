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
Chd|  ASSEM_S10                     source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|-- calls ---------------
Chd|        ASS10_KKII                    source/implicit/assem_s10.F   
Chd|        ASS10_KKIJ                    source/implicit/assem_s10.F   
Chd|        ASS10_KKJI                    source/implicit/assem_s10.F   
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        ASSEM_KIJ                     source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_S10(
     1                   IXS   ,IXS10 ,NEL   ,IDDL  ,NDOF  ,
     2                   K_DIAG,K_LT  ,IADK  ,JDIK  ,K11   ,     
     3                   K12   ,K13   ,K14   ,K15   ,K16   ,
     4                   K17   ,K18   ,K19   ,K10   ,K22   ,
     5                   K23   ,K24   ,K25   ,K26   ,K27   ,
     6                   K28   ,K29   ,K20   ,K33   ,K34   ,
     7                   K35   ,K36   ,K37   ,K38   ,K39   ,
     8                   K30   ,K44   ,K45   ,K46   ,K47   ,
     9                   K48   ,K49   ,K40   ,K55   ,K56   ,
     A                   K57   ,K58   ,K59   ,K50   ,K66   ,
     B                   K67   ,K68   ,K69   ,K60   ,K77   ,
     C                   K78   ,K79   ,K70   ,K88   ,K89   ,
     D                   K80   ,K99   ,K90   ,K00   ,OFF   )
C----6---------------------------------------------------------------7---------8
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*),NEL   ,IDDL(*)  ,NDOF(*)  ,
     .          IADK(*)  ,JDIK(*)        
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,OFF(*),
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
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER EP,NN(MVSIZ,10),IPERM1(10),IPERM2(10),N,N1,N2,I
      INTEGER NI,NJ,N10
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C----6---------------------------------------------------------------7---------8
        N10= 0
        DO EP = 1,NEL
         NN(EP,1)=IXS(2,EP)
         NN(EP,2)=IXS(4,EP)
         NN(EP,3)=IXS(7,EP)
         NN(EP,4)=IXS(6,EP)
         NN(EP,5) =IXS10(1,EP)
         NN(EP,6) =IXS10(2,EP)
         NN(EP,7) =IXS10(3,EP)
         NN(EP,8) =IXS10(4,EP)
         NN(EP,9) =IXS10(5,EP)
         NN(EP,10)=IXS10(6,EP)
         IF (NN(EP,5)>0.AND.NN(EP,6)>0.AND.NN(EP,7)>0.AND.
     .       NN(EP,8)>0.AND.NN(EP,9)>0.AND.NN(EP,10)>0) 
     .    N10 = N10 + 1  
        ENDDO
C---------condense first in KIJ--for degenerated element------------
      IF (N10<NEL) THEN
C-------block k00,k10,k20,k30,k40,k50,k60,k70,k80,k90-----
       N=10
C---------N1=3,N2=4--------------
       N1=IPERM1(N)
       N2=IPERM2(N)
       DO EP = 1,NEL
        IF (NN(EP,N)==0.AND.OFF(EP)>ZERO) THEN
          CALL ASS10_KKII(N1   ,N2    ,K33(1,EP),K44(1,EP),K34(1,EP),
     1                    K00(1,EP),3  )
C------- k10,k20,k30,k40,50,60,70,80,90-----
          CALL ASS10_KKIJ(1     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K44(1,EP),K13(1,EP),K14(1,EP),
     2                  K10(1,EP))
          CALL ASS10_KKIJ(2     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K44(1,EP),K23(1,EP),K24(1,EP),
     2                  K20(1,EP))
          CALL ASS10_KKIJ(3     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K44(1,EP),K33(1,EP),K34(1,EP),
     2                  K30(1,EP))
          CALL ASS10_KKIJ(4     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K44(1,EP),K34(1,EP),K44(1,EP),
     2                  K40(1,EP))
          CALL ASS10_KKIJ(5     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K44(1,EP),K35(1,EP),K45(1,EP),
     2                  K50(1,EP))
          CALL ASS10_KKIJ(6     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K44(1,EP),K36(1,EP),K46(1,EP),
     2                  K60(1,EP))
          CALL ASS10_KKIJ(7     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K44(1,EP),K37(1,EP),K47(1,EP),
     2                  K70(1,EP))
          CALL ASS10_KKIJ(8     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K44(1,EP),K38(1,EP),K48(1,EP),
     2                  K80(1,EP))
          CALL ASS10_KKIJ(9     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K44(1,EP),K39(1,EP),K49(1,EP),
     2                  K90(1,EP))
        ENDIF 
       ENDDO 
C-------block k99,k19,k29,k39,k49,k59,k69,k79,k89-----
       N=9
C---------N1=2,N2=4--------------
       N1=IPERM1(N)
       N2=IPERM2(N)
       DO EP = 1,NEL
        IF (NN(EP,N)==0.AND.OFF(EP)>ZERO) THEN
          CALL ASS10_KKII(N1   ,N2    ,K22(1,EP),K44(1,EP),K24(1,EP),
     1                    K99(1,EP),3  )
C------- k19,k29,k39,k49,59,69,79,89-----
          CALL ASS10_KKIJ(1     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K44(1,EP),K12(1,EP),K14(1,EP),
     2                  K19(1,EP))
          CALL ASS10_KKIJ(2     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K44(1,EP),K22(1,EP),K24(1,EP),
     2                  K29(1,EP))
          CALL ASS10_KKIJ(3     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K44(1,EP),K23(1,EP),K34(1,EP),
     2                  K39(1,EP))
          CALL ASS10_KKIJ(4     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K44(1,EP),K24(1,EP),K44(1,EP),
     2                  K49(1,EP))
          CALL ASS10_KKIJ(5     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K44(1,EP),K25(1,EP),K45(1,EP),
     2                  K59(1,EP))
          CALL ASS10_KKIJ(6     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K44(1,EP),K26(1,EP),K46(1,EP),
     2                  K69(1,EP))
          CALL ASS10_KKIJ(7     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K44(1,EP),K27(1,EP),K47(1,EP),
     2                  K79(1,EP))
          CALL ASS10_KKIJ(8     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K44(1,EP),K28(1,EP),K48(1,EP),
     2                  K89(1,EP))
          CALL ASS10_KKJI(10     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K44(1,EP),K20(1,EP),K40(1,EP),
     2                  K90(1,EP))
        ENDIF 
       ENDDO 
C-------block k88,k18,k28,k38,k48,k58,k68,k78-----
       N=8
C---------N1=1,N2=4--------------
       N1=IPERM1(N)
       N2=IPERM2(N)
       DO EP = 1,NEL
        IF (NN(EP,N)==0.AND.OFF(EP)>ZERO) THEN
          CALL ASS10_KKII(N1   ,N2    ,K11(1,EP),K44(1,EP),K14(1,EP),
     1                    K88(1,EP),3  )
C------- k18,k28,k38,k48,58,68,78-----
          CALL ASS10_KKIJ(1     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K11(1,EP),K14(1,EP),
     2                  K18(1,EP))
          CALL ASS10_KKIJ(2     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K12(1,EP),K24(1,EP),
     2                  K28(1,EP))
          CALL ASS10_KKIJ(3     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K13(1,EP),K34(1,EP),
     2                  K38(1,EP))
          CALL ASS10_KKIJ(4     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K14(1,EP),K44(1,EP),
     2                  K48(1,EP))
          CALL ASS10_KKIJ(5     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K15(1,EP),K45(1,EP),
     2                  K58(1,EP))
          CALL ASS10_KKIJ(6     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K16(1,EP),K46(1,EP),
     2                  K68(1,EP))
          CALL ASS10_KKIJ(7     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K17(1,EP),K47(1,EP),
     2                  K78(1,EP))
          CALL ASS10_KKJI(9     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K19(1,EP),K49(1,EP),
     2                  K89(1,EP))
          CALL ASS10_KKJI(10     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K10(1,EP),K40(1,EP),
     2                  K80(1,EP))
        ENDIF 
       ENDDO 
C-------block k77,k17,k27,k37,k47,k57,k67-----
       N=7
C---------N1=3,N2=1--------------
       N1=IPERM1(N)
       N2=IPERM2(N)
       DO EP = 1,NEL
        IF (NN(EP,N)==0.AND.OFF(EP)>ZERO) THEN
          CALL ASS10_KKII(N1   ,N2    ,K33(1,EP),K11(1,EP),K13(1,EP),
     1                    K77(1,EP),3  )
C------- k17,k27,k37,k47,57,67-----
          CALL ASS10_KKIJ(1     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K11(1,EP),K13(1,EP),K11(1,EP),
     2                  K17(1,EP))
          CALL ASS10_KKIJ(2     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K11(1,EP),K23(1,EP),K12(1,EP),
     2                  K27(1,EP))
          CALL ASS10_KKIJ(3     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K11(1,EP),K33(1,EP),K13(1,EP),
     2                  K37(1,EP))
          CALL ASS10_KKIJ(4     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K11(1,EP),K34(1,EP),K14(1,EP),
     2                  K47(1,EP))
          CALL ASS10_KKIJ(5     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K11(1,EP),K35(1,EP),K15(1,EP),
     2                  K57(1,EP))
          CALL ASS10_KKIJ(6     ,N1    ,N2    ,3     ,
     1                  K33(1,EP),K11(1,EP),K36(1,EP),K16(1,EP),
     2                  K67(1,EP))
          CALL ASS10_KKJI(8     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K38(1,EP),K18(1,EP),
     2                  K78(1,EP))
          CALL ASS10_KKJI(9     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K39(1,EP),K19(1,EP),
     2                  K79(1,EP))
          CALL ASS10_KKJI(10     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K44(1,EP),K30(1,EP),K10(1,EP),
     2                  K70(1,EP))
        ENDIF 
       ENDDO 
C-------block k66,k16,k26,k36,k46,k56-----
       N=6
C---------N1=2,N2=3--------------
       N1=IPERM1(N)
       N2=IPERM2(N)
       DO EP = 1,NEL
        IF (NN(EP,N)==0.AND.OFF(EP)>ZERO) THEN
          CALL ASS10_KKII(N1   ,N2    ,K22(1,EP),K33(1,EP),K23(1,EP),
     1                    K66(1,EP),3  )
C------- k16,k26,k36,k46,56-----
          CALL ASS10_KKIJ(1     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K33(1,EP),K12(1,EP),K13(1,EP),
     2                  K16(1,EP))
          CALL ASS10_KKIJ(2     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K33(1,EP),K22(1,EP),K23(1,EP),
     2                  K26(1,EP))
          CALL ASS10_KKIJ(3     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K33(1,EP),K23(1,EP),K33(1,EP),
     2                  K36(1,EP))
          CALL ASS10_KKIJ(4     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K33(1,EP),K24(1,EP),K34(1,EP),
     2                  K46(1,EP))
          CALL ASS10_KKIJ(5     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K33(1,EP),K25(1,EP),K35(1,EP),
     2                  K56(1,EP))
          CALL ASS10_KKJI(7     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K33(1,EP),K27(1,EP),K37(1,EP),
     2                  K67(1,EP))
          CALL ASS10_KKJI(8     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K33(1,EP),K28(1,EP),K38(1,EP),
     2                  K68(1,EP))
          CALL ASS10_KKJI(9     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K33(1,EP),K29(1,EP),K39(1,EP),
     2                  K69(1,EP))
          CALL ASS10_KKJI(10     ,N1    ,N2    ,3     ,
     1                  K22(1,EP),K33(1,EP),K20(1,EP),K30(1,EP),
     2                  K60(1,EP))
        ENDIF 
       ENDDO 
C-------block k55,k15,k25,k35,k45-----
       N=5
C---------N1=1,N2=2--------------
       N1=IPERM1(N)
       N2=IPERM2(N)
       DO EP = 1,NEL
        IF (NN(EP,N)==0.AND.OFF(EP)>ZERO) THEN
          CALL ASS10_KKII(N1   ,N2    ,K11(1,EP),K22(1,EP),K12(1,EP),
     1                    K55(1,EP),3  )
C------- k15,k25,k35,k45-----
          CALL ASS10_KKIJ(1     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K22(1,EP),K11(1,EP),K12(1,EP),
     2                  K15(1,EP))
          CALL ASS10_KKIJ(2     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K22(1,EP),K12(1,EP),K22(1,EP),
     2                  K25(1,EP))
          CALL ASS10_KKIJ(3     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K22(1,EP),K13(1,EP),K23(1,EP),
     2                  K35(1,EP))
          CALL ASS10_KKIJ(4     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K22(1,EP),K14(1,EP),K24(1,EP),
     2                  K45(1,EP))
          CALL ASS10_KKJI(6     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K22(1,EP),K16(1,EP),K26(1,EP),
     2                  K56(1,EP))
          CALL ASS10_KKJI(7     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K22(1,EP),K17(1,EP),K27(1,EP),
     2                  K57(1,EP))
          CALL ASS10_KKJI(8     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K22(1,EP),K18(1,EP),K28(1,EP),
     2                  K58(1,EP))
          CALL ASS10_KKJI(9     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K22(1,EP),K19(1,EP),K29(1,EP),
     2                  K59(1,EP))
          CALL ASS10_KKJI(10     ,N1    ,N2    ,3     ,
     1                  K11(1,EP),K22(1,EP),K10(1,EP),K20(1,EP),
     2                  K50(1,EP))
        ENDIF 
       ENDDO 
      END IF !(N10<NEL) THEN
C-------block diagonal k11,k22...k44-----
       CALL ASSEM_KII(NN(1,1),NEL,IDDL,IADK,K_DIAG,K_LT ,K11,3,OFF) 
       CALL ASSEM_KII(NN(1,2),NEL,IDDL,IADK,K_DIAG,K_LT ,K22,3,OFF) 
       CALL ASSEM_KII(NN(1,3),NEL,IDDL,IADK,K_DIAG,K_LT ,K33,3,OFF) 
       CALL ASSEM_KII(NN(1,4),NEL,IDDL,IADK,K_DIAG,K_LT ,K44,3,OFF) 
C-------non diag kij-----
       CALL ASSEM_KIJ(NN(1,1),NN(1,2),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K12,3,OFF) 
       CALL ASSEM_KIJ(NN(1,1),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K13,3,OFF) 
       CALL ASSEM_KIJ(NN(1,1),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K14,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K23,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K24,3,OFF) 
       CALL ASSEM_KIJ(NN(1,3),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K34,3,OFF)
C-------- 
        CALL ASSEM_KII(NN(1,5),NEL,IDDL,IADK,K_DIAG,K_LT ,K55,3,OFF) 
        CALL ASSEM_KII(NN(1,6),NEL,IDDL,IADK,K_DIAG,K_LT ,K66,3,OFF) 
        CALL ASSEM_KII(NN(1,7),NEL,IDDL,IADK,K_DIAG,K_LT ,K77,3,OFF) 
        CALL ASSEM_KII(NN(1,8),NEL,IDDL,IADK,K_DIAG,K_LT ,K88,3,OFF) 
        CALL ASSEM_KII(NN(1,9),NEL,IDDL,IADK,K_DIAG,K_LT ,K99,3,OFF) 
        CALL ASSEM_KII(NN(1,10),NEL,IDDL,IADK,K_DIAG,K_LT ,K00,3,OFF) 
C-------non diag kij-----
       CALL ASSEM_KIJ(NN(1,1),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K15,3,OFF) 
       CALL ASSEM_KIJ(NN(1,1),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K16,3,OFF) 
       CALL ASSEM_KIJ(NN(1,1),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K17,3,OFF) 
       CALL ASSEM_KIJ(NN(1,1),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K18,3,OFF) 
       CALL ASSEM_KIJ(NN(1,1),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K19,3,OFF) 
       CALL ASSEM_KIJ(NN(1,1),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K10,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K25,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K26,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K27,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K28,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K29,3,OFF) 
       CALL ASSEM_KIJ(NN(1,2),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K20,3,OFF) 
       CALL ASSEM_KIJ(NN(1,3),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K35,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K36,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K37,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K38,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K39,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K30,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K45,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K46,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K47,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K48,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K49,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K40,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K56,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K57,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K58,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K59,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K50,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K67,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K68,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K69,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K60,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K78,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K79,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K70,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K89,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K80,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K90,3,OFF)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ASS10_KII                     source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ASSEM_KII1                    source/implicit/assem_s10.F   
Chd|        ASSEM_KIJ1                    source/implicit/assem_s10.F   
Chd|====================================================================
      SUBROUTINE ASS10_KII(NC   ,N1    ,N2    ,
     1                    IDDL  ,IADK  ,JDIK  ,K_DIAG,K_LT  ,
     2                    KJJ   ,ND    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND,NC(10),N1,N2,IDDL(*) , IADK(*) ,JDIK(*)      
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,KJJ(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   KII(ND,ND)
C------------------------------
         DO I =1,ND
         DO J =I,ND
          KII(I,J) = FOURTH*KJJ(I,J)
         ENDDO 
         ENDDO 
         DO I =1,ND
         DO J =I+1,ND
          KII(J,I) = KII(I,J)
         ENDDO 
         ENDDO 
C
          CALL ASSEM_KII1(NC(N1),IDDL  ,IADK ,K_DIAG,
     1                    K_LT  ,KII   ,ND   )
          CALL ASSEM_KII1(NC(N2),IDDL  ,IADK ,K_DIAG,
     1                    K_LT  ,KII   ,ND   )
          CALL ASSEM_KIJ1(NC(N1),NC(N2),IDDL ,IADK,JDIK,
     1                   K_DIAG,K_LT  ,KII  ,ND )
C
      RETURN
      END
Chd|====================================================================
Chd|  ASS10_KIJ                     source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ASSEM_KII1                    source/implicit/assem_s10.F   
Chd|        ASSEM_KIJ1                    source/implicit/assem_s10.F   
Chd|====================================================================
      SUBROUTINE ASS10_KIJ(NC   ,NI    ,NJ1   ,NJ2   ,ND    ,
     1                    IDDL  ,IADK  ,JDIK  ,K_DIAG,K_LT  ,
     2                    KIJ0  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND,NC(10),NI,NJ1,NJ2,IDDL(*) , IADK(*) ,JDIK(*)      
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,KIJ0(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NJ
      my_real
     .   KII(ND,ND),KIJ(ND,ND)
C------------------------------
        DO I =1,ND
        DO J =1,ND
         KIJ(I,J) = HALF*KIJ0(I,J)
        ENDDO 
        ENDDO
C 
        NJ=NJ1
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           KII(I,J) = KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
          CALL ASSEM_KII1(NC(NI),IDDL  ,IADK ,K_DIAG,
     1                    K_LT  ,KII   ,ND   )
        ELSEIF (NI>NJ) THEN
          CALL ASSEM_KIJ1(NC(NJ),NC(NI),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ELSE
          CALL ASSEM_KIJ1(NC(NI),NC(NJ),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ENDIF
        NJ=NJ2
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           KII(I,J) = KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
          CALL ASSEM_KII1(NC(NI),IDDL  ,IADK ,K_DIAG,
     1                    K_LT  ,KII   ,ND   )
        ELSEIF (NI>NJ) THEN
          CALL ASSEM_KIJ1(NC(NJ),NC(NI),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ELSE
          CALL ASSEM_KIJ1(NC(NI),NC(NJ),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  ASS10_KIJ1                    source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ASSEM_KII1                    source/implicit/assem_s10.F   
Chd|        ASSEM_KIJ1                    source/implicit/assem_s10.F   
Chd|====================================================================
      SUBROUTINE ASS10_KIJ1(NC   ,NI1   ,NI2   ,NJ1   ,NJ2   ,
     1                     IDDL  ,IADK  ,JDIK  ,K_DIAG,K_LT  ,
     2                     KIJ0  ,ND    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND,NC(10),NI1,NI2,NJ1,NJ2,IDDL(*) , IADK(*) ,JDIK(*)      
      my_real
     .   K_DIAG(*) ,K_LT(*) ,KIJ0(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NI,NJ
      my_real
     .   KII(ND,ND),KIJ(ND,ND)
C------------------------------
        DO I =1,ND
        DO J =1,ND
         KIJ(I,J) = FOURTH*KIJ0(I,J)
        ENDDO 
        ENDDO
C 
        NI=NI1
        NJ=NJ1
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           KII(I,J) = KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
          CALL ASSEM_KII1(NC(NI),IDDL  ,IADK ,K_DIAG,
     1                    K_LT  ,KII   ,ND   )
        ELSEIF (NI>NJ) THEN
          CALL ASSEM_KIJ1(NC(NJ),NC(NI),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ELSE
          CALL ASSEM_KIJ1(NC(NI),NC(NJ),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ENDIF
C
        NJ=NJ2
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           KII(I,J) = KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
          CALL ASSEM_KII1(NC(NI),IDDL  ,IADK ,K_DIAG,
     1                    K_LT  ,KII   ,ND   )
        ELSEIF (NI>NJ) THEN
          CALL ASSEM_KIJ1(NC(NJ),NC(NI),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ELSE
          CALL ASSEM_KIJ1(NC(NI),NC(NJ),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ENDIF
C
        NI=NI2
        NJ=NJ1
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           KII(I,J) = KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
          CALL ASSEM_KII1(NC(NI),IDDL  ,IADK ,K_DIAG,
     1                    K_LT  ,KII   ,ND   )
        ELSEIF (NI>NJ) THEN
          CALL ASSEM_KIJ1(NC(NJ),NC(NI),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ELSE
          CALL ASSEM_KIJ1(NC(NI),NC(NJ),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ENDIF
C
        NJ=NJ2
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           KII(I,J) = KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
          CALL ASSEM_KII1(NC(NI),IDDL  ,IADK ,K_DIAG,
     1                    K_LT  ,KII   ,ND   )
        ELSEIF (NI>NJ) THEN
          CALL ASSEM_KIJ1(NC(NJ),NC(NI),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ELSE
          CALL ASSEM_KIJ1(NC(NI),NC(NJ),IDDL  ,IADK,JDIK,
     1                    K_DIAG,K_LT   ,KIJ  ,ND   )
        ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  ASSEM_KII1                    source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|        ASS10_KII                     source/implicit/assem_s10.F   
Chd|        ASS10_KIJ                     source/implicit/assem_s10.F   
Chd|        ASS10_KIJ1                    source/implicit/assem_s10.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSEM_KII1(NI    ,IDDL  ,IADK  ,K_DIAG,K_LT  ,
     1                      KII   ,ND    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND        
      INTEGER NI,IDDL(*)  , IADK(*)        
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,KII(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,K,IK,ID,JD,L
C----6----------KII is always triag_sup whatever IKPAT---------------7--------
       N = NI
       ID = IDDL(N)
       IF (IKPAT==0) THEN
C   minimize lockon/lockoff
#include "lockon.inc"
        DO K=1,ND
c#include "lockon.inc"
          K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K)
c#include "lockoff.inc"
          JD = IADK(ID+K)-1
          DO L=K+1,ND
           IK = JD+L-K
c#include "lockon.inc"
           K_LT(IK) = K_LT(IK) + KII(K,L)
c#include "lockoff.inc"
          ENDDO
        ENDDO 
#include "lockoff.inc"
        ELSE
#include "lockon.inc"
        DO K=1,ND
c#include "lockon.inc"
          K_DIAG(ID+K) = K_DIAG(ID+K) + KII(K,K)
c#include "lockoff.inc"
          JD = IADK(ID+K+1)-K
          DO L=1,K-1
           IK = JD+L
c#include "lockon.inc"
           K_LT(IK) = K_LT(IK) + KII(L,K)
c#include "lockoff.inc"
          ENDDO
        ENDDO 
#include "lockoff.inc"
       ENDIF 
C
      RETURN
      END
Chd|====================================================================
Chd|  ASSEM_KIJ1                    source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|        ASS10_KII                     source/implicit/assem_s10.F   
Chd|        ASS10_KIJ                     source/implicit/assem_s10.F   
Chd|        ASS10_KIJ1                    source/implicit/assem_s10.F   
Chd|        ASS10_KIJ2                    source/implicit/assem_s10.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSEM_KIJ1(NI    ,NJ    ,IDDL  ,IADK,JDIK,
     1                      K_DIAG,K_LT  ,KIJ   ,ND  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND,NI,NJ,IDDL(*)  ,IADK(*),JDIK(*)       
      my_real
     .   K_DIAG(*),K_LT(*)   ,KIJ(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,JD,JDL,L,JJ
C----6---------------------------------------------------------------7---------8
       IF (IKPAT==0) THEN
        I = IDDL(NI)
        J = IDDL(NJ)
        ID = MIN(I,J)
        JD = MAX(I,J)+1
        IF (I==ID) THEN
         DO K=1,ND
          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
           IF (JDIK(JJ)==JD) THEN
            JDL = JJ-1
            GOTO 100
           ENDIF
          ENDDO 
 100      CONTINUE
C   minimize lockon/lockoff
#include "lockon.inc"
          DO L=1,ND
c#include "lockon.inc"
           K_LT(JDL+L) = K_LT(JDL+L) + KIJ(K,L)
c#include "lockoff.inc"
          ENDDO
#include "lockoff.inc"
         ENDDO 
        ELSE
         DO K=1,ND
          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
           IF (JDIK(JJ)==JD) THEN
            JDL = JJ-1
            GOTO 200
           ENDIF
          ENDDO 
 200      CONTINUE
#include "lockon.inc"
          DO L=1,ND
c#include "lockon.inc"
           K_LT(JDL+L) = K_LT(JDL+L) + KIJ(L,K)
c#include "lockoff.inc"
          ENDDO
#include "lockoff.inc"
         ENDDO 
        ENDIF
      ELSE 
        I = IDDL(NI)
        J = IDDL(NJ)
        ID = MAX(I,J)
        JD = MIN(I,J)+1
        IF (I==ID) THEN
         DO K=1,ND
          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
           IF (JDIK(JJ)==JD) THEN
            JDL = JJ-1
            GOTO 300
           ENDIF
          ENDDO 
 300      CONTINUE
#include "lockon.inc"
          DO L=1,ND
c#include "lockon.inc"
           K_LT(JDL+L) = K_LT(JDL+L) + KIJ(K,L)
c#include "lockoff.inc"
          ENDDO
#include "lockoff.inc"
         ENDDO 
        ELSE
         DO K=1,ND
          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
           IF (JDIK(JJ)==JD) THEN
            JDL = JJ-1
            GOTO 400
           ENDIF
          ENDDO 
 400      CONTINUE
#include "lockon.inc"
          DO L=1,ND
c#include "lockon.inc"
           K_LT(JDL+L) = K_LT(JDL+L) + KIJ(L,K)
c#include "lockoff.inc"
          ENDDO
#include "lockoff.inc"
         ENDDO 
        ENDIF
       ENDIF 
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ASS10_KIJ2                    source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ASSEM_KIJ1                    source/implicit/assem_s10.F   
Chd|====================================================================
      SUBROUTINE ASS10_KIJ2(NC   ,NI    ,NJ1   ,NJ2   ,ND    ,
     1                    IDDL  ,IADK  ,JDIK  ,K_DIAG,K_LT  ,
     2                    KIJ0  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND,NC(10),NI,NJ1,NJ2,IDDL(*) , IADK(*) ,JDIK(*)      
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,KIJ0(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NJ
      my_real
     .   KII(ND,ND),KIJ(ND,ND)
C------------------------------
        DO I =1,ND
        DO J =1,ND
         KIJ(I,J) = HALF*KIJ0(I,J)
        ENDDO 
        ENDDO
C 
        NJ=NJ1

        CALL ASSEM_KIJ1(NC(NI),NC(NJ),IDDL  ,IADK,JDIK,
     1                  K_DIAG,K_LT   ,KIJ  ,ND   )
        NJ=NJ2

        CALL ASSEM_KIJ1(NC(NI),NC(NJ),IDDL  ,IADK,JDIK,
     1                  K_DIAG,K_LT   ,KIJ  ,ND   )
C
      RETURN
      END
Chd|====================================================================
Chd|  ASS10_KKII                    source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|        ASSEM_S10                     source/implicit/assem_s10.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASS10_KKII(N1    ,N2    ,K11   ,K22   ,K12   ,
     1                      KJJ   ,ND    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND,N1,N2      
      my_real
     .   K11(ND,ND) ,K22(ND,ND),K12(ND,ND),KJJ(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   KII(ND,ND)
C------------------------------
         DO I =1,ND
         DO J =I,ND
          KII(I,J) = FOURTH*KJJ(I,J)
         ENDDO 
         ENDDO 
         DO I =1,ND
         DO J =I+1,ND
          KII(J,I) = KII(I,J)
         ENDDO 
         ENDDO 
C
         DO I =1,ND
         DO J =I,ND
          K11(I,J) = K11(I,J)+ KII(I,J)
          K22(I,J) = K22(I,J)+ KII(I,J)
         ENDDO 
         ENDDO 
C   
         DO I =1,ND
         DO J =1,ND
          K12(I,J) = K12(I,J)+ KII(I,J)
         ENDDO 
         ENDDO 
C
      RETURN
      END
Chd|====================================================================
Chd|  ASS10_KKIJ                    source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|        ASSEM_S10                     source/implicit/assem_s10.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASS10_KKIJ(NI    ,NJ1   ,NJ2   ,ND    ,
     1                      K11   ,K22   ,KI1   ,KI2   ,KIJ0  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND,NI,NJ1,NJ2   
      my_real
     .   K11(ND,ND),K22(ND,ND),KI1(ND,ND),KI2(ND,ND),KIJ0(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NJ
      my_real
     .   KIJ(ND,ND)
C------------------------------
        DO I =1,ND
        DO J =1,ND
         KIJ(I,J) = HALF*KIJ0(I,J)
        ENDDO 
        ENDDO
C 
        NJ=NJ1
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           K11(I,J) = K11(I,J)+KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
        ELSEIF (NI>NJ) THEN
          DO I =1,ND
          DO J =1,ND
           KI1(I,J) = KI1(I,J)+KIJ(J,I)
          ENDDO 
          ENDDO 
        ELSE
          DO I =1,ND
          DO J =1,ND
           KI1(I,J) = KI1(I,J)+KIJ(I,J)
          ENDDO 
          ENDDO 
        END IF
        NJ=NJ2
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           K22(I,J) = K22(I,J)+KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
        ELSEIF (NI>NJ) THEN
          DO I =1,ND
          DO J =1,ND
           KI2(I,J) = KI2(I,J)+KIJ(J,I)
          ENDDO 
          ENDDO 
        ELSE
          DO I =1,ND
          DO J =1,ND
           KI2(I,J) = KI2(I,J)+KIJ(I,J)
          ENDDO 
          ENDDO 
        END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  ASS10_KKJI                    source/implicit/assem_s10.F   
Chd|-- called by -----------
Chd|        ASSEM_S10                     source/implicit/assem_s10.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASS10_KKJI(NI    ,NJ1   ,NJ2   ,ND    ,
     1                      K11   ,K22   ,KI1   ,KI2   ,KIJ0  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND,NI,NJ1,NJ2   
C     REAL
      my_real
     .   K11(ND,ND),K22(ND,ND),KI1(ND,ND),KI2(ND,ND),KIJ0(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NJ
      my_real
     .   KIJ(ND,ND)
C------------------------------
        DO I =1,ND
        DO J =1,ND
         KIJ(I,J) = HALF*KIJ0(J,I)
        ENDDO 
        ENDDO
C 
        NJ=NJ1
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           K11(I,J) = K11(I,J)+KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
        ELSEIF (NI>NJ) THEN
          DO I =1,ND
          DO J =1,ND
           KI1(I,J) = KI1(I,J)+KIJ(J,I)
          ENDDO 
          ENDDO 
        ELSE
          DO I =1,ND
          DO J =1,ND
           KI1(I,J) = KI1(I,J)+KIJ(I,J)
          ENDDO 
          ENDDO 
        END IF
        NJ=NJ2
        IF (NI==NJ) THEN
          DO I =1,ND
          DO J =I,ND
           K22(I,J) = K22(I,J)+KIJ(I,J)+ KIJ(J,I)
          ENDDO 
          ENDDO 
        ELSEIF (NI>NJ) THEN
          DO I =1,ND
          DO J =1,ND
           KI2(I,J) = KI2(I,J)+KIJ(J,I)
          ENDDO 
          ENDDO 
        ELSE
          DO I =1,ND
          DO J =1,ND
           KI2(I,J) = KI2(I,J)+KIJ(I,J)
          ENDDO 
          ENDDO 
        END IF
C
      RETURN
      END
