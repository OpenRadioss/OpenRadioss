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
Chd|  S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_S10                     source/implicit/assem_s10.F   
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|        MORTHLOCK3                    source/elements/solid/solide4/s4ke3.F
Chd|        S10COORK                      source/elements/solid/solide10/s10coork.F
Chd|        S10CUMG3                      source/elements/solid/solide10/s10cumg3.F
Chd|        S10DERI3                      source/elements/solid/solide10/s10deri3.F
Chd|        S10EOFF                       source/elements/solid/solide10/s10eoff.F
Chd|        S10KGEO3                      source/elements/solid/solide10/s10ke3.F
Chd|        SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S10KE3(PM    ,GEO   ,IXS   ,IXS10 ,X  ,ELBUF_STR,
     1                  ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  ,
     2                  IADK  ,JDIK  ,NEL   ,
     3                  IPM   ,IGEO  ,IKGEO ,BUFMAT)
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "scr06_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*), IKGEO
C
      INTEGER NEL ,IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .         ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)
C
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*), 
     . K11(9,MVSIZ),K12(9,MVSIZ),K13(9,MVSIZ),K14(9,MVSIZ),K15(9,MVSIZ),
     . K16(9,MVSIZ),K17(9,MVSIZ),K18(9,MVSIZ),K19(9,MVSIZ),K10(9,MVSIZ),
     . K22(9,MVSIZ),K23(9,MVSIZ),K24(9,MVSIZ),K25(9,MVSIZ),K26(9,MVSIZ),
     . K27(9,MVSIZ),K28(9,MVSIZ),K29(9,MVSIZ),K20(9,MVSIZ),K33(9,MVSIZ),
     . K34(9,MVSIZ),K35(9,MVSIZ),K36(9,MVSIZ),K37(9,MVSIZ),K38(9,MVSIZ),
     . K39(9,MVSIZ),K30(9,MVSIZ),K44(9,MVSIZ),K45(9,MVSIZ),K46(9,MVSIZ),
     . K47(9,MVSIZ),K48(9,MVSIZ),K49(9,MVSIZ),K40(9,MVSIZ),K55(9,MVSIZ),
     . K56(9,MVSIZ),K57(9,MVSIZ),K58(9,MVSIZ),K59(9,MVSIZ),K50(9,MVSIZ),
     . K66(9,MVSIZ),K67(9,MVSIZ),K68(9,MVSIZ),K69(9,MVSIZ),K60(9,MVSIZ),
     . K77(9,MVSIZ),K78(9,MVSIZ),K79(9,MVSIZ),K70(9,MVSIZ),K88(9,MVSIZ),
     . K89(9,MVSIZ),K80(9,MVSIZ),K99(9,MVSIZ),K90(9,MVSIZ),K00(9,MVSIZ),
     .   OFFG(MVSIZ)    ,BUFMAT(*),K_DIAG(*)   ,K_LT(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LCO, NF1, IFLAG, NB3S, IP, NF2, I,J
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),PID
      my_real
     . RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     . SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     . TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) ,
     .  E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) , 
     .  E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) , 
     .  E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) ,
     .  VOLN(MVSIZ), DELTAX(MVSIZ), DELTAX2(MVSIZ),VOLG(MVSIZ)
C----------------
      INTEGER NC(MVSIZ,10),IADBUF,IKORTH,IBID,IUN
      my_real
     .   OFF(MVSIZ) , VOLP(MVSIZ,5),
     .  XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),
     .  VX(MVSIZ,10),VY(MVSIZ,10),VZ(MVSIZ,10),
     .  PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5),
     .  NX(MVSIZ,10,5),
     .  WIP(5,5), ALPH(5,5), BETA(5,5),BID(MVSIZ)
      DATA WIP / 1.  ,0.  ,0.  ,0.  ,0.  ,
     2           0.  ,0.  ,0.  ,0.  ,0.  ,
     3           0.  ,0.  ,0.  ,0.  ,0.  ,
     4           0.25,0.25,0.25,0.25,0.  ,
     5           0.45,0.45,0.45,0.45,-0.8/
C                                                                     12
       my_real
     .  HH(2,MVSIZ),DD(9,MVSIZ),GG(MVSIZ),DM(9,MVSIZ),GM(9,MVSIZ),
     .      DGM(9,MVSIZ),DG(9,MVSIZ),G33(9,MVSIZ),GAMA(MVSIZ,6)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      DOUBLE PRECISION
     .   VOLDP(MVSIZ,5)
C----HH(1,):lamda,HH(2,):G
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
      DO IP=1,3
        DO J=1,5
          ALPH(J,IP)=ZERO
          BETA(J,IP)=ZERO
        END DO
      END DO
      ALPH(1,4)=ZEP5854102
      ALPH(2,4)=ZEP5854102
      ALPH(3,4)=ZEP5854102
      ALPH(4,4)=ZEP5854102
      ALPH(5,4)=ZERO
      ALPH(1,5)=HALF
      ALPH(2,5)=HALF
      ALPH(3,5)=HALF
      ALPH(4,5)=HALF
      ALPH(5,5)=FOURTH
      BETA(1,4)=ZEP1381966
      BETA(2,4)=ZEP1381966
      BETA(3,4)=ZEP1381966
      BETA(4,4)=ZEP1381966
      BETA(5,4)=ZERO
      BETA(1,5)=ONE_OVER_6
      BETA(2,5)=ONE_OVER_6
      BETA(3,5)=ONE_OVER_6
      BETA(4,5)=ONE_OVER_6
      BETA(5,5)=FOURTH
      IF (ISORTH>0) THEN
       IKORTH=1
      ELSE
       IKORTH=0
      ENDIF
C-----------
      NF1=NFT+1
      NF2=NF1-NUMELS8
C-----------
      CALL S10COORK(
     .   X,IXS(1,NF1),IXS10(1,NF2),
     .   XX, YY, ZZ,  
     .   GBUF%OFF,OFFG,GBUF%SMSTR,
     .   NC,NGL,MXT,NGEO,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K19,K10,
     .   K22,K23,K24,K25,K26,K27,K28,K29,K20,K33,
     .   K34,K35,K36,K37,K38,K39,K30,K44,K45,K46,
     .   K47,K48,K49,K40,K55,K56,K57,K58,K59,K50,
     .   K66,K67,K68,K69,K60,K77,K78,K79,K70,K88,
     .   K89,K80,K99,K90,K00,NEL)
C-----------
      CALL S10DERI3(OFFG,VOLP,NGL,DELTAX,DELTAX2,
     .   XX, YY, ZZ, PX,PY,PZ, NX,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX, TY, TZ,
     .   WIP(1,NPT),ALPH(1,NPT),BETA(1,NPT),VOLN,VOLG,VOLDP,
     .   NC ,GBUF%SMSTR,GBUF%OFF ,NEL)
        CALL SREPLOC3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  ) 
      IF (ISORTH == 0) THEN            
        DO I=LFT,LLT                                            
          GAMA(I,1) = ONE                               
          GAMA(I,2) = ZERO                                
          GAMA(I,3) = ZERO             
          GAMA(I,4) = ZERO                                
          GAMA(I,5) = ONE                                
          GAMA(I,6) = ZERO             
        ENDDO                          
      ELSE                             
        CALL SORTHDIR3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  , 
     .       GBUF%GAMA,GAMA )
        CALL MORTHLOCK3(LFT,LLT ,GAMA,E1X,E2X,E3X,
     +                  E1Y,E2Y ,E3Y ,E1Z,E2Z,E3Z)
      ENDIF
      IF (MTN>=28) THEN
       IADBUF = IPM(7,MXT(1))
      ELSE
       IADBUF = 0
      ENDIF
        CALL MMATS(LFT    ,LLT     ,PM   ,MXT    ,HH    ,
     .            MTN    ,IKORTH  ,IPM   ,IGEO   ,GAMA  ,
     .            BUFMAT(IADBUF)  ,DM    ,DGM    ,GM    ,
     .            JHBE  ,GBUF%SIG ,BID   ,NPT    ,NEL   )
        IBID = 0
C-----------------------------
C     POINTS D' INTEGRATION 
C-----------------------------
      IUN=1
      DO IP=1,NPT
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)
C
        CALL MMSTIFS(PM   ,MXT  ,HH    ,VOLP(1,IP),IBID  ,
     .               DD   ,GG   ,DG    ,G33   ,DM    ,
     .               GM   ,DGM  ,IKORTH,LBUF%SIG,IUN  ,
     .               IUN  ,IP   ,NEL   )
        CALL S10CUMG3(
     .   PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP), 
     .   DD  ,GG   ,DG   ,G33  ,IKORTH,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K19,K10,
     .   K22,K23,K24,K25,K26,K27,K28,K29,K20,K33,
     .   K34,K35,K36,K37,K38,K39,K30,K44,K45,K46,
     .   K47,K48,K49,K40,K55,K56,K57,K58,K59,K50,
     .   K66,K67,K68,K69,K60,K77,K78,K79,K70,K88,
     .   K89,K80,K99,K90,K00)
C----------------------------
C     geometrical stiffness 
C----------------------------
        IF (IKGEO>0) THEN
         CALL S10KGEO3(LBUF%SIG,VOLP(1,IP) ,
     .    PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP), 
     .    K11,K12,K13,K14,K15,K16,K17,K18,K19,K10,
     .    K22,K23,K24,K25,K26,K27,K28,K29,K20,K33,
     .    K34,K35,K36,K37,K38,K39,K30,K44,K45,K46,
     .    K47,K48,K49,K40,K55,K56,K57,K58,K59,K50,
     .    K66,K67,K68,K69,K60,K77,K78,K79,K70,K88,
     .    K89,K80,K99,K90,K00,NEL)
        ENDIF 
      ENDDO
      
         IF (NEIG>0) CALL S10EOFF(
     1      LFT, LLT, IXS(1,NF1),IXS10(1,NF2), ETAG, OFFG)
         CALL ASSEM_S10(
     1                   IXS(1,NF1),IXS10(1,NF2),NEL,IDDL  ,NDOF  ,
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
     D                   K80   ,K99   ,K90   ,K00   ,OFFG   )
C
      RETURN
      END
Chd|====================================================================
Chd|  S10KGEO3                      source/elements/solid/solide10/s10ke3.F
Chd|-- called by -----------
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|-- calls ---------------
Chd|        S4KEH3                        source/elements/solid/solide4/s4ke3.F
Chd|====================================================================
      SUBROUTINE S10KGEO3(SIG , VOL,
     .    PX, PY, PZ, 
     .    K11,K12,K13,K14,K15,K16,K17,K18,K19,K10,
     .    K22,K23,K24,K25,K26,K27,K28,K29,K20,K33,
     .    K34,K35,K36,K37,K38,K39,K30,K44,K45,K46,
     .    K47,K48,K49,K40,K55,K56,K57,K58,K59,K50,
     .    K66,K67,K68,K69,K60,K77,K78,K79,K70,K88,
     .    K89,K80,K99,K90,K00,NEL)
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
      INTEGER NEL
C     REAL
      my_real
     .  PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .  VOL(*),SIG(NEL,6)
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
      INTEGER I,J
C     REAL
      my_real
     .   FXX(MVSIZ),FYY(MVSIZ),FZZ(MVSIZ),FXY(MVSIZ),FXZ(MVSIZ),
     .   FYZ(MVSIZ)
C-----------------------------------------------
       DO I=LFT,LLT
        FXX(I)=SIG(I,1)*VOL(I)
        FYY(I)=SIG(I,2)*VOL(I)
        FZZ(I)=SIG(I,3)*VOL(I)
        FXY(I)=SIG(I,4)*VOL(I)
        FYZ(I)=SIG(I,5)*VOL(I)
        FXZ(I)=SIG(I,6)*VOL(I)
       ENDDO
C -----------------------------------------------
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,1),PY(1,1),PZ(1,1),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K11 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,2),PY(1,2),PZ(1,2),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K22 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,3),PY(1,3),PZ(1,3),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K33 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K44 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K55 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K66 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K77 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K88 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K99 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K00 )
C
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,2),PY(1,2),PZ(1,2),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K12 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,3),PY(1,3),PZ(1,3),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K13 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K14 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K15 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K16 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K17 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K18 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K19 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K10 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,3),PY(1,3),PZ(1,3),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K23 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K24 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K25 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K26 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K27 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K28 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K29 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K20 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K34 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K35 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K36 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K37 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K38 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K39 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K30 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K45 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K46 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K47 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K48 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K49 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K40 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K56 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K57 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K58 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K59 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K50 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K67 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K68 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K69 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K60 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K78 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K79 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K70 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K89 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K80 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K90 )
C
      RETURN
      END
