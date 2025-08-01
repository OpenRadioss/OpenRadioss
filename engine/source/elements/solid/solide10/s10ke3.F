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
!||    s10ke3         ../engine/source/elements/solid/solide10/s10ke3.F
!||--- called by ------------------------------------------------------
!||    imp_glob_k     ../engine/source/implicit/imp_glob_k.F
!||    imp_glob_k0    ../engine/source/implicit/imp_glob_k.F
!||--- calls      -----------------------------------------------------
!||    assem_s10      ../engine/source/implicit/assem_s10.F
!||    mmats          ../engine/source/elements/solid/solide8z/mmats.F
!||    mmstifs        ../engine/source/elements/solid/solide8z/mmstifs.F
!||    morthlock3     ../engine/source/elements/solid/solide4/morthlock3.F
!||    s10coork       ../engine/source/elements/solid/solide10/s10coork.F
!||    s10cumg3       ../engine/source/elements/solid/solide10/s10cumg3.F
!||    s10deri3       ../engine/source/elements/solid/solide10/s10deri3.F
!||    s10eoff        ../engine/source/elements/solid/solide10/s10eoff.F
!||    s10kgeo3       ../engine/source/elements/solid/solide10/s10kgeo3.F
!||    sorthdir3      ../engine/source/elements/solid/solide/sorthdir3.F
!||    sreploc3       ../engine/source/elements/solid/solide/sreploc3.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE S10KE3(
     1   PM,       GEO,      IXS,      IXS10,
     2   X,        ELBUF_STR,ETAG,     IDDL,
     3   NDOF,     K_DIAG,   K_LT,     IADK,
     4   JDIK,     NEL,      IPM,      IGEO,
     5   IKGEO,    BUFMAT,   NFT,      MTN,
     6   NPT,      ISMSTR,   JHBE,     IREP,
     7   ISORTH,   JLAG)
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JLAG
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: NPT
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: IREP
      INTEGER, INTENT(IN) :: ISORTH
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
     1   X,           IXS(1,NF1),  IXS10(1,NF2),XX,
     2   YY,          ZZ,          GBUF%OFF,    OFFG,
     3   GBUF%SMSTR,  NC,          NGL,         MXT,
     4   NGEO,        K11,         K12,         K13,
     5   K14,         K15,         K16,         K17,
     6   K18,         K19,         K10,         K22,
     7   K23,         K24,         K25,         K26,
     8   K27,         K28,         K29,         K20,
     9   K33,         K34,         K35,         K36,
     A   K37,         K38,         K39,         K30,
     B   K44,         K45,         K46,         K47,
     C   K48,         K49,         K40,         K55,
     D   K56,         K57,         K58,         K59,
     E   K50,         K66,         K67,         K68,
     F   K69,         K60,         K77,         K78,
     G   K79,         K70,         K88,         K89,
     H   K80,         K99,         K90,         K00,
     I   NEL,         ISMSTR)
C-----------
      CALL S10DERI3(
     1   OFFG,       VOLP,       NGL,        DELTAX,
     2   DELTAX2,    XX,         YY,         ZZ,
     3   PX,         PY,         PZ,         NX,
     4   RX,         RY,         RZ,         SX,
     5   SY,         SZ,         TX,         TY,
     6   TZ,         WIP(1,NPT), ALPH(1,NPT),BETA(1,NPT),
     7   VOLN,       VOLG,       VOLDP,      NC,
     8   GBUF%SMSTR, GBUF%OFF,   NEL,        NPT,
     9   ISMSTR,     JLAG)
        CALL SREPLOC3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      E1X,     E2X,     E3X,
     4   E1Y,     E2Y,     E3Y,     E1Z,
     5   E2Z,     E3Z,     NEL)
      IF (ISORTH == 0) THEN            
        DO I=1,NEL                                            
          GAMA(I,1) = ONE                               
          GAMA(I,2) = ZERO                                
          GAMA(I,3) = ZERO             
          GAMA(I,4) = ZERO                                
          GAMA(I,5) = ONE                                
          GAMA(I,6) = ZERO             
        ENDDO                          
      ELSE                             
        CALL SORTHDIR3(
     1   RX,       RY,       RZ,       SX,
     2   SY,       SZ,       TX,       TY,
     3   TZ,       E1X,      E2X,      E3X,
     4   E1Y,      E2Y,      E3Y,      E1Z,
     5   E2Z,      E3Z,      GBUF%GAMA,GAMA,
     6   NEL,      IREP)
        CALL MORTHLOCK3(1,NEL ,GAMA,E1X,E2X,E3X,
     +                  E1Y,E2Y ,E3Y ,E1Z,E2Z,E3Z)
      ENDIF
      IF (MTN>=28) THEN
       IADBUF = IPM(7,MXT(1))
      ELSE
       IADBUF = 1
      ENDIF
        CALL MMATS(1    ,NEL     ,PM   ,MXT    ,HH    ,
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
        CALL MMSTIFS(
     1   PM,        MXT,       HH,        VOLP(1,IP),
     2   IBID,      DD,        GG,        DG,
     3   G33,       DM,        GM,        DGM,
     4   IKORTH,    LBUF%SIG,  IUN,       IUN,
     5   IP,        NEL,       JHBE,      MTN)
        CALL S10CUMG3(
     1   PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),DD,
     2   GG,        DG,        G33,       IKORTH,
     3   K11,       K12,       K13,       K14,
     4   K15,       K16,       K17,       K18,
     5   K19,       K10,       K22,       K23,
     6   K24,       K25,       K26,       K27,
     7   K28,       K29,       K20,       K33,
     8   K34,       K35,       K36,       K37,
     9   K38,       K39,       K30,       K44,
     A   K45,       K46,       K47,       K48,
     B   K49,       K40,       K55,       K56,
     C   K57,       K58,       K59,       K50,
     D   K66,       K67,       K68,       K69,
     E   K60,       K77,       K78,       K79,
     F   K70,       K88,       K89,       K80,
     G   K99,       K90,       K00,       NEL)
C----------------------------
C     geometrical stiffness 
C----------------------------
        IF (IKGEO>0) THEN
         CALL S10KGEO3(
     1   LBUF%SIG,  VOLP(1,IP),PX(1,1,IP),PY(1,1,IP),
     2   PZ(1,1,IP),K11,       K12,       K13,
     3   K14,       K15,       K16,       K17,
     4   K18,       K19,       K10,       K22,
     5   K23,       K24,       K25,       K26,
     6   K27,       K28,       K29,       K20,
     7   K33,       K34,       K35,       K36,
     8   K37,       K38,       K39,       K30,
     9   K44,       K45,       K46,       K47,
     A   K48,       K49,       K40,       K55,
     B   K56,       K57,       K58,       K59,
     C   K50,       K66,       K67,       K68,
     D   K69,       K60,       K77,       K78,
     E   K79,       K70,       K88,       K89,
     F   K80,       K99,       K90,       K00,
     G   NEL)
        ENDIF 
      ENDDO
      
         IF (NEIG>0) CALL S10EOFF(
     1      1, NEL, IXS(1,NF1),IXS10(1,NF2), ETAG, OFFG)
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
