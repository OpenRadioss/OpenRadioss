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
Chd|  RINIT3                        source/elements/spring/rinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        R1BUF3                        source/elements/spring/r1buf3.F
Chd|        R23MASS                       source/elements/spring/rmass.F
Chd|        R2BUF3                        source/elements/spring/r2buf3.F
Chd|        R3BUF3                        source/elements/spring/r3buf3.F
Chd|        R4BUF3                        source/elements/spring/r4buf3.F
Chd|        R4INI                         source/elements/spring/rinit3.F
Chd|        R8INI                         source/elements/spring/rinit3.F
Chd|        RINI1U                        source/elements/spring/rinit3.F
Chd|        RINI2U                        source/elements/spring/rinit3.F
Chd|        RINI32                        source/properties/spring/hm_read_prop32.F
Chd|        RINI33                        source/elements/joint/rjoint/rini33.F
Chd|        RINI35                        source/elements/spring/rini35.F
Chd|        RINI36                        source/properties/spring/hm_read_prop36.F
Chd|        RINI3U                        source/elements/spring/rinit3.F
Chd|        RINI44                        source/elements/spring/rini44.F
Chd|        RINI45                        source/elements/joint/rjoint/rini45.F
Chd|        RINI46                        source/elements/spring/rini46.F
Chd|        RKINI3                        source/elements/spring/rkini3.F
Chd|        RMAS12                        source/elements/spring/rmas12.F
Chd|        RMASS                         source/elements/spring/rmass.F
Chd|        RUINI                         source/elements/spring/rinit3.F
Chd|        ST_USERLIB_RINIUSER           source/user_interface/dyn_userlib.c
Chd|        USER_OUTPUT                   source/user_interface/user_output.F
Chd|        STRR                          source/tools/univ/strr.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE RINIT3(ELBUF_STR,
     .                  IXR      ,X        ,GEO      ,XMAS   ,NPC   ,
     .                  PLD      ,XIN      ,SKEW     ,DTELEM ,NEL   ,
     .                  STIFN    ,STIFR    ,PARTSAV  ,V      ,IPART ,
     .                  ITAB     ,MSR      , 
     .                  INR      ,STIFINT  ,STR      ,IGEO   ,SIGRS ,
     .                  NSIGRS   ,IMERGE2  ,IADMERGE2,MSRT   ,IXR_KJ,
     .                  NOM_OPT  ,STRR     ,PTSPRI   ,IPM    ,PM   ,
     .                  UPARAM   ,R_SKEW)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE SEATBELT_MOD
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "random_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "userlib.inc"
#include      "scr15_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*), NPC(*),IPART(*),ITAB(*),NEL,
     .        IGEO(NPROPGI,*),NSIGRS,IMERGE2(NUMNOD+1),
     .        IADMERGE2(NUMNOD+1),IXR_KJ(5,*),PTSPRI(*),
     .        IPM(NPROPMI,*),R_SKEW(*)
      INTEGER NOM_OPT(LNOPT1,*)
C     REAL
      my_real
     .   X(3,*), GEO(NPROPG,*), XMAS(*), PLD(*), XIN(*),
     .   SKEW(LSKEW,*), DTELEM(*),STIFN(*),STIFR(*),PARTSAV(20,*), V(*),
     .   MSR(3,*), INR(3,*),
     .   STIFINT(*), STR(*),SIGRS(NSIGRS,*), MSRT(*),STRR(*),UPARAM(*),
     .   PM(NPROPM,*)
C
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,I2, IGTYP, NDEPAR, NB1, NB2, NB3, NB4, NB5,NB5A,NB5B,
     .   NB6, NB7, NB8, NB9, NB10, NB11, NB12, NB13, NB14, 
     .   NB15, NB15A, NB15B, NB15C, NB15D, NB15E, NB16, NBFI,
     .   NEL4,K,KK,KK1,ITMP,NEL5,
     .   I1, I0, I3,NEL3,NUVAR,NUPARAM,NFUNC,IADFUN,
     .   NMAT,IADMAT,NJPID,IADPID,ILENG,NFUND,
     3   UIX(4,MVSIZ),
     .   NB2A ,NB2B ,NB4A ,NB4B ,NB6A ,NB6B,NB7A,NB7B,NB8A ,NB8B,NB9A, 
     .    NB9B ,NB10A,NB10B,NB11A,NB11B,NB12A,NB12B,NB13A,NB13B,NB18,
     .    IMAT,K1,K11,K14,K12,K13,IADBUF,IMASS,SLIP,FRA
C     REAL
      my_real
     .   DT, DTC, XKM, XCM, XKR, XCR, XM, XINE,  EX, EY, EZ,
     .   AL2, STI,RHO,KX,KXY,KXZ,
     .   UL(MVSIZ),
     .   UINER(MVSIZ) ,USTIFM(MVSIZ) ,
     .   USTIFR(MVSIZ),UVISM(MVSIZ)  ,
     .   UVISR(MVSIZ), XL(MVSIZ), DX(MVSIZ,3),EMS(MVSIZ)
      my_real 
     .     LENGTH, RATIO, LMIN
      my_real 
     .     MINL, MAXL, RFAC, IXX, IYY, INE2
      INTEGER IDS, CNT1, CNT2, NSPRG, NSPRG4, NSPRG8, NSPRG12, 
     .     NSPRG13, NSPRG25, NSPRG26, NSPRGU, IUN,NSPRG23,NSPRG27
      DATA NSPRG /0/, NSPRG4 /0/, NSPRG8 /0/, NSPRG12 /0/, 
     .     NSPRG13 /0/, NSPRG25 /0/,NSPRG26/0/,NSPRGU /0/, 
     .     NSPRG23 /0/,NSPRG27/0/
      INTEGER MINIDL, MAXIDL,IPID
      my_real 
     .     NOISE,BIDON,MAS2
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      CHARACTER OPTION*50
C
      TYPE(G_BUFEL_),POINTER :: GBUF
      INTEGER II(6)
C=======================================================================
      BIDON = ZERO
C
      GBUF => ELBUF_STR%GBUF
C
      DO I=1,6
        II(I) = (I-1)*NEL + 1
      ENDDO
C
      IUN = 1
      NOISE = TWO*SQRT(THREE)*XALEA
C
      DO I=1,NUMGEO
        IGTYP=IGEO(11,I)
        ID=IGEO(1,I)
        CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,I),LTITR)
        IF (IGTYP == 4 .OR. IGTYP == 12 .OR. IGTYP == 27) THEN
          CALL RKINI3(IGEO(101,I),NPC,PLD,GEO(2,I),GEO(7,I),IGEO(1,I),
     .                GEO(10,I)  ,GEO(39,I)   ,ID,TITR,NOM_OPT)
        ELSEIF (IGTYP == 8 .OR. IGTYP == 13) THEN
          CALL RKINI3(IGEO(101,I),NPC,PLD,GEO(3,I) , GEO(7,I) , IGEO(1,I),
     .                GEO(41,I)  ,GEO(39,I)   ,ID,TITR,NOM_OPT)
          CALL RKINI3(IGEO(104,I),NPC,PLD,GEO(10,I), GEO(14,I), IGEO(1,I),
     .                GEO(45,I)  ,GEO(174,I)   ,ID,TITR,NOM_OPT)
          CALL RKINI3(IGEO(107,I),NPC,PLD,GEO(15,I), GEO(18,I), IGEO(1,I),
     .                GEO(49,I)  ,GEO(175,I)   ,ID,TITR,NOM_OPT)
          CALL RKINI3(IGEO(110,I),NPC,PLD,GEO(19,I), GEO(22,I), IGEO(1,I),
     .                GEO(53,I)  ,GEO(176,I)   ,ID,TITR,NOM_OPT)
          CALL RKINI3(IGEO(113,I),NPC,PLD,GEO(23,I), GEO(26,I), IGEO(1,I),
     .                GEO(57,I)  ,GEO(177,I)   ,ID,TITR,NOM_OPT)
          CALL RKINI3(IGEO(116,I),NPC,PLD,GEO(27,I), GEO(30,I), IGEO(1,I),
     .                GEO(61,I)  ,GEO(178,I)   ,ID,TITR,NOM_OPT)
        ELSEIF (IGTYP == 25) THEN
          CALL RKINI3(IGEO(102,I),NPC,PLD,GEO(3,I) , GEO(7,I) , IGEO(1,I),
     .                GEO(41,I)  ,GEO(39,I)   ,ID,TITR,NOM_OPT)
          CALL RKINI3(IGEO(106,I),NPC,PLD,GEO(10,I), GEO(14,I), IGEO(1,I),
     .                GEO(45,I)  ,GEO(174,I)   ,ID,TITR,NOM_OPT)
          CALL RKINI3(IGEO(110,I),NPC,PLD,GEO(19,I), GEO(22,I), IGEO(1,I),
     .                GEO(53,I)  ,GEO(176,I)   ,ID,TITR,NOM_OPT)
          CALL RKINI3(IGEO(114,I),NPC,PLD,GEO(23,I), GEO(26,I), IGEO(1,I),
     .                GEO(57,I)  ,GEO(177,I)   ,ID,TITR,NOM_OPT)
        ELSEIF (IGTYP == 26) THEN
          NFUNC = IGEO(20,I)
          NFUND = IGEO(21,I)
          IADFUN = 100
          DO J = 1,NFUNC
            CALL RKINI3(IGEO(IADFUN+J,I),NPC,PLD,GEO(2,I),ONE, IGEO(1,I),
     .                  ONE       ,ONE    ,ID,TITR,NOM_OPT)
          ENDDO
          IADFUN = NFUND+100
          DO J = 1,NFUND
            CALL RKINI3(IGEO(IADFUN+J,I),NPC,PLD,GEO(2,I),ONE, IGEO(1,I),
     .                  ONE       ,ONE    ,ID,TITR,NOM_OPT)
          ENDDO
        ELSEIF (IGTYP == 23) THEN
          GEO(4,I) = EP30 ! 
        ENDIF ! IF (IGTYP
      ENDDO ! DO I=1,NUMGEO
C
      CALL ANCMSG(MSGID=506,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT)
C-----------------
      IPID=IXR(1,NFT+1)
      ID=IGEO(1,IPID)
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
      DO I=LFT,LLT
        J=I+NFT
        I0=IXR(1,J)
        I1=IXR(2,J)
        I2=IXR(3,J)
        I3=IXR(4,J)
C-----------------
        IF (I1 == I2 .OR. I1 == I3 .OR. I2 == I3) THEN
          IF (I1 == I2 .OR. I1 == I3) ITMP = I1
          IF (I2 == I3) ITMP = I2
          IF (IMERGE2(ITMP) /= 0) THEN
            CALL ANCMSG(MSGID=682,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IXR(NIXR,J),
     .                  I2=ITAB(ITMP))
            WRITE (IOUT,1000) ITAB(ITMP)
            KK = 0
            DO K=1,IADMERGE2(ITMP+1) - IADMERGE2(ITMP)
              KK = KK + 1
              IF (KK  ==  10) THEN
                WRITE (IOUT,FMT=FMT_10I)
     .         (ITAB(IMERGE2(IADMERGE2(ITMP)+KK1)),KK1=0,KK-1)
                KK = 0
              ENDIF
            ENDDO
            IF (KK /= 0) THEN
              WRITE (IOUT,FMT=FMT_10I)
     .       (ITAB(IMERGE2(IADMERGE2(ITMP)+KK1)),KK1=0,KK-1)
            ENDIF
          ELSE
            CALL ANCMSG(MSGID=681,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IXR(NIXR,J) )
          ENDIF ! IF (IMERGE2(ITMP) /= 0)
        ENDIF ! IF (I1 == I2 .OR. I1 == I3 .OR. I2 == I3)
C-----------------
        IGTYP=IGEO(11,I0)
        IF (IGTYP /= 4  .AND. IGTYP /= 8  .AND.
     .      IGTYP /= 12 .AND. IGTYP /= 13 .AND. IGTYP /= 25 .AND.
     .      IGTYP /= 44 .AND. IGTYP /= 26 .AND. IGTYP <  29 .AND.
     .      IGTYP /= 46 .AND. IGTYP /= 23 .AND. IGTYP /= 27) THEN
           CALL ANCMSG(MSGID=243,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR)
        ENDIF
C       check compatibility of property type with spring elements.
        IF (IGTYP > 33  .AND. IGTYP /= 35 .AND. IGTYP /= 36 .AND.
     .      IGTYP /= 44 .AND. IGTYP /= 45 .AND. IGTYP /= 46) THEN
           CALL ANCMSG(MSGID=243,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR)
        ENDIF
      ENDDO
C-----
      I0=IXR(1,1+NFT)
      ID=IGEO(1,I0)
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,I0),LTITR)
      IGTYP =  IGEO(11,I0)
      IF (IGTYP == 12) THEN
        DO I=LFT,LLT
          IF (IXR(4,I+NFT) == 0) THEN
            IPID=IXR(1,I+NFT)
            CALL ANCMSG(MSGID=244,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IXR(NIXR,I+NFT))
          ENDIF
        ENDDO
      ENDIF
C
      IDS = 328
      CNT1 = 0
      CNT2 = 0
      NSPRG = 0
c      CALL ANCNTS(IDS, CNT2)
      DO I=LFT,LLT
        J=I+NFT
        I0=IXR(1,J)
        I1=IXR(2,J)
        I2=IXR(3,J)
        I3=IXR(4,J)
        IGTYP=IGEO(11,I0)
        ILENG=NINT(GEO(93,I0))
        IF (IGTYP == 4) THEN
           NSPRG4 = NSPRG4 + 1
        ELSE IF (IGTYP == 8) THEN
           NSPRG8 = NSPRG8 + 1
        ELSE IF (IGTYP == 12) THEN
           NSPRG12 = NSPRG12 + 1
        ELSE IF (IGTYP == 13) THEN
           NSPRG13 = NSPRG13 + 1
        ELSE IF (IGTYP == 23) THEN
           NSPRG23 = NSPRG23 + 1
           IMAT  = IXR(5,I+NFT)
           IADBUF = IPM(7,IMAT) - 1
           ILENG = NINT(UPARAM(IADBUF + 2))
           IMASS =  IGEO(4,I0)
           MTN   = IPM(2,IMAT)
           IF(MTN == 108) IMASS = 2
           IF(MTN == 114) THEN
             IMASS = 1
             LMIN = MAX(UPARAM(IADBUF + 119),UPARAM(IADBUF + 126))
           ENDIF
        ELSE IF (IGTYP == 25) THEN
           NSPRG25 = NSPRG25 + 1
        ELSE IF (IGTYP == 26) THEN
           NSPRG26 = NSPRG26 + 1
        ELSE IF (IGTYP == 27) THEN
           NSPRG27 = NSPRG27 + 1
        ELSE
           NSPRGU = NSPRGU + 1
        ENDIF
        IF (ILENG > 0) THEN
          XL(I) =  SQRT(
     .         (X(1,I1)-X(1,I2))*(X(1,I1)-X(1,I2))
     +      +  (X(2,I1)-X(2,I2))*(X(2,I1)-X(2,I2))
     +      +  (X(3,I1)-X(3,I2))*(X(3,I1)-X(3,I2)) )
          IF (IGTYP == 12) THEN
            XL(I) =  XL(I) + SQRT(
     .         (X(1,I3)-X(1,I2))*(X(1,I3)-X(1,I2))
     +      +  (X(2,I3)-X(2,I2))*(X(2,I3)-X(2,I2))
     +      +  (X(3,I3)-X(3,I2))*(X(3,I3)-X(3,I2)) )
          ENDIF
          IF (MTN == 114) XL(I) = MAX(XL(I),LMIN)
          IF (XL(I) <=  NOISE) THEN
             IPID = IXR(1,I)
             CALL ANCMSG(MSGID=328,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXR(NIXR,I))
          ENDIF
        ELSE
          XL(I)=ONE
        ENDIF
      ENDDO
C
c      CALL ANCNTG(IDS, CNT1, CNT2)
      NSPRG = NSPRG + CNT2
      MINL = ZERO
      MAXL = ZERO
      MINIDL = 0
      MAXIDL = 0
      DO I=LFT,LLT
        J=I+NFT
        I0=IXR(1,J)
        I1=IXR(2,J)
        I2=IXR(3,J)
        I3=IXR(4,J)
        IGTYP=IGEO(11,I0)
C
        LENGTH =  SQRT(
     .       (X(1,I1)-X(1,I2))*(X(1,I1)-X(1,I2))
     +       +  (X(2,I1)-X(2,I2))*(X(2,I1)-X(2,I2))
     +       +  (X(3,I1)-X(3,I2))*(X(3,I1)-X(3,I2)) )
        IF (IGTYP == 12) THEN
           LENGTH =  LENGTH + SQRT(
     .          (X(1,I3)-X(1,I2))*(X(1,I3)-X(1,I2))
     +       +  (X(2,I3)-X(2,I2))*(X(2,I3)-X(2,I2))
     +       +  (X(3,I3)-X(3,I2))*(X(3,I3)-X(3,I2)) )
        ENDIF
C
        IF (MINL <= 0 .OR. (LENGTH < MINL .AND. LENGTH > EM15)) THEN 
          MINIDL = IXR(NIXR,J)
          MINL = LENGTH
        ENDIF
C
        IF (LENGTH > MAXL) THEN
          MAXIDL = IXR(NIXR,J)
          MAXL = LENGTH
        ENDIF
C
        IF(IGTYP == 8 .OR. IGTYP==13 .OR. IGTYP==25) THEN
          ILENG=NINT(GEO(93,I0))
C
          IF (ILENG > 0) THEN
            XM=GEO(1,I0)*XL(I)
            XINE=GEO(9,I0)*XL(I)
          ELSE
            XM=GEO(1,I0)
            XINE=GEO(9,I0)
          ENDIF
C
C---- For prop type8 - skew per element is used if available
          IF ((IGTYP == 8).AND.( R_SKEW(I+NFT) > 0)) THEN
            GBUF%SKEW_ID(I) = R_SKEW(I+NFT)
          ELSEIF (IGTYP == 8) THEN
C---- For prop type8 - skew of property is used if no skew per element
            GBUF%SKEW_ID(I) = IGEO(2,I0)
            R_SKEW(I+NFT)   = IGEO(2,I0)
          ENDIF
C
          RATIO = XM * LENGTH * LENGTH
          IF ( (.NOT.((IGTYP == 8).AND.(LENGTH < EM15))) .AND.
     .        (XINE < RATIO/EP03  .OR. XINE > RATIO*EP03) ) THEN
            CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,I0),LTITR)
            CALL ANCMSG(MSGID=432,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=IGEO(1,I0),
     .                  C1=TITR,
     .                  R2=RATIO,
     .                  R1=XINE,
     .                  I2=IXR(NIXR,I+NFT),
     .                  PRMOD=MSG_CUMU)
          ENDIF
        ELSEIF(IGTYP  == 23) THEN
            IMAT  = IXR(5,I+NFT)
            IADBUF = IPM(7,IMAT) - 1
            ILENG = NINT(UPARAM(IADBUF + 2))
            RHO = PM(1,IMAT)
            IMASS = IGEO(4,I0)
            MTN   = IPM(2,IMAT)
            UINER(I) = ZERO
            IF(MTN == 108) IMASS = 2
C---- For mat law108 skew per element is used if available
            IF ((MTN == 108).AND.( R_SKEW(I+NFT) > 0)) THEN
              GBUF%SKEW_ID(I) = R_SKEW(I+NFT)
            ELSEIF (MTN == 108) THEN
C---- For mat law108 skew of property 23 is used if no skew per element
              GBUF%SKEW_ID(I) = IGEO(2,I0)
            ELSEIF (MTN == 114) THEN
C---- For mat law114 lmin is used for mass setting of element with null length
              IMASS = 1
              LMIN = MAX(UPARAM(IADBUF + 119),UPARAM(IADBUF + 126))
              RFAC = UPARAM(IADBUF + 124)
              IXX = UPARAM(IADBUF + 122)
              IYY = UPARAM(IADBUF + 123)
              LENGTH = MAX(LENGTH,LMIN)
              IF (UPARAM(IADBUF + 127) > ZERO) THEN
C-              1D material of 2D seatbelt - no need for inertia - mass and inertia given by shell
                RFAC = ZERO
              ENDIF
C-            inertia of element is automatically computed according to moment of inertia area if Young modulus > 0
              UINER(I) = MAX(EM20,RFAC*MAX((RHO*GEO(1,I0)*LENGTH*LENGTH*LENGTH)/TWELVE + RHO*IYY*LENGTH,RHO*IXX*LENGTH))     
            ENDIF
            IF(IMASS == 1) THEN
             GBUF%MASS(I) = GEO(1,I0)*LENGTH*RHO
C
             IF (LENGTH == ZERO .AND. RHO /= ZERO) THEN
               IPID = IXR(1,I)
               CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,I0),LTITR)
               CALL ANCMSG(MSGID=1664,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXR(NIXR,I))
             ENDIF
            ELSEIF(IMASS == 2) THEN
              GBUF%MASS(I) = GEO(1,I0)*RHO
            ENDIF
C

          XM = GBUF%MASS(I)
          XINE = GEO(2,I0)

C
          RATIO = XM * LENGTH * LENGTH
          IF( MTN == 113) THEN
            IF ( ((LENGTH < EM15)) .AND.
     .          (XINE < RATIO/EP03  .OR. XINE > RATIO*EP03) ) THEN
              CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,I0),LTITR)
              CALL ANCMSG(MSGID=432,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=IGEO(1,I0),
     .                    C1=TITR,
     .                    R2=RATIO,
     .                    R1=XINE,
     .                    I2=IXR(NIXR,I+NFT),
     .                    PRMOD=MSG_CUMU)
            ENDIF   
          ENDIF  
        ENDIF
      ENDDO ! DO I=LFT,LLT
C
      CALL ANCMSG(MSGID=432,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_2,
     .            PRMOD=MSG_PRINT)
C------------------------------------------
C     INITIALISATION DES RIGIDITES NODALES POUR INTERFACES
C------------------------------------------
      IF (I7STIFS /= 0) THEN
        IF (IGTYP == 4 .OR. IGTYP == 12 .OR. IGTYP == 27) THEN
          DO I=LFT,LLT
            J=I+NFT
            I0=IXR(1,J)
            I1=IXR(2,J)
            I2=IXR(3,J)
            I3=IXR(4,J)
            STI = GEO(2,I0)*GEO(10,I0)/MAX(EM30,XL(I))
            STR(I)=STI
          ENDDO
        ELSEIF (IGTYP == 8 .OR. IGTYP == 13) THEN
          DO I=LFT,LLT
            J=I+NFT
            I0=IXR(1,J)
            I1=IXR(2,J)
            I2=IXR(3,J)
            STI = MAX(GEO(3,I0)*GEO(41,I0),GEO(10,I0)*GEO(45,I0),GEO(15,I0)*GEO(49,I0))/MAX(EM30,XL(I))
            STR(I)=STI
          ENDDO
        ELSEIF (IGTYP == 23 ) THEN
          K11 = 64 ! 4 + 6*10
          DO I=LFT,LLT
            J=I+NFT
            I0=IXR(1,J)
            I1=IXR(2,J)
            I2=IXR(3,J)
            IMAT  = IXR(5,I+NFT)
            IADBUF = IPM(7,IMAT) - 1
            KX   = UPARAM(IADBUF + K11 + 1)
            KXY  = UPARAM(IADBUF + K11 + 2)
            KXZ  = UPARAM(IADBUF + K11 + 3)
            STI = MAX(KX,KXY,KXZ)/MAX(EM30,XL(I))
            STR(I)=STI
          ENDDO
        ELSEIF (IGTYP == 25) THEN
          DO I=LFT,LLT
            J=I+NFT
            I0=IXR(1,J)
            I1=IXR(2,J)
            I2=IXR(3,J) 
            STI = MAX(GEO(3,I0)*GEO(41,I0),GEO(10,I0)*GEO(45,I0))/MAX(EM30,XL(I))
            STR(I)=STI
          ENDDO
        ELSEIF (IGTYP == 26) THEN
          DO I=LFT,LLT
            J=I+NFT
            I0=IXR(1,J)
            I1=IXR(2,J)
            I2=IXR(3,J)
            I3=IXR(4,J)
            STI = GEO(2,I0)/MAX(EM30,XL(I))
            STR(I)=STI
          ENDDO    
        ELSE
          DO I=LFT,LLT
            J=I+NFT
            I0=IXR(1,J)
            I1=IXR(2,J)
            I2=IXR(3,J)
            STI = GEO(3,I0)
            STR(I)=STI
          ENDDO
        ENDIF ! IF (IGTYP
      ENDIF ! IF (I7STIF /= 0)
C------------------------------------------
      NDEPAR=NUMELS+NUMELC+NUMELT+NUMELP+NFT
C-------------------------------------------------------------------
C   SPRINGS --> all types
C-------------------------------------------------------------------
C=======================================================================
      IF (IGTYP == 4) THEN
C=======================================================================
        CALL RMASS(IXR         ,GEO  ,XMAS        ,XIN ,PARTSAV     ,
     2             X           ,V    ,IPART(NFT+1),XL  ,MSR(1,NFT+1),
     3             INR(1,NFT+1),MSRT ,EMS         )
        CALL R1BUF3(GBUF%OFF ,GEO  ,X  ,GBUF%LENGTH(II(1)) ,IXR ,GBUF%POSX,
     2              IGEO     )
C----
       IF (INISPRI /= 0) 
     .   CALL R4INI(SIGRS        ,IXR        ,NSIGRS           ,GBUF%EINT       ,GBUF%FOR   ,
     2              GBUF%TOTDEPL ,GBUF%FOREP ,GBUF%DEP_IN_TENS ,GBUF%DEP_IN_COMP,GBUF%LENGTH,
     3              BIDON        ,BIDON      ,IGTYP            ,PTSPRI   ,GBUF%DEFINI(II(1)),
     4              GBUF%FORINI(II(1)))
      ELSEIF (IGTYP == 26) THEN
        CALL RMASS(IXR         ,GEO  ,XMAS        ,XIN ,PARTSAV     ,
     2             X           ,V    ,IPART(NFT+1),XL  ,MSR(1,NFT+1),
     3             INR(1,NFT+1),MSRT ,EMS         )
        CALL R1BUF3(GBUF%OFF ,GEO  ,X  ,GBUF%LENGTH(II(1)) ,IXR ,GBUF%POSX,
     2              IGEO     )
C----
        IF (INISPRI /= 0) 
     .    CALL R4INI(SIGRS        ,IXR        ,NSIGRS          ,GBUF%EINT       ,GBUF%FOR   ,
     2               GBUF%TOTDEPL ,GBUF%FOREP ,BIDON           ,BIDON           ,GBUF%LENGTH,
     3               BIDON        ,GBUF%DV    ,IGTYP           ,PTSPRI          ,GBUF%DEFINI,
     4               GBUF%FORINI  )
c------     
C=======================================================================
      ELSEIF (IGTYP == 8) THEN
C=======================================================================
        CALL RMASS(IXR         ,GEO ,XMAS        ,XIN,PARTSAV     ,
     2             X           ,V   ,IPART(NFT+1),XL ,MSR(1,NFT+1),
     3             INR(1,NFT+1),MSRT,EMS         )
        CALL R2BUF3(GBUF%OFF ,GEO      ,X        ,GBUF%LENGTH(II(1)) ,GBUF%LENGTH(II(2)),
     2              GBUF%LENGTH(II(3)) ,IXR      ,SKEW     ,GBUF%POSX,GBUF%POSY,
     3              GBUF%POSZ       ,GBUF%POSXX,GBUF%POSYY,GBUF%POSZZ,IGEO     ,
     3              GBUF%SKEW_ID)
C----
       IF (INISPRI /= 0) 
     .   CALL R8INI(
     2 IGTYP                  ,NEL                    ,SIGRS                  ,IXR                    ,NSIGRS                 ,
     3 GBUF%FOR(II(1))        ,GBUF%FOR(II(2))        ,GBUF%FOR(II(3))        ,GBUF%MOM(II(1))        ,GBUF%MOM(II(2))        ,
     4 GBUF%MOM(II(3))        ,GBUF%FOREP(II(1))      ,GBUF%FOREP(II(2))      ,GBUF%FOREP(II(3))      ,GBUF%MOMEP(II(1))      ,
     5 GBUF%MOMEP(II(2))      ,GBUF%MOMEP(II(3))      ,GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),
     6 GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),GBUF%ROT_IN_TENS(II(3)),GBUF%DEP_IN_COMP(II(1)),GBUF%DEP_IN_COMP(II(2)),
     7 GBUF%DEP_IN_COMP(II(3)),GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),GBUF%TOTDEPL(II(1))    ,
     8 GBUF%TOTDEPL(II(2))    ,GBUF%TOTDEPL(II(3))    ,GBUF%TOTROT(II(1))     ,GBUF%TOTROT(II(2))     ,GBUF%TOTROT(II(3))     ,
     9 GBUF%LENGTH(II(1))     ,GBUF%LENGTH(II(2))     ,GBUF%LENGTH(II(3))     ,GBUF%EINT              ,GBUF%E6                ,
     A PTSPRI                 ,GBUF%DEFINI(II(1))     ,GBUF%DEFINI(II(2))     ,GBUF%DEFINI(II(3))     ,GBUF%DEFINI(II(4))     ,
     B GBUF%DEFINI(II(5))     ,GBUF%DEFINI(II(6))     ,GBUF%FORINI(II(1))     ,GBUF%FORINI(II(2))     ,GBUF%FORINI(II(3))     ,
     B GBUF%FORINI(II(4))     ,GBUF%FORINI(II(5))     ,GBUF%FORINI(II(6))     )
C=======================================================================
      ELSEIF (IGTYP == 12) THEN
C=======================================================================
         CALL RMAS12 (IXR         ,GEO,XMAS        ,XIN,PARTSAV     ,
     2                X           ,V  ,IPART(NFT+1),XL ,MSR(1,NFT+1),
     3                INR(1,NFT+1),MSRT)
        IDS = 457
        CNT1 = 0
        CNT2 = 0
        CALL R3BUF3(GBUF%OFF,GEO  ,X ,GBUF%LENGTH(II(1)) ,IXR ,GBUF%POSX,
     .              IGEO    ,ITAB )
        NSPRG = NSPRG + CNT2
        IF (INISPRI /= 0) 
     .    CALL R4INI(SIGRS        ,IXR        ,NSIGRS          ,GBUF%EINT       ,GBUF%FOR   ,
     2               GBUF%TOTDEPL ,GBUF%FOREP ,GBUF%DEP_IN_TENS,GBUF%DEP_IN_TENS,GBUF%LENGTH,
     3               GBUF%DFS     ,BIDON      ,IGTYP           ,PTSPRI          ,GBUF%DEFINI,
     4               GBUF%FORINI  )
C=======================================================================
      ELSEIF (IGTYP == 13) THEN
C=======================================================================
        CALL RMASS(IXR         ,GEO ,XMAS        ,XIN,PARTSAV     ,
     2             X           ,V   ,IPART(NFT+1),XL ,MSR(1,NFT+1),
     3             INR(1,NFT+1),MSRT,EMS         )
     
        IDS = 325
        CNT1 = 0
        CNT2 = 0
        CALL R4BUF3(
     1       GBUF%OFF          ,GEO       ,X         ,GBUF%LENGTH(II(1)),GBUF%LENGTH(II(2)),
     2       GBUF%LENGTH(II(3)),IXR       ,SKEW      ,GBUF%SKEW         ,GBUF%POSX, 
     3       GBUF%POSY         ,GBUF%POSZ ,GBUF%POSXX,GBUF%POSYY        ,GBUF%POSZZ, 
     4       ITAB              ,GBUF%E6   ,IGEO      ,IPM)
        NSPRG = NSPRG + CNT2
        IF (INISPRI /= 0) 
     .    CALL R8INI(
     2 IGTYP                  ,NEL                    ,SIGRS                  ,IXR                    ,NSIGRS                 ,
     3 GBUF%FOR(II(1))        ,GBUF%FOR(II(2))        ,GBUF%FOR(II(3))        ,GBUF%MOM(II(1))        ,GBUF%MOM(II(2))        ,
     4 GBUF%MOM(II(3))        ,GBUF%FOREP(II(1))      ,GBUF%FOREP(II(2))      ,GBUF%FOREP(II(3))      ,GBUF%MOMEP(II(1))      ,
     5 GBUF%MOMEP(II(2))      ,GBUF%MOMEP(II(3))      ,GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),
     6 GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),GBUF%ROT_IN_TENS(II(3)),GBUF%DEP_IN_COMP(II(1)),GBUF%DEP_IN_COMP(II(2)),
     7 GBUF%DEP_IN_COMP(II(3)),GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),GBUF%TOTDEPL(II(1))    ,
     8 GBUF%TOTDEPL(II(2))    ,GBUF%TOTDEPL(II(3))    ,GBUF%TOTROT(II(1))     ,GBUF%TOTROT(II(2))     ,GBUF%TOTROT(II(3))     ,
     9 GBUF%LENGTH(II(1))     ,GBUF%LENGTH(II(2))     ,GBUF%LENGTH(II(3))     ,GBUF%EINT              ,GBUF%E6                ,
     A PTSPRI                 ,GBUF%DEFINI(II(1))     ,GBUF%DEFINI(II(2))     ,GBUF%DEFINI(II(3))     ,GBUF%DEFINI(II(4))     ,
     B GBUF%DEFINI(II(5))     ,GBUF%DEFINI(II(6))     ,GBUF%FORINI(II(1))     ,GBUF%FORINI(II(2))     ,GBUF%FORINI(II(3))     ,
     B GBUF%FORINI(II(4))     ,GBUF%FORINI(II(5))     ,GBUF%FORINI(II(6))     )
C=======================================================================
      ELSEIF (IGTYP == 23) THEN
C=======================================================================
        IDS = 325
        CNT1 = 0
        CNT2 = 0
        CALL R23MASS(IXR         ,GEO ,XMAS        ,XIN,PARTSAV     ,
     2               X           ,V   ,IPART(NFT+1),XL ,MSR(1,NFT+1),
     3               INR(1,NFT+1),MSRT,EMS      ,GBUF%MASS   ,UINER,MTN)
       IF(MTN == 108) THEN
C     
           CALL R2BUF3(GBUF%OFF ,GEO      ,X        ,GBUF%LENGTH(II(1)) ,GBUF%LENGTH(II(2)),
     2                 GBUF%LENGTH(II(3)) ,IXR      ,SKEW     ,GBUF%POSX,GBUF%POSY,
     3                 GBUF%POSZ       ,GBUF%POSXX,GBUF%POSYY,GBUF%POSZZ,IGEO     ,
     4                 GBUF%SKEW_ID)
C----
       ELSEIF (MTN==113) THEN  
          CALL R4BUF3(
     1         GBUF%OFF          ,GEO       ,X         ,GBUF%LENGTH(II(1)),GBUF%LENGTH(II(2)),
     2         GBUF%LENGTH(II(3)),IXR       ,SKEW      ,GBUF%SKEW         ,GBUF%POSX, 
     3         GBUF%POSY         ,GBUF%POSZ ,GBUF%POSXX,GBUF%POSYY        ,GBUF%POSZZ, 
     4         ITAB              ,GBUF%E6   ,IGEO      ,IPM)
           NSPRG = NSPRG + CNT2
C----
       ELSEIF(MTN == 114) THEN
          CALL R4BUF3(
     1         GBUF%OFF          ,GEO       ,X         ,GBUF%LENGTH(II(1)),GBUF%LENGTH(II(2)),
     2         GBUF%LENGTH(II(3)),IXR       ,SKEW      ,GBUF%SKEW         ,GBUF%POSX, 
     3         GBUF%POSY         ,GBUF%POSZ ,GBUF%POSXX,GBUF%POSYY        ,GBUF%POSZZ, 
     4         ITAB              ,GBUF%E6   ,IGEO      ,IPM)
           NSPRG = NSPRG + CNT2
       ENDIF ! MTN

       IF (INISPRI /= 0) 
     .      CALL R8INI(
     2    IGTYP                  ,NEL                    ,SIGRS                  ,IXR                    ,NSIGRS                 ,
     3    GBUF%FOR(II(1))        ,GBUF%FOR(II(2))        ,GBUF%FOR(II(3))        ,GBUF%MOM(II(1))        ,GBUF%MOM(II(2))        ,
     4    GBUF%MOM(II(3))        ,GBUF%FOREP(II(1))      ,GBUF%FOREP(II(2))      ,GBUF%FOREP(II(3))      ,GBUF%MOMEP(II(1))      ,
     5    GBUF%MOMEP(II(2))      ,GBUF%MOMEP(II(3))      ,GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),
     6    GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),GBUF%ROT_IN_TENS(II(3)),GBUF%DEP_IN_COMP(II(1)),GBUF%DEP_IN_COMP(II(2)),
     7    GBUF%DEP_IN_COMP(II(3)),GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),GBUF%TOTDEPL(II(1))    ,
     8    GBUF%TOTDEPL(II(2))    ,GBUF%TOTDEPL(II(3))    ,GBUF%TOTROT(II(1))     ,GBUF%TOTROT(II(2))     ,GBUF%TOTROT(II(3))     ,
     9    GBUF%LENGTH(II(1))     ,GBUF%LENGTH(II(2))     ,GBUF%LENGTH(II(3))     ,GBUF%EINT              ,GBUF%E6                ,
     A    PTSPRI                 ,GBUF%DEFINI(II(1))     ,GBUF%DEFINI(II(2))     ,GBUF%DEFINI(II(3))     ,GBUF%DEFINI(II(4))     ,
     B    GBUF%DEFINI(II(5))     ,GBUF%DEFINI(II(6))     ,GBUF%FORINI(II(1))     ,GBUF%FORINI(II(2))     ,GBUF%FORINI(II(3))     ,
     B    GBUF%FORINI(II(4))     ,GBUF%FORINI(II(5))     ,GBUF%FORINI(II(6))     )
C=======================================================================
      ELSEIF (IGTYP == 25) THEN
C=======================================================================
        CALL RMASS(IXR         ,GEO ,XMAS        ,XIN,PARTSAV     ,
     2             X           ,V   ,IPART(NFT+1),XL ,MSR(1,NFT+1),
     3             INR(1,NFT+1),MSRT,EMS         )
        IDS = 325
        CNT1 = 0
        CNT2 = 0
        CALL R4BUF3(GBUF%OFF          ,GEO      ,X         ,GBUF%LENGTH(II(1)),GBUF%LENGTH(II(2)) ,
     2              GBUF%LENGTH(II(3)),IXR      ,SKEW      ,GBUF%SKEW         ,GBUF%POSX, 
     3              GBUF%POSY         ,GBUF%POSZ,GBUF%POSXX,GBUF%POSYY        ,GBUF%POSZZ, 
     4              ITAB              ,GBUF%E6  ,IGEO      ,IPM)
        NSPRG = NSPRG + CNT2
        IF (INISPRI /= 0) 
     .    CALL R8INI(
     2 IGTYP                  ,NEL                    ,SIGRS                  ,IXR                    ,NSIGRS                 ,
     3 GBUF%FOR(II(1))        ,GBUF%FOR(II(2))        ,GBUF%FOR(II(3))        ,GBUF%MOM(II(1))        ,GBUF%MOM(II(2))        ,
     4 GBUF%MOM(II(3))        ,GBUF%FOREP(II(1))      ,GBUF%FOREP(II(2))      ,GBUF%FOREP(II(3))      ,GBUF%MOMEP(II(1))      ,
     5 GBUF%MOMEP(II(2))      ,GBUF%MOMEP(II(3))      ,GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),
     6 GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),GBUF%ROT_IN_TENS(II(3)),GBUF%DEP_IN_COMP(II(1)),GBUF%DEP_IN_COMP(II(2)),
     7 GBUF%DEP_IN_COMP(II(3)),GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),GBUF%TOTDEPL(II(1))    ,
     8 GBUF%TOTDEPL(II(2))    ,GBUF%TOTDEPL(II(3))    ,GBUF%TOTROT(II(1))     ,GBUF%TOTROT(II(2))     ,GBUF%TOTROT(II(3))     ,
     9 GBUF%LENGTH(II(1))     ,GBUF%LENGTH(II(2))     ,GBUF%LENGTH(II(3))     ,GBUF%EINT              ,GBUF%E6                ,
     A PTSPRI                 ,GBUF%DEFINI(II(1))     ,GBUF%DEFINI(II(2))     ,GBUF%DEFINI(II(3))     ,GBUF%DEFINI(II(4))     ,
     B GBUF%DEFINI(II(5))     ,GBUF%DEFINI(II(6))     ,GBUF%FORINI(II(1))     ,GBUF%FORINI(II(2))     ,GBUF%FORINI(II(3))     ,
     B GBUF%FORINI(II(4))     ,GBUF%FORINI(II(5))     ,GBUF%FORINI(II(6))     )
C=======================================================================
      ELSEIF (IGTYP == 27) THEN
C=======================================================================
        CALL RMASS(IXR         ,GEO  ,XMAS        ,XIN ,PARTSAV     ,
     2             X           ,V    ,IPART(NFT+1),XL  ,MSR(1,NFT+1),
     3             INR(1,NFT+1),MSRT ,EMS         )
        CALL R1BUF3(GBUF%OFF ,GEO  ,X  ,GBUF%LENGTH(II(1)) ,IXR ,GBUF%POSX,
     2              IGEO     )
C----
       IF (INISPRI /= 0) 
     .   CALL R4INI(SIGRS        ,IXR        ,NSIGRS           ,GBUF%EINT       ,GBUF%FOR   ,
     2              GBUF%TOTDEPL ,GBUF%FOREP ,GBUF%DEP_IN_TENS ,GBUF%DEP_IN_COMP,GBUF%LENGTH,
     3              BIDON        ,BIDON      ,IGTYP            ,PTSPRI   ,GBUF%DEFINI(II(1)),
     4              GBUF%FORINI(II(1)))
C=======================================================================
      ELSEIF (IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 45) THEN
C=======================================================================
        CALL RINI1U(GBUF%OFF ,GEO      ,X        ,UL    ,IXR ,
     2              SKEW     ,GBUF%SKEW,ITAB     ,UIX   ,IGEO)
        NUVAR =  NINT(GEO(25,I0))
        NUPARAM =  NINT(GEO(26,I0))
        IF (IGTYP == 32) THEN
          CALL RINI32(
     1                NEL      ,IOUT  ,I0       ,
     2                UIX      ,UL    ,GBUF%MASS,UINER   ,USTIFM ,
     3                USTIFR   ,UVISM ,UVISR    ,GBUF%VAR,NUVAR  ,ID,TITR,
     4                GBUF%EINT,NPC   ,PLD      )
        ELSEIF (IGTYP == 33) THEN
           DO I=LFT,LLT
             J=I+NFT
             I1=IXR(2,J)
             I2=IXR(3,J)
             DX(I,1) = (X(1,I2)-X(1,I1))
             DX(I,2) = (X(2,I2)-X(2,I1))
             DX(I,3) = (X(3,I2)-X(3,I1))
           ENDDO
           CALL RINI33(NEL       ,IOUT   ,I0      ,UIX,DX,
     1                 GBUF%MASS ,UINER  ,USTIFM  ,USTIFR,
     2                 UVISM     ,UVISR  ,GBUF%VAR,NUVAR )
        ELSEIF (IGTYP == 45) THEN
           DO I=LFT,LLT
             J=I+NFT
             I1=IXR(2,J)
             I2=IXR(3,J)
             DX(I,1) = (X(1,I2)-X(1,I1))
             DX(I,2) = (X(2,I2)-X(2,I1))
             DX(I,3) = (X(3,I2)-X(3,I1))
           ENDDO
           CALL RINI45(NEL      ,IOUT    ,I0     ,UIX    ,X     ,DX,
     .                 GBUF%MASS,UINER   ,USTIFM ,USTIFR ,UVISM ,
     .                 UVISR    ,GBUF%VAR,NUVAR  ,IXR    ,IXR_KJ,ID ,TITR)
        ENDIF
C
        DO I=LFT,LLT
          J=I+NFT
          I0=IXR(1,J)
          I1=IXR(2,J)
          I2=IXR(3,J)
          I3=IXR(4,J)
          XM = GBUF%MASS(I)
          XINE = UINER(I)
          AL2= UL(I)*UL(I)
          XKR= USTIFR(I)
          XKM= USTIFM(I)
          XCR= UVISR(I)
          XCM= UVISM(I)
          STIFN(I1)=STIFN(I1)+XKM
          STIFN(I2)=STIFN(I2)+XKM
          STIFR(I1)=STIFR(I1)+XKR
          STIFR(I2)=STIFR(I2)+XKR
          STRR(J)=XKR
          IF (XCM+XKM<EM15) XM =ONE
          IF (XCR+XKR<EM15) XINE=ONE
          XKM= MAX(EM15,XKM)
          XKR= MAX(EM15,XKR)
          DT=(SQRT(XCM*XCM+XM*XKM)-XCM)/XKM
          DTC=HALF*XM / MAX(EM15,XCM)
          DT = MIN(DT,DTC)
          DTC=(SQRT(XCR*XCR+XINE*XKR)-XCR)/XKR
          DT = MIN(DT,DTC)
          DTC=HALF*XINE / MAX( EM15,XCR)
          DT = MIN(DT,DTC)
          DTELEM(NDEPAR+I)= DT
        ENDDO
C
        CALL RINI2U(
     1    IXR         ,GBUF%MASS,UINER,XMAS        ,XIN         ,
     2    PARTSAV     ,X        ,V    ,IPART(NFT+1),MSR(1,NFT+1),
     3    INR(1,NFT+1),MSRT     ,EMS  )
        IF (INISPRI /= 0) 
     .    CALL RUINI(
     2 SIGRS                ,IXR                 ,NSIGRS              ,NUVAR                ,GBUF%FOR(II(1))      ,
     3 GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,
     4 GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     5 GBUF%VR_REPCVT(II(3)),GBUF%VAR            ,GBUF%EINT           ,BIDON                ,BIDON                ,
     6 IGTYP                ,PTSPRI)
C=======================================================================
      ELSEIF (IGTYP == 35 .OR. IGTYP == 36) THEN
C=======================================================================
        CALL RINI3U(GBUF%OFF ,GEO       ,X      ,UL     ,IXR ,  
     2              SKEW     ,GBUF%SKEW ,ITAB   ,UIX    ,IGEO)
        NUVAR =  NINT(GEO(25,I0))
        NUPARAM =  NINT(GEO(26,I0))
C---
        IF (IGTYP == 35) THEN
           CALL RINI35(
     1       NEL    ,IOUT  ,I0       ,
     2       UIX    ,UL    ,GBUF%MASS,UINER   ,USTIFM ,
     3       USTIFR ,UVISM ,UVISR    ,GBUF%VAR,NUVAR  )
        ELSEIF (IGTYP == 36) THEN
           CALL RINI36(
     1       NEL    ,IOUT  ,I0       ,
     2       UIX    ,UL    ,GBUF%MASS,UINER   ,USTIFM ,
     3       USTIFR ,UVISM ,UVISR    ,GBUF%VAR,NUVAR  )
        ENDIF
C---
        DO I=LFT,LLT
          J=I+NFT
          I0=IXR(1,J)
          I1=IXR(2,J)
          I2=IXR(3,J)
          I3=IXR(4,J)
          XM = GBUF%MASS(I)
          XINE = UINER(I)
          AL2= UL(I)*UL(I)
          XKR= USTIFR(I)
          XKM= USTIFM(I)
          XCR= UVISR(I)
          XCM= UVISM(I)
          STIFN(I1)=STIFN(I1)+XKM
          STIFN(I2)=STIFN(I2)+XKM
          STIFR(I1)=STIFR(I1)+XKR
          STIFR(I2)=STIFR(I2)+XKR
          STRR(J)=XKR
          IF (XCM+XKM<EM15) XM =ONE
          IF (XCR+XKR<EM15) XINE=ONE
          XKM= MAX(EM15,XKM)
          XKR= MAX(EM15,XKR)
          DT=(SQRT(XCM*XCM+XM*XKM)-XCM)/XKM
          DTC=HALF*XM / MAX(EM15,XCM)
          DT = MIN(DT,DTC)
          DTC=(SQRT(XCR*XCR+XINE*XKR)-XCR)/XKR
          DT = MIN(DT,DTC)
          DTC=HALF*XINE / MAX( EM15,XCR)
          DT = MIN(DT,DTC)
          DTELEM(NDEPAR+I)= DT
        ENDDO
        CALL RINI2U(
     1    IXR         ,GBUF%MASS,UINER,XMAS        ,XIN         ,
     2    PARTSAV     ,X        ,V    ,IPART(NFT+1),MSR(1,NFT+1),
     3    INR(1,NFT+1),MSRT     ,EMS  )
        IF (INISPRI /= 0) 
     +    CALL RUINI(
     2  SIGRS                ,IXR                 ,NSIGRS              ,NUVAR                ,GBUF%FOR(II(1))      ,
     3  GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,
     4  GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     5  GBUF%VR_REPCVT(II(3)),GBUF%VAR            ,GBUF%EINT           ,BIDON                ,BIDON                ,
     6  IGTYP                ,PTSPRI)
C
C=======================================================================
      ELSEIF (IGTYP > 28 .AND. IGTYP < 43) THEN  ! reserved for user properties
C=======================================================================
        CALL RINI3U(GBUF%OFF ,GEO       ,X      ,UL     ,IXR ,  
     2              SKEW     ,GBUF%SKEW ,ITAB   ,UIX    ,IGEO)
        NUVAR =  NINT(GEO(25,I0))
        NUPARAM =  NINT(GEO(26,I0))
C
        IF (IGTYP == 29) THEN   
          IF (USERL_AVAIL == 1) THEN
            CALL ST_USERLIB_RINIUSER(IGTYP,ROOTNAM,ROOTLEN,
     1        NEL      ,I0     ,
     2        UIX    ,UL    ,GBUF%MASS  ,UINER  ,USTIFM ,
     3        USTIFR ,UVISM ,UVISR  ,GBUF%VAR,NUVAR  )
            CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,0)
          ELSE
         OPTION='/PROP/USER29'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
          ENDIF
        ELSEIF (IGTYP == 30) THEN
          IF (USERL_AVAI L == 1) THEN
            CALL ST_USERLIB_RINIUSER(IGTYP,ROOTNAM,ROOTLEN,
     1        NEL      ,I0     ,
     2        UIX    ,UL    ,GBUF%MASS  ,UINER  ,USTIFM ,
     3        USTIFR ,UVISM ,UVISR  ,GBUF%VAR,NUVAR  )
            CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,0)
          ELSE
         OPTION='/PROP/USER30'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
          ENDIF
        ELSEIF (IGTYP == 31) THEN
          IF (USERL_AVAIL == 1) THEN
            CALL ST_USERLIB_RINIUSER(IGTYP,ROOTNAM,ROOTLEN,
     1        NEL      ,I0     ,
     2        UIX    ,UL    ,GBUF%MASS  ,UINER  ,USTIFM ,
     3        USTIFR ,UVISM ,UVISR  ,GBUF%VAR,NUVAR  )
            CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,0)
          ELSE
         OPTION='/PROP/USER31'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
          ENDIF
        ELSEIF (IGTYP == 37) THEN
          IF (USERL_AVAIL == 1) THEN
            CALL ST_USERLIB_RINIUSER(IGTYP,ROOTNAM,ROOTLEN,
     1        NEL      ,I0     ,
     2        UIX    ,UL    ,GBUF%MASS  ,UINER  ,USTIFM ,
     3        USTIFR ,UVISM ,UVISR  ,GBUF%VAR,NUVAR  )
            CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,0)
          ELSE
         OPTION='/PROP/USER37'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
          ENDIF
        ELSEIF (IGTYP == 38) THEN
          IF (USERL_AVAIL == 1) THEN
            CALL ST_USERLIB_RINIUSER(IGTYP,ROOTNAM,ROOTLEN,
     1        NEL      ,I0     ,
     2        UIX    ,UL    ,GBUF%MASS  ,UINER  ,USTIFM ,
     3        USTIFR ,UVISM ,UVISR  ,GBUF%VAR,NUVAR  )
            CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,0)
          ELSE
         OPTION='/PROP/USER38'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
          ENDIF
        ELSEIF (IGTYP == 39) THEN
          IF (USERL_AVAIL == 1) THEN
            CALL ST_USERLIB_RINIUSER(IGTYP,ROOTNAM,ROOTLEN,
     1        NEL      ,I0     ,
     2        UIX    ,UL    ,GBUF%MASS  ,UINER  ,USTIFM ,
     3        USTIFR ,UVISM ,UVISR  ,GBUF%VAR,NUVAR  )
            CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,0)
          ELSE
         OPTION='/PROP/USER39'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
          ENDIF
        ELSEIF (IGTYP == 40) THEN
          IF (USERL_AVAIL == 1) THEN
            CALL ST_USERLIB_RINIUSER(IGTYP,ROOTNAM,ROOTLEN,
     1        NEL      ,I0     ,
     2        UIX    ,UL    ,GBUF%MASS  ,UINER  ,USTIFM ,
     3        USTIFR ,UVISM ,UVISR  ,GBUF%VAR,NUVAR  )
            CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,0)
          ELSE
         OPTION='/PROP/USER40'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
          ENDIF
        ELSEIF (IGTYP == 41) THEN
          IF (USERL_AVAIL == 1) THEN
            CALL ST_USERLIB_RINIUSER(IGTYP,ROOTNAM,ROOTLEN,
     1        NEL      ,I0     ,
     2        UIX    ,UL    ,GBUF%MASS  ,UINER  ,USTIFM ,
     3        USTIFR ,UVISM ,UVISR  ,GBUF%VAR,NUVAR  )
            CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,0)
          ELSE
         OPTION='/PROP/USER41'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
          ENDIF
        ELSEIF (IGTYP == 42) THEN
          IF (USERL_AVAIL == 1) THEN
            CALL ST_USERLIB_RINIUSER(IGTYP,ROOTNAM,ROOTLEN,
     1        NEL      ,I0     ,
     2        UIX    ,UL    ,GBUF%MASS  ,UINER  ,USTIFM ,
     3        USTIFR ,UVISM ,UVISR  ,GBUF%VAR,NUVAR  )
            CALL USER_OUTPUT(IOUT,IGTYP,ROOTNAM,ROOTLEN,0)
          ELSE
         OPTION='/PROP/USER42'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
          ENDIF
        ENDIF
C
        DO I=LFT,LLT
          J=I+NFT
          I0=IXR(1,J)
          I1=IXR(2,J)
          I2=IXR(3,J)
          I3=IXR(4,J)
          XM = GBUF%MASS(I)
          XINE = UINER(I)
          AL2= UL(I)*UL(I)
          XKR= USTIFR(I)
          XKM= USTIFM(I)
          XCR= UVISR(I)
          XCM= UVISM(I)
          STIFN(I1)=STIFN(I1)+XKM
          STIFN(I2)=STIFN(I2)+XKM
          STIFR(I1)=STIFR(I1)+XKR
          STIFR(I2)=STIFR(I2)+XKR
          STRR(J)=XKR
          IF (XCM+XKM<EM15) XM =ONE
          IF (XCR+XKR<EM15) XINE=ONE
          XKM= MAX(EM15,XKM)
          XKR= MAX(EM15,XKR)
          DT=(SQRT(XCM*XCM+XM*XKM)-XCM)/XKM
          DTC=HALF*XM / MAX(EM15,XCM)
          DT = MIN(DT,DTC)
          DTC=(SQRT(XCR*XCR+XINE*XKR)-XCR)/XKR
          DT = MIN(DT,DTC)
          DTC=HALF*XINE / MAX( EM15,XCR)
          DT = MIN(DT,DTC)
          DTELEM(NDEPAR+I)= DT
        ENDDO
C
        CALL RINI2U(
     1    IXR         ,GBUF%MASS,UINER,XMAS        ,XIN         ,
     2    PARTSAV     ,X        ,V    ,IPART(NFT+1),MSR(1,NFT+1),
     3    INR(1,NFT+1),MSRT     ,EMS  )
        IF (INISPRI /= 0) 
     .    CALL RUINI(
     2  SIGRS                ,IXR                 ,NSIGRS              ,NUVAR                ,GBUF%FOR(II(1))      ,
     3  GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,
     4  GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     5  GBUF%VR_REPCVT(II(3)),GBUF%VAR            ,GBUF%EINT           ,BIDON                ,BIDON                ,
     6  IGTYP                ,PTSPRI)
C=======================================================================
      ELSEIF (IGTYP == 44) THEN
C=======================================================================
        CALL RINI3U(GBUF%OFF ,GEO       ,X      ,UL     ,IXR ,  
     2              SKEW     ,GBUF%SKEW ,ITAB   ,UIX    ,IGEO)
        NUVAR =  NINT(GEO(25,I0))
        NUPARAM =  NINT(GEO(26,I0))
        CALL RINI44(                                 
     1    NEL    ,IOUT  ,I0         ,                      
     2    UIX    ,UL    ,GBUF%MASS  ,UINER   ,USTIFM ,   
     3    USTIFR ,UVISM ,UVISR      ,GBUF%VAR,NUVAR  )
C
        DO I=LFT,LLT
          J=I+NFT
          I0=IXR(1,J)
          I1=IXR(2,J)
          I2=IXR(3,J)
          I3=IXR(4,J)
          XM = GBUF%MASS(I)
          XINE = UINER(I)
          AL2= UL(I)*UL(I)
          XKR= USTIFR(I)
          XKM= USTIFM(I)
          XCR= UVISR(I)
          XCM= UVISM(I)
          STIFN(I1)=STIFN(I1)+XKM
          STIFN(I2)=STIFN(I2)+XKM
          STIFR(I1)=STIFR(I1)+XKR
          STIFR(I2)=STIFR(I2)+XKR
          STRR(J)=XKR
          IF(XCM+XKM<EM15)XM =ONE
          IF(XCR+XKR<EM15)XINE=ONE
          XKM= MAX(EM15,XKM)
          XKR= MAX(EM15,XKR)
          DT=(SQRT(XCM*XCM+XM*XKM)-XCM)/XKM
          DTC=HALF*XM / MAX(EM15,XCM)
          DT = MIN(DT,DTC)
          DTC=(SQRT(XCR*XCR+XINE*XKR)-XCR)/XKR
          DT = MIN(DT,DTC)
          DTC=HALF*XINE / MAX( EM15,XCR)
          DT = MIN(DT,DTC)
          DTELEM(NDEPAR+I)= DT
        ENDDO
        CALL RINI2U(
     1    IXR         ,GBUF%MASS,UINER,XMAS        ,XIN         ,
     2    PARTSAV     ,X        ,V    ,IPART(NFT+1),MSR(1,NFT+1),
     3    INR(1,NFT+1),MSRT     ,EMS  )
        IF (INISPRI /= 0) 
     .    CALL RUINI(
     2  SIGRS                ,IXR                 ,NSIGRS              ,NUVAR                ,GBUF%FOR(II(1))      ,
     3  GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,
     4  GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     5  GBUF%VR_REPCVT(II(3)),GBUF%VAR            ,GBUF%EINT           ,BIDON                ,BIDON                ,
     6  IGTYP                ,PTSPRI)
C=======================================================================
      ELSEIF (IGTYP == 46) THEN
C=======================================================================
        CALL RINI3U(GBUF%OFF ,GEO       ,X      ,UL     ,IXR ,  
     2              SKEW     ,GBUF%SKEW ,ITAB   ,UIX    ,IGEO)
        NUVAR =  NINT(GEO(25,I0))
        NUPARAM =  NINT(GEO(26,I0))
        CALL RINI46(                                 
     1    NEL    ,IOUT  ,I0        ,                      
     2    UIX    ,UL    ,GBUF%MASS ,UINER   ,USTIFM ,   
     3    USTIFR ,UVISM ,UVISR     ,GBUF%VAR,NUVAR  )
C
        DO I=LFT,LLT
          J=I+NFT
          I0=IXR(1,J)
          I1=IXR(2,J)
          I2=IXR(3,J)
          I3=IXR(4,J)
          XM = GBUF%MASS(I)
          XINE = UINER(I)
          AL2= UL(I)*UL(I)
          XKR= USTIFR(I)
          XKM= USTIFM(I)
          XCR= UVISR(I)
          XCM= UVISM(I)
          STIFN(I1)=STIFN(I1)+XKM
          STIFN(I2)=STIFN(I2)+XKM
          STIFR(I1)=STIFR(I1)+XKR
          STIFR(I2)=STIFR(I2)+XKR
          STRR(J)=XKR
          IF (XCM+XKM<EM15) XM =ONE
          IF (XCR+XKR<EM15) XINE=ONE
          XKM= MAX(EM15,XKM)
          XKR= MAX(EM15,XKR)
          DT=(SQRT(XCM*XCM+XM*XKM)-XCM)/XKM
          DTC=HALF*XM / MAX(EM15,XCM)
          DT = MIN(DT,DTC)
          DTC=(SQRT(XCR*XCR+XINE*XKR)-XCR)/XKR
          DT = MIN(DT,DTC)
          DTC=HALF*XINE / MAX( EM15,XCR)
          DT = MIN(DT,DTC)
          DTELEM(NDEPAR+I)= DT
        ENDDO
C
        CALL RINI2U(
     1    IXR         ,GBUF%MASS,UINER,XMAS        ,XIN         ,
     2    PARTSAV     ,X        ,V    ,IPART(NFT+1),MSR(1,NFT+1),
     3    INR(1,NFT+1),MSRT     ,EMS  )
C
        IF (INISPRI /= 0) 
     .   CALL RUINI(
     2  SIGRS                ,IXR                 ,NSIGRS              ,NUVAR                ,GBUF%FOR(II(1))      ,
     3  GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,
     4  GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     5  GBUF%VR_REPCVT(II(3)),GBUF%VAR            ,GBUF%EINT           ,BIDON                ,BIDON                ,
     6  IGTYP                ,PTSPRI)
C-----
      ENDIF  ! IGTYP
C------------------------------------------
C     CALCUL DES DT ELEMENTAIRES & DT Nodal (take into account XCM)
C------------------------------------------
C
      K1 = 4
      K11 = 64
      K12 = K11 + 6
      K13 = K12 + 6
      K14 = K13 + 6
      DO I=LFT,LLT
        J=I+NFT
        I0=IXR(1,J)
        I1=IXR(2,J)
        I2=IXR(3,J)
        I3=IXR(4,J)
        IGTYP=IGEO(11,I0)
        IPID=IXR(1,I+NFT)
C
        IF (IGTYP == 4) THEN
          XM = GEO(1,I0)*XL(I)
          XKM= GEO(2,I0)*GEO(10,I0)/XL(I)
          XCM= (GEO(3,I0)) +GEO(141,I0) /XL(I)!
          IF (XCM /= ZERO .AND. XKM /= ZERO) THEN
            DT=XM/(SQRT(XCM*XCM+XKM*XM)+XCM)
          ELSEIF (XKM /= ZERO) THEN
            DT=SQRT(XM/XKM)
          ELSEIF (XCM /= ZERO) THEN
            DT=XM/XCM
          ELSE
            DT=EP20
          ENDIF
          DTC=HALF*XM / MAX(EM15,XCM)
          DTELEM(NDEPAR+I)=MIN(DT,DTC)
          MAS2 = TWO*MSR(1,J)
          IF (MAS2>ZERO) THEN
           STI = (SQRT(XCM**2+XKM*MAS2)+XCM)**2/MAS2
          ELSE
           STI = XKM
          END IF
          STIFN(I1)=STIFN(I1)+STI
          STIFN(I2)=STIFN(I2)+STI
        ELSEIF (IGTYP == 26) THEN
          XM = GEO(1,I0)*XL(I)
          XKM= GEO(2,I0)/XL(I)
          XCM= ZERO
          IF (XKM > ZERO) THEN
            DT=SQRT(XM/XKM)
          ELSE
            DT=EP20
          ENDIF
          DTC=HALF*XM / MAX(EM15,XCM)
          DTELEM(NDEPAR+I)=MIN(DT,DTC)
          STIFN(I1)=STIFN(I1)+XKM
          STIFN(I2)=STIFN(I2)+XKM
        ELSEIF (IGTYP == 8) THEN
          XKM= MAX(GEO(3,I0)*GEO(41,I0),
     .             GEO(10,I0)*GEO(45,I0),
     .             GEO(15,I0)*GEO(49,I0))/XL(I)
          XCM= (MAX(GEO(4,I0),GEO(11,I0),GEO(16,I0))
     .        + MAX(GEO(141,I0),GEO(142,I0),GEO(143,I0)))/XL(I)
          XKR= MAX(GEO(19,I0)*GEO(53,I0),
     .             GEO(23,I0)*GEO(57,I0),
     .             GEO(27,I0)*GEO(61,I0))/XL(I)
          XCR= (MAX(GEO(20,I0),GEO(24,I0),GEO(28,I0))
     .        + MAX(GEO(144,I0),GEO(145,I0),GEO(146,I0)))/XL(I)
          XM=GEO(1,I0)*XL(I)
          XINE=GEO(9,I0)*XL(I)
          IF (XCM+XKM<EM15) XM =ONE
          IF (XCR+XKR<EM15) XINE=ONE
          XKM= MAX(EM15,XKM)
          XKR= MAX(EM15,XKR)
          DT=XM/MAX(EM15,SQRT(XCM*XCM+XKM*XM)+XCM)
          DTC=XINE/MAX(EM15,SQRT(XCR*XCR+XINE*XKR)+XCR)
          DTELEM(NDEPAR+I)=MIN(DT,DTC)
          MAS2 = TWO*MSR(1,J)
          IF (MAS2>ZERO) THEN
           STI = (SQRT(XCM**2+XKM*MAS2)+XCM)**2/MAS2
          ELSE
           STI = XKM
          END IF
          STIFN(I1)=STIFN(I1)+STI
          STIFN(I2)=STIFN(I2)+STI
          MAS2 = INR(1,J)
          IF (MAS2>ZERO) THEN
            STI = (SQRT(XCR**2+XKR*MAS2)+XCR)**2/MAS2
          ELSE
            STI = XKR
          END IF
          STIFR(I1)=STIFR(I1)+STI
          STIFR(I2)=STIFR(I2)+STI
          STRR(J)=XKR
        ELSEIF(IGTYP == 12) THEN
          XM = GEO(1,I0)*XL(I)
          XKM= GEO(2,I0)/XL(I)
          XCM= (GEO(3,I0)+GEO(141,I0))/XL(I)
          IF (XCM /= ZERO .AND. XKM /= ZERO) THEN
            DT=XM/(TWO*SQRT(XCM*XCM+XKM*XM)+XCM)
          ELSEIF (XKM /= ZERO) THEN
            DT=SQRT(XM/XKM)
          ELSEIF (XCM /= ZERO) THEN
            DT=XM/XCM
          ELSE
            DT=EP20
          ENDIF
          DTC=HALF*XM / MAX(EM15,XCM)
          DTELEM(NDEPAR+I)=MIN(DT,DTC)
          MAS2 = TWO*MSR(2,J)
          IF (MAS2>ZERO) THEN
           STI = (SQRT(XCM**2+XKM*MAS2)+XCM)**2/MAS2
          ELSE
           STI = XKM
          END IF
          STIFN(I2)=STIFN(I2)+STI
          MAS2 = TWO*MSR(1,J)
          STI = (SQRT(XCM**2+XKM*MAS2)+XCM)**2/MAS2
          STIFN(I1)=STIFN(I1)+STI
          STIFN(I3)=STIFN(I3)+STI
        ELSEIF (IGTYP == 13) THEN
          EX=X(1,I2)-X(1,I1)
          EY=X(2,I2)-X(2,I1)
          EZ=X(3,I2)-X(3,I1)
          AL2= EX*EX+EY*EY+EZ*EZ
          XKM= MAX(GEO(3,I0)*GEO(41,I0),
     .             GEO(10,I0)*GEO(45,I0),
     .             GEO(15,I0)*GEO(49,I0))/XL(I)
          XCM= (MAX(GEO(4,I0),GEO(11,I0),GEO(16,I0))
     .         + MAX(GEO(141,I0),GEO(142,I0),GEO(143,I0)) )/XL(I)
          XKR= MAX(GEO(10,I0)*GEO(45,I0),
     .             GEO(15,I0)*GEO(49,I0)) * AL2
          XCR= (MAX(GEO(11,I0),GEO(16,I0))+ MAX(GEO(142,I0),GEO(143,I0)))* AL2
          XKR= ( XKR
     .          +MAX(GEO(19,I0)*GEO(53,I0),
     .               GEO(23,I0)*GEO(57,I0),
     .               GEO(27,I0)*GEO(61,I0)))/XL(I)
          XCR= (XCR+MAX(GEO(20,I0),GEO(24,I0),GEO(28,I0))
     .        +   MAX(GEO(144,I0),GEO(145,I0),GEO(146,I0)) )/XL(I)
          XM=GEO(1,I0)*XL(I)
          XINE=GEO(9,I0)*XL(I)
          IF (XCM+XKM<EM15) XM =ONE
          IF (XCR+XKR<EM15) XINE=ONE
          XKM= MAX(EM15,XKM)
          XKR= MAX(EM15,XKR)
          DT=XM/MAX(EM15,SQRT(XCM*XCM+XKM*XM)+XCM)
          DTC=XINE/MAX(EM15,SQRT(XCR*XCR+XINE*XKR)+XCR)
          DT = MIN(DT,DTC)
          DTELEM(NDEPAR+I)= DT
          MAS2 = TWO*MSR(1,J)
          IF (MAS2>ZERO) THEN
           STI = (SQRT(XCM**2+XKM*MAS2)+XCM)**2/MAS2
          ELSE
           STI = XKM
          END IF
          STIFN(I1)=STIFN(I1)+STI
          STIFN(I2)=STIFN(I2)+STI
          MAS2 = TWO*INR(1,J)
          IF (MAS2>ZERO) THEN
            STI = (SQRT(XCR**2+XKR*MAS2)+XCR)**2/MAS2
          ELSE
            STI = XKR
          END IF
          STIFR(I1)=STIFR(I1)+STI
          STIFR(I2)=STIFR(I2)+STI
          STRR(J)=XKR
        ELSEIF (IGTYP == 23) THEN
          IMAT   = IXR(5,I+NFT)
          IADBUF = IPM(7,IMAT) - 1
          MTN    = IPM(2,IMAT)
          IF(MTN == 108) THEN
              XKM= MAX(UPARAM(IADBUF + K11 + 1)*UPARAM(IADBUF + K1 + 1),
     .                 UPARAM(IADBUF + K11 + 2)*UPARAM(IADBUF + K1 + 2),
     .                 UPARAM(IADBUF + K11 + 3)*UPARAM(IADBUF + K1 + 3))/XL(I)
              XCM= MAX(UPARAM(IADBUF + K12 + 1),UPARAM(IADBUF + K12 + 2),UPARAM(IADBUF + K12 + 3))
C
              XKR= MAX(UPARAM(IADBUF + K11 + 4)*UPARAM(IADBUF + K1 + 4),
     .                 UPARAM(IADBUF + K11 + 5)*UPARAM(IADBUF + K1 + 5),
     .                 UPARAM(IADBUF + K11 + 6)*UPARAM(IADBUF + K1 + 6))/XL(I)
C          
              XCR= (MAX(UPARAM(IADBUF + K12 + 4),UPARAM(IADBUF + K12 + 5),UPARAM(IADBUF + K12 + 6)))/XL(I) 
              ! old Geo 144,145,146 not used.
              XM  = GBUF%MASS(I)*XL(I)
              XINE= GEO(2,I0)*XL(I)
              IF (XCM+XKM<EM15) XM  =ONE
              IF (XCR+XKR<EM15) XINE=ONE
              XKM= MAX(EM15,XKM)
              XKR= MAX(EM15,XKR)
              DT =XM/MAX(EM15,SQRT(XCM*XCM+XKM*XM)+XCM)
              DTC=XINE/MAX(EM15,SQRT(XCR*XCR+XINE*XKR)+XCR)
              DTELEM(NDEPAR+I)=MIN(DT,DTC)
              GEO(4,I0)= MIN(GEO(4,I0),DT,DTC)  ! to be fixed also put it in buffer material
              MAS2 = TWO*MSR(1,J)
              INE2 = TWO*INR(1,J)
          ELSEIF (MTN==113) THEN           
              EX=X(1,I2)-X(1,I1)
              EY=X(2,I2)-X(2,I1)
              EZ=X(3,I2)-X(3,I1)
              AL2= EX*EX+EY*EY+EZ*EZ
              XKM= MAX(UPARAM(IADBUF + K11 + 1)*UPARAM(IADBUF + K1 + 1),
     .                 UPARAM(IADBUF + K11 + 2)*UPARAM(IADBUF + K1 + 2),
     .                 UPARAM(IADBUF + K11 + 3)*UPARAM(IADBUF + K1 + 3))/XL(I)
              XCM=  (MAX(UPARAM(IADBUF + K12 +1),UPARAM(IADBUF + K12 +2 ),UPARAM(IADBUF + K12 + 3))
     .             + MAX(UPARAM(IADBUF + K14 + 1),UPARAM(IADBUF + K14 + 2),UPARAM(IADBUF + K14 + 3)))/XL(I)
              XKR= MAX(UPARAM(IADBUF + K11 + 2)*UPARAM(IADBUF + K1 + 2),
     .                 UPARAM(IADBUF + K11 + 3)*UPARAM(IADBUF + K1 + 3)) * AL2
              XCR= (MAX(UPARAM(IADBUF + K12 + 2),UPARAM(IADBUF + K12 + 3)) + 
     .              MAX(UPARAM(IADBUF + K14 + 2),UPARAM(IADBUF + K14 + 3)))* AL2
              XKR= ( XKR
     .              + MAX(UPARAM(IADBUF + K11 + 4)*UPARAM(IADBUF + K1 + 4),
     .                   UPARAM(IADBUF + K11 + 5)*UPARAM(IADBUF + K1 + 5),
     .                   UPARAM(IADBUF + K11 + 6)*UPARAM(IADBUF + K1 + 6)))/XL(I)
              XCR= (XCR+MAX(UPARAM(IAD + K12 + 1),UPARAM(IADBUF + K12 + 2),UPARAM(IADBUF + K12 + 3))
     .            +     MAX(UPARAM(IAD + K14 + 4),UPARAM(IADBUF + K14 + 5),UPARAM(IADBUF + K14 + 6)) )/XL(I)
              XM  =GBUF%MASS(I)
              XINE=GEO(2,I0)*XL(I)
              IF (XCM+XKM<EM15) XM  =ONE
              IF (XCR+XKR<EM15) XINE=ONE
              XKM= MAX(EM15,XKM)
              XKR= MAX(EM15,XKR)
              DT =XM/MAX(EM15,SQRT(XCM*XCM+XKM*XM)+XCM)
              DTC=XINE/MAX(EM15,SQRT(XCR*XCR+XINE*XKR)+XCR)
              DT = MIN(DT,DTC)
              GEO(4,I0)= MIN(GEO(4,I0),DT)
              DTELEM(NDEPAR+I)= DT
              MAS2 = TWO*MSR(1,J)
              INE2 = TWO*INR(1,J)
          ELSEIF (MTN==114) THEN           
              EX=X(1,I2)-X(1,I1)
              EY=X(2,I2)-X(2,I1)
              EZ=X(3,I2)-X(3,I1)
              AL2= EX*EX+EY*EY+EZ*EZ
C
              XKM= MAX(UPARAM(IADBUF + K11 + 1)*UPARAM(IADBUF + K1 + 1),
     .                 UPARAM(IADBUF + K11 + 2)*UPARAM(IADBUF + K1 + 2),
     .                 UPARAM(IADBUF + K11 + 3)*UPARAM(IADBUF + K1 + 3),
     .                 UPARAM(IADBUF+117)*GEO(1,I0))/XL(I)
C
              XCM=  (MAX(UPARAM(IADBUF + K12 +1),UPARAM(IADBUF + K12 +2 ),UPARAM(IADBUF + K12 + 3))
     .             + MAX(UPARAM(IADBUF + K14 + 1),UPARAM(IADBUF + K14 + 2),UPARAM(IADBUF + K14 + 3)))/XL(I)
              XKR= MAX(UPARAM(IADBUF + K11 + 2)*UPARAM(IADBUF + K1 + 2),
     .                 UPARAM(IADBUF + K11 + 3)*UPARAM(IADBUF + K1 + 3)) * AL2
              XCR= (MAX(UPARAM(IADBUF + K12 + 2),UPARAM(IADBUF + K12 + 3)) + 
     .              MAX(UPARAM(IADBUF + K14 + 2),UPARAM(IADBUF + K14 + 3)))* AL2
              XKR= ( XKR
     .              + MAX(UPARAM(IADBUF + K11 + 4)*UPARAM(IADBUF + K1 + 4),
     .                   UPARAM(IADBUF + K11 + 5)*UPARAM(IADBUF + K1 + 5),
     .                   UPARAM(IADBUF + K11 + 6)*UPARAM(IADBUF + K1 + 6)))/XL(I)
              XCR= (XCR+MAX(UPARAM(IADBUF+K12 + 1),UPARAM(IADBUF+ K12 + 2),UPARAM(IADBUF+ K12 + 3))
     .            +     MAX(UPARAM(IADBUF+K14 + 4),UPARAM(IADBUF+ K14 + 5),UPARAM(IADBUF+ K14 + 6)) )/XL(I)
C-
              IF (UPARAM(IADBUF + 127) > ZERO) THEN
C-              1D material of 2D seatbelt - element mass and inertia recomputed for elementary time step
                RHO = UPARAM(IADBUF+128)
                XM = RHO*XL(I)*GEO(1,I0)
                XINE=MAX(EM20,MAX((RHO*GEO(1,I0)*LENGTH*LENGTH*LENGTH)/TWELVE+ RHO*IYY*LENGTH,RHO*IXX*LENGTH))
                GBUF%MASS(I) = XM*GBUF%FRAM_FACTOR(I)
                GBUF%INTVAR(I) = XINE*GBUF%FRAM_FACTOR(I)
                MAS2 = XM
                INE2 = XINE
              ELSE
                GBUF%FRAM_FACTOR(I) = ONE
                XM  =GBUF%MASS(I)
                XINE=UINER(I)
                GBUF%INTVAR(I) = XINE
                MAS2 = TWO*MSR(1,J)
                INE2 = TWO*INR(1,J)
              ENDIF
C
              IF (GBUF%SLIPRING_STRAND(I) > 0) THEN
C--------->     Update of third node if seatbelt spring in slipring------------
                SLIP = GBUF%SLIPRING_ID(I)
                FRA = GBUF%SLIPRING_FRAM_ID(I)
                DO KK=1,3
                  IF ((SLIPRING(SLIP)%FRAM(FRA)%NODE(KK)/=I1).AND.(SLIPRING(SLIP)%FRAM(FRA)%NODE(KK)/=I2)) THEN
                     IXR(4,J)=SLIPRING(SLIP)%FRAM(FRA)%NODE(KK)
                  ENDIF
                ENDDO
              ELSEIF (GBUF%RETRACTOR_ID(I) < 0) THEN
C--------->     Deactivation of elements initially in retractor------------
                GBUF%OFF(I) = ZERO
                GBUF%RETRACTOR_ID(I) = 0
              ENDIF
C
              IF (XCM+XKM<EM15) XM  =ONE
              IF (XCR+XKR<EM15) XINE=ONE
              XKM= MAX(EM15,XKM)
              XKR= MAX(EM15,XKR)
              DT =XM/MAX(EM15,SQRT(XCM*XCM+XKM*XM)+XCM)
              DTC=XINE/MAX(EM15,SQRT(XCR*XCR+XINE*XKR)+XCR)
              DT = MIN(DT,DTC)
              GEO(4,I0)= MIN(GEO(4,I0),DT)
              DTELEM(NDEPAR+I)= DT
           ENDIF   
           IF (MAS2>ZERO) THEN
            STI = (SQRT(XCM**2+XKM*MAS2)+XCM)**2/MAS2
           ELSE
            STI = XKM
           END IF
           STIFN(I1)=STIFN(I1)+STI
           STIFN(I2)=STIFN(I2)+STI
           IF (INE2>ZERO) THEN
            STI = (SQRT(XCR**2+XKR*INE2)+XCR)**2/INE2
           ELSE
            STI = XKR
           END IF
           STIFR(I1)=STIFR(I1)+STI
           STIFR(I2)=STIFR(I2)+STI
           STRR(J)=XKR
        ELSEIF (IGTYP == 27) THEN
          XM = GEO(1,I0)*XL(I)
          XKM= GEO(2,I0)*GEO(10,I0)/XL(I)
          XCM= (GEO(3,I0)+GEO(141,I0))/XL(I)!
          IF (XCM /= ZERO .AND. XKM /= ZERO) THEN
            DT=XM/(SQRT(XCM*XCM+XKM*XM)+XCM)
          ELSEIF (XKM /= ZERO) THEN
            DT=SQRT(XM/XKM)
          ELSEIF (XCM /= ZERO) THEN
            DT=XM/XCM
          ELSE
            DT=EP20
          ENDIF
          DTC=HALF*XM / MAX(EM15,XCM)
          DTELEM(NDEPAR+I)=MIN(DT,DTC)
          MAS2 = TWO*MSR(1,J)
          IF (MAS2>ZERO) THEN
           STI = (SQRT(XCM**2+XKM*MAS2)+XCM)**2/MAS2
          ELSE
           STI = XKM
          END IF
          STIFN(I1)=STIFN(I1)+STI
          STIFN(I2)=STIFN(I2)+STI
        END IF 
      ENDDO
      IDS = 42
c      CALL ANCHECK(IDS)
C
      IDS = 43
c      CALL ANCHECK(IDS)
C
      IDS = 40
c      CALL ANCHECK(IDS)
      IDS = 37
c      CALL ANCHECK(IDS)
C-----
 1000 FORMAT('LIST OF POSSIBLE CNODES MERGED WITH NODE OF ID=',I10)
C-----
      RETURN
      END
Chd|====================================================================
Chd|  RINI3U                        source/elements/spring/rinit3.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RINI3U(OFF  ,GEO  ,X     ,X0    ,IX   ,
     .                  SKEW ,RLOC ,ITAB  ,UIX   ,IGEO )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX(NIXR,*),UIX(4,MVSIZ),ITAB(*), IGEO(NPROPGI,*)
C     REAL
      my_real
     .   OFF(*), GEO(NPROPG,*), X(3,*), X0(*), SKEW(LSKEW,*),
     .   RLOC(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NG, I1, I2, I3, ISK
C     REAL
      my_real
     .   X1, Y1, Z1, IDIR
C-----------------------------------------------
      DO I=LFT,LLT
        OFF(I) = ONE
      ENDDO
C
      DO I=LFT,LLT
        J=I+NFT
        NG=IX(1,J)
        ISK=IGEO(2,NG)
        I1=IX(2,J)
        I2=IX(3,J)
        I3=IX(4,J)
        X1=X(1,I2)-X(1,I1)
        Y1=X(2,I2)-X(2,I1)
        Z1=X(3,I2)-X(3,I1)
        X0(I)=SQRT(X1**2+Y1**2+Z1**2)
        IF (X0(I) < EM15) THEN
          RLOC(1,I)= ONE
          RLOC(2,I)= ZERO
          RLOC(3,I)= ZERO
          RLOC(4,I)= ZERO
          RLOC(5,I)= ONE
          RLOC(6,I)= ZERO
        ELSEIF (I3 /= 0) THEN
          RLOC(1,I)=X1
          RLOC(2,I)=Y1
          RLOC(3,I)=Z1
          RLOC(4,I)=X(1,I3)-X(1,I1)
          RLOC(5,I)=X(2,I3)-X(2,I1)
          RLOC(6,I)=X(3,I3)-X(3,I1)
        ELSEIF (ISK /= 1) THEN
          RLOC(1,I)= X1
          RLOC(2,I)= Y1
          RLOC(3,I)= Z1
          RLOC(4,I)=SKEW(4,ISK)
          RLOC(5,I)=SKEW(5,ISK)
          RLOC(6,I)=SKEW(6,ISK)
        ELSE
          RLOC(1,I)=X1
          RLOC(2,I)=Y1
          RLOC(3,I)=Z1
          IF (ABS(Y1) < HALF*X0(I)) THEN
            RLOC(4,I)=ZERO
            RLOC(5,I)=ONE
            RLOC(6,I)=ZERO
          ELSE
            RLOC(4,I)=ONE
            RLOC(5,I)=ZERO
            RLOC(6,I)=ZERO
          ENDIF
        ENDIF
        UIX(1,I)=ITAB(I1)
        UIX(2,I)=ITAB(I2)
        IF(I3 /= 0) THEN
           UIX(3,I)=ITAB(I3)
        ELSE
           UIX(3,I)=0
        ENDIF
        UIX(4,I)=IX(6,J)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RINI1U                        source/elements/spring/rinit3.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RINI1U(OFF  ,GEO  ,X     ,X0    ,IX   ,
     .                  SKEW ,RLOC ,ITAB  ,UIX   ,IGEO )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX(NIXR,*),UIX(4,MVSIZ),ITAB(*), IGEO(NPROPGI,*)
C     REAL
      my_real
     .   OFF(*), GEO(NPROPG,*), X(3,*), X0(*), SKEW(LSKEW,*)
C     REAL
      my_real
     .   RLOC(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NG, I1, I2, I3, ISK
C     REAL
      my_real
     .   X1, Y1, Z1
C-----------------------------------------------
      DO I=LFT,LLT
        OFF(I) = ONE
      ENDDO
C
      DO I=LFT,LLT
        J=I+NFT
        NG=IX(1,J)
        ISK=IGEO(2,NG)
        I1=IX(2,J)
        I2=IX(3,J)
        I3=IX(4,J)
        X1=X(1,I2)-X(1,I1)
        Y1=X(2,I2)-X(2,I1)
        Z1=X(3,I2)-X(3,I1)
        X0(I)=SQRT(X1**2+Y1**2+Z1**2)
        IF (X0(I) < EM15) THEN
           RLOC(1,I)= ONE
           RLOC(2,I)= ZERO
           RLOC(3,I)= ZERO
        ELSEIF (I3 /= 0) THEN
          RLOC(1,I)=X(1,I3)-X(1,I1)
          RLOC(2,I)=X(2,I3)-X(2,I1)
          RLOC(3,I)=X(3,I3)-X(3,I1)
        ELSEIF( ISK /= 1) THEN
          RLOC(1,I)=SKEW(4,ISK)
          RLOC(2,I)=SKEW(5,ISK)
          RLOC(3,I)=SKEW(6,ISK)
        ELSE
          IF (ABS(Y1) < HALF*X0(I)) THEN
            RLOC(1,I)=ZERO
            RLOC(2,I)=ONE
            RLOC(3,I)=ZERO
          ELSE
            RLOC(1,I)=ONE
            RLOC(2,I)=ZERO
            RLOC(3,I)=ZERO
          ENDIF
        ENDIF
        UIX(1,I)=ITAB(I1)
        UIX(2,I)=ITAB(I2)
        IF (I3 == 0) THEN
          UIX(3,I)=0
        ELSE
          UIX(3,I)=ITAB(I3)
        ENDIF
        UIX(4,I)=IX(6,J)
      ENDDO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  RINI2U                        source/elements/spring/rinit3.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RINI2U(IXR     ,UMASS ,UINER ,MS    ,XIN,
     .                  PARTSAV ,X     ,V     ,IPART ,MSR,
     .                  INR     ,MSRT  ,EMS   )
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
C----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*),IPART(*)
      my_real
     .   UMASS(*),UINER(*), MS(*), XIN(*),X(3,*),V(3,*),
     .   PARTSAV(20,*), MSR(3,*), INR(3,*), MSRT(*),EMS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IP,I1,I2
C     REAL
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX
      my_real
     .   XI
C---------------------------------------------------------------------
C----------------------------------------------
C     MASSE ELEMENT /2
C----------------------------------------------
      DO I=LFT,LLT
        EMS(I)=HALF*UMASS(I)
      ENDDO
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
C----------------------------------------------
C     traitement specifique spmd pour mass et iner parith/on
      IF (IMACH /= 3) THEN
        DO I=LFT,LLT
          I1 = IXR(2,I+NFT)
          I2 = IXR(3,I+NFT)
C
          XI=HALF*UINER(I)
          MS(I1)=MS(I1)+EMS(I)
          MS(I2)=MS(I2)+EMS(I)
          XIN(I1)=XIN(I1)+XI
          XIN(I2)=XIN(I2)+XI
C
          IP=IPART(I)
          PARTSAV(1,IP)=PARTSAV(1,IP) + TWO*EMS(I)
          PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*(X(1,I1)+X(1,I2))
          PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*(X(2,I1)+X(2,I2))
          PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*(X(3,I1)+X(3,I2))
          XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2))
          XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2))
          YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2))
          YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2))
          ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2))
          ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2))
          PARTSAV(5,IP) =PARTSAV(5,IP) + TWO*XI + EMS(I) * (YY+ZZ)
          PARTSAV(6,IP) =PARTSAV(6,IP) + TWO*XI + EMS(I) * (ZZ+XX)
          PARTSAV(7,IP) =PARTSAV(7,IP) + TWO*XI + EMS(I) * (XX+YY)
          PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
          PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
          PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
          PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*(V(1,I1)+V(1,I2))
          PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*(V(2,I1)+V(2,I2))
          PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*(V(3,I1)+V(3,I2))
          PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .       (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .       +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))
        ENDDO
      ELSE
        DO I=LFT,LLT
          I1 = IXR(2,I+NFT)
          I2 = IXR(3,I+NFT)
C
          XI=HALF*UINER(I)
          MSR(1,I)=EMS(I)
          MSR(2,I)=EMS(I)
          MSR(3,I)=EMS(I)
          INR(1,I)=XI
          INR(2,I)=XI
          INR(3,I)=XI
C
          IP=IPART(I)
          PARTSAV(1,IP)=PARTSAV(1,IP) + TWO*EMS(I)
          PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*(X(1,I1)+X(1,I2))
          PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*(X(2,I1)+X(2,I2))
          PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*(X(3,I1)+X(3,I2))
          XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2))
          XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2))
          YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2))
          YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2))
          ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2))
          ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2))
          PARTSAV(5,IP) =PARTSAV(5,IP) + TWO*XI + EMS(I) * (YY+ZZ)
          PARTSAV(6,IP) =PARTSAV(6,IP) + TWO*XI + EMS(I) * (ZZ+XX)
          PARTSAV(7,IP) =PARTSAV(7,IP) + TWO*XI + EMS(I) * (XX+YY)
          PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
          PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
          PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
          PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*(V(1,I1)+V(1,I2))
          PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*(V(2,I1)+V(2,I2))
          PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*(V(3,I1)+V(3,I2))
          PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .       (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .       +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))
        ENDDO
      ENDIF
C
      IF (IREST_MSELT /= 0)THEN
        DO I=LFT,LLT
          MSRT(I)=UMASS(I)
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  R4INI                         source/elements/spring/rinit3.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R4INI(SIGRS ,IXR  ,NSIGI ,EINT  ,F   ,
     .                 DL    ,FEP  ,DPL   ,DPL2  ,XL0 ,
     .                 DFS   ,DV   ,IGTYP ,PTSPRI,DL0 ,
     .                 F0    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,N,NSIGI,NUVAR,NEL,IGTYP
      INTEGER IXR(NIXR,*),PTSPRI(*)
C     REAL
      my_real
     .   F(*),EINT(*),SIGRS(NSIGI,*),DPL(*),DPL2(*),DFS(*),
     .   FEP(*),DL(*),XL0(*),DV(*),DL0(*),F0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,JJ,K
C----------------------------------------------------------------------- 
C---
C     CONTRAINTES INITIALES + OTHERS
C---
      IF (INISPRI /= 0) THEN
        DO I=LFT,LLT
          II = I+NFT
!         length recumputed in engine if not concerned by INISPRI
          XL0(I)   = ZERO
!
          JJ = PTSPRI(II)
          IF( JJ == 0) GOTO 200
C---
!!          F(I)    = SIGRS(2,JJ)
          F0(I)   = SIGRS(2,JJ)
!!          DL(I)   = SIGRS(3,JJ)
          DL0(I)  = SIGRS(3,JJ)
          FEP(I)  = SIGRS(4,JJ)
          IF (IGTYP /= 26) THEN ! IGTYP = 4,12
            DPL(I)  = SIGRS(5,JJ)
            DPL2(I) = SIGRS(6,JJ)
          ENDIF
          XL0(I)  = SIGRS(7,JJ)
          EINT(I) = SIGRS(8,JJ) 
          IF (IGTYP == 12) THEN
            DFS(I) = SIGRS(9,JJ)
          ELSEIF (IGTYP == 26) THEN
            DV(I) = SIGRS(9,JJ)
          ENDIF
C---
 200      CONTINUE 
        ENDDO ! DO I=LFT,LLT
      ENDIF ! IF (INISPRI /= 0)
C---
      RETURN
      END
Chd|====================================================================
Chd|  R8INI                         source/elements/spring/rinit3.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R8INI(IGTYP ,NEL  ,SIGRS ,IXR   ,NSIGI,
     .                 FX    ,FY   ,FZ    ,MX    ,MY   ,
     .                 MZ    ,FXEP ,FYEP  ,FZEP  ,XMEP ,
     .                 YMEP  ,ZMEP ,DXPL  ,DYPL  ,DZPL ,
     .                 RPX   ,RPY  ,RPZ   ,DXPL2 ,DYPL2, 
     .                 DZPL2 ,RPX2 ,RPY2  ,RPZ2  ,DX   ,
     .                 DY    ,DZ   ,RX    ,RY    ,RZ   ,
     .                 XL0   ,YL0  ,ZL0   ,EINT  ,E6   ,
     .                 PTSPRI,DX0  ,DY0   ,DZ0   ,RX0  ,
     .                 RY0   ,RZ0  ,FX0   ,FY0   ,FZ0  ,
     .                 MX0   ,MY0  ,MZ0   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include       "com04_c.inc"
#include       "param_c.inc"
#include       "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,N
      INTEGER IXR(NIXR,*),NSIGI,NUVAR,NEL,IGTYP,PTSPRI(*)
C     REAL
      my_real
     .   FX(*),FY(*),FZ(*),EINT(*),SIGRS(NSIGI,*),
     .   MX(*),MY(*),MZ(*),DXPL(*),DYPL(*),DZPL(*),
     .   DXPL2(*),DZPL2(*),DYPL2(*),FXEP(*),FYEP(*),FZEP(*),
     .   XMEP(*),YMEP(*),ZMEP(*),RPX(*),RPY(*),RPZ(*),
     .   RPX2(*),RPY2(*),RPZ2(*),DX(*),DY(*),DZ(*),RX(*),
     .   RY(*),RZ(*),XL0(*),YL0(*),ZL0(*),E6(NEL,6),
     .   DX0(*),DY0(*),DZ0(*),RX0(*),RY0(*),RZ0(*),
     .   FX0(*),FY0(*),FZ0(*),MX0(*),MY0(*),MZ0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,JJ,K
C----------------------------------------------------------------------- 
C---
C     CONTRAINTES INITIALES + OTHERS
C---
      IF (INISPRI /= 0) THEN
        DO I=LFT,LLT
          II = I+NFT
!         length recumputed in engine if not concerned by INISPRI
          XL0(I)   = ZERO
          YL0(I)   = ZERO
          ZL0(I)   = ZERO
!
          JJ = PTSPRI(II)
          IF (JJ == 0) GOTO 200
C---
!          FX(I)	   = SIGRS(2, JJ)
!          DX(I)	   = SIGRS(3, JJ)
          FXEP(I)  = SIGRS(4, JJ)
          DXPL(I)  = SIGRS(5, JJ)
          DXPL2(I) = SIGRS(6, JJ)
!          FY(I)    = SIGRS(7, JJ)
!          DY(I)    = SIGRS(8, JJ)
          FYEP(I)  = SIGRS(9,JJ)
          DYPL(I)  = SIGRS(10,JJ) 
          DYPL2(I) = SIGRS(11,JJ)
!          FZ(I)    = SIGRS(12,JJ)
!          DZ(I)    = SIGRS(13,JJ)
          FZEP(I)  = SIGRS(14,JJ)
          DZPL(I)  = SIGRS(15,JJ)
          DZPL2(I) = SIGRS(16,JJ)
!          MX(I)	   = SIGRS(17,JJ)
!          RX(I)	   = SIGRS(18,JJ)
          XMEP(I)  = SIGRS(19,JJ)
          RPX(I)   = SIGRS(20,JJ)
          RPX2(I)  = SIGRS(21,JJ)
!          MY(I)    = SIGRS(22,JJ)
!          RY(I)    = SIGRS(23,JJ)
          YMEP(I)  = SIGRS(24,JJ) 
          RPY(I)   = SIGRS(25,JJ)
          RPY2(I)  = SIGRS(26,JJ)
!          MZ(I)	   = SIGRS(27,JJ) 
!          RZ(I)	   = SIGRS(28,JJ)
          ZMEP(I)  = SIGRS(29,JJ)
          RPZ(I)   = SIGRS(30,JJ)
          RPZ2(I)  = SIGRS(31,JJ)
          XL0(I)   = SIGRS(32,JJ)
          YL0(I)   = SIGRS(33,JJ)
          ZL0(I)   = SIGRS(34,JJ)
!!          EINT(I)  = SIGRS(35,JJ)
          IF (IGTYP == 8  .OR. IGTYP == 13 .OR.
     .        IGTYP == 23 .OR. IGTYP == 25) THEN
            DX0(I) = SIGRS(3, JJ)
            FX0(I) = SIGRS(2, JJ)
            DY0(I) = SIGRS(8, JJ)
            FY0(I) = SIGRS(7, JJ)
            DZ0(I) = SIGRS(13,JJ)
            FZ0(I) = SIGRS(12,JJ)
            RX0(I) = SIGRS(18,JJ)
            MX0(I) = SIGRS(17,JJ)
            RY0(I) = SIGRS(23,JJ)
            MY0(I) = SIGRS(22,JJ)
            RZ0(I) = SIGRS(28,JJ)
            MZ0(I) = SIGRS(27,JJ)
!
            E6(I,1) = SIGRS(36,JJ)
            E6(I,2) = SIGRS(37,JJ)
            E6(I,3) = SIGRS(38,JJ)
            E6(I,4) = SIGRS(39,JJ)
            E6(I,5) = SIGRS(40,JJ)
            E6(I,6) = SIGRS(41,JJ)
          ENDIF
C---
 200      CONTINUE      
        ENDDO ! DO I=LFT,LLT
      ENDIF  ! IF (INISPRI /= 0)
C---
      RETURN
      END      
Chd|====================================================================
Chd|  RUINI                         source/elements/spring/rinit3.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RUINI(
     .                 SIGRS,IXR   ,NSIGI,NUVAR,FX   ,
     .                 FY   ,FZ    ,XMOM ,YMOM ,ZMOM ,
     .                 DX   ,DY    ,DZ   ,RX   ,RY   ,
     .                 RZ   ,UVAR  ,EINT ,YMOM1,ZMOM1,
     .                 IGTYP,PTSPRI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include       "com04_c.inc"
#include       "param_c.inc"
#include       "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,N,NUVAR,IGTYP,NSIGI,NEL
      INTEGER IXR(NIXR,*),PTSPRI(*)
C     REAL
      my_real
     .   FX(*),FY(*),FZ(*),XMOM(*),YMOM(*),ZMOM(*),
     .   EINT(*),SIGRS(NSIGI,*),YMOM1(*),ZMOM1(*),
     .   UVAR(NUVAR,*),DX(*),DY(*),DZ(*),RX(*),RY(*),RZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,JJ,K,PT
C-----------------------------------------------
C---
C     CONTRAINTES INITIALES + OTHERS
C---
      IF (INISPRI /= 0) THEN
C         
        DO I=LFT,LLT
          II = I+NFT
          JJ = PTSPRI(II)
          IF (JJ == 0) GOTO 200
C---
          FX(I)	  = SIGRS(2, JJ)
          DX(I)   = SIGRS(3, JJ)
          FY(I)   = SIGRS(4, JJ)
          DY(I)   = SIGRS(5, JJ)
          FZ(I)   = SIGRS(6, JJ)
          DZ(I)	  = SIGRS(7, JJ)
          XMOM(I) = SIGRS(8, JJ)
          RX(I)	  = SIGRS(9,JJ)
          YMOM(I) = SIGRS(10,JJ)
          RY(I)	  = SIGRS(11,JJ)
          ZMOM(I) = SIGRS(12,JJ)
          RZ(I)   = SIGRS(13,JJ)  
          EINT(I) = SIGRS(14,JJ)
C
          PT = 14
!!   no real need for initialisation, since they are computed at cycle = 0
!!   within engine (overwriting initial values)
!!          IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!            YMOM1(I) = SIGRS(PT+1,JJ)
!!            ZMOM1(I) = SIGRS(PT+2,JJ)
!!            PT = PT + 2
!!          ENDIF
C  -- UVAR --
          DO K=1,NUVAR
            UVAR(K,I) = SIGRS(PT + K ,JJ)
          ENDDO
C---
  200     CONTINUE            
        ENDDO ! DO I=LFT,LLT
      ENDIF ! IF (INISPRI /= 0)
C-----------
      RETURN
      END      
