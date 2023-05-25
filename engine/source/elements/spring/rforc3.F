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
Chd|  RFORC3                        source/elements/spring/rforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_RUSER             source/user_interface/dyn_userlib.c
Chd|        R1COOR3                       source/elements/spring/r1coor3.F
Chd|        R1CUM3                        source/elements/spring/r1cum3.F
Chd|        R1CUM3P                       source/elements/spring/r1cum3p.F
Chd|        R1DEF3                        source/elements/spring/r1def3.F
Chd|        R1LEN3                        source/elements/spring/r1len3.F
Chd|        R1SENS3                       source/elements/spring/r1sens3.F
Chd|        R1TORS                        source/elements/spring/r1tors.F
Chd|        R26DEF3                       source/elements/spring/r26def3.F
Chd|        R27DEF3                       source/elements/spring/r27def3.F
Chd|        R2COOR3                       source/elements/spring/r2coor3.F
Chd|        R2CUM3                        source/elements/spring/r2cum3.F
Chd|        R2CUM3P                       source/elements/spring/r2cum3p.F
Chd|        R2DEF3                        source/elements/spring/r2def3.F
Chd|        R2LEN3                        source/elements/spring/r2len3.F
Chd|        R2SENS3                       source/elements/spring/r2sens3.F
Chd|        R2TORS                        source/elements/spring/r2tors.F
Chd|        R3BILAN                       source/elements/spring/r3bilan.F
Chd|        R3COOR3                       source/elements/spring/r3coor3.F
Chd|        R3CUM3                        source/elements/spring/r3cum3.F
Chd|        R3CUM3P                       source/elements/spring/r3cum3p.F
Chd|        R3DEF3                        source/elements/spring/r3def3.F
Chd|        R3LEN3                        source/elements/spring/r3len3.F
Chd|        R3TORS                        source/elements/spring/r3tors.F
Chd|        R4COOR3                       source/elements/spring/r4coor3.F
Chd|        R4CUM3                        source/elements/spring/r4cum3.F
Chd|        R4CUM3P                       source/elements/spring/r4cum3p.F
Chd|        R4DEF3                        source/elements/spring/r4def3.F
Chd|        R4EVEC3                       source/elements/spring/r4evec3.F
Chd|        R4TORS                        source/elements/spring/r4tors.F
Chd|        R5BILAN                       source/elements/spring/r5bilan.F
Chd|        R5CUM3                        source/elements/spring/r5cum3.F
Chd|        R5CUM3P                       source/elements/spring/r5cum3p.F
Chd|        R5DEF3                        source/elements/spring/r5def3.F
Chd|        R5EVEC3                       source/elements/spring/r5evec3.F
Chd|        R5LEN3                        source/elements/spring/r5len3.F
Chd|        R6DEF3                        source/elements/spring/r6def3.F
Chd|        RBILAN                        source/elements/spring/rbilan.F
Chd|        RGJOINT                       source/elements/joint/rgjoint.F
Chd|        RSENS_NIC                     source/tools/sensor/rsens_nic.F
Chd|        RUSER32                       source/elements/spring/ruser32.F
Chd|        RUSER35                       source/elements/spring/ruser35.F
Chd|        RUSER36                       source/elements/spring/ruser36.F
Chd|        RUSER44                       source/elements/spring/ruser44.F
Chd|        RUSER46                       source/elements/spring/ruser46.F
Chd|        SET_SPRING_ELNUM              source/user_interface/uaccess.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE RFORC3(
     1   ELBUF_STR, JFT,       JLT,       NEL,
     2   MTN,       IGEO,      GEO,       IXR,
     3   X,         TABLE,     XDP,       F,
     4   NPF,       TF,        SKEW,      FLG_KJ2,
     5   VR,        AR,        V,         DT2T,
     6   NELTST,    ITYPTST,   STIFN,     STIFR,
     7   MS,        IN,        FSKY,      IADR,
     8   SENSORS,   OFFSET,    ANIM,      PARTSAV,
     9   IPARTR,    TANI,      FR_WAVE,   BUFMAT,
     A   BUFGEO,    PM,        RBY,       FX1,
     B   FX2,       FY1,       FY2,       FZ1,
     C   FZ2,       MX1,       MX2,       MY1,
     D   MY2,       MZ1,       MZ2,       GRESAV,
     E   GRTH,      IGRTH,     MSRT,      DMELRT,
     F   ITASK,     H3D_DATA,  JSMS,      NFT,
     G   IAD,       IGRE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE TABLE_MOD
      USE SENSOR_MOD
      USE H3D_MOD
      USE MESSAGE_MOD
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
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: IAD
      INTEGER, INTENT(IN) :: IGRE
      INTEGER, INTENT(IN) :: NFT
      INTEGER IXR(NIXR,*), NPF(*),IADR(3,*),IPARTR(*),
     .        IGEO(NPROPGI,*),JFT,JLT,NELTST ,ITYPTST,OFFSET,
     .        NEL,MTN,GRTH(*),IGRTH(*),FLG_KJ2,ITASK
      INTEGER, INTENT(IN) :: JSMS
      my_real DT2T ,
     .   GEO(NPROPG,*),X(*),F(*),TF(*),SKEW(LSKEW,*),FSKY(*),
     .   VR(*), V(*), AR(*), STIFN(*),STIFR(*),MS(*), IN(*),
     .   ANIM(*),PARTSAV(*),TANI(15,*),
     .   FR_WAVE(*),BUFMAT(*),BUFGEO(*),PM(*),RBY(*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),GRESAV(*),
     .   MSRT(*), DMELRT(*)
      DOUBLE PRECISION XDP(3,*)
      TYPE(TTABLE) TABLE(*)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSORS_) , INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(MVSIZ),MGN(MVSIZ),NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),IEQUIL(MVSIZ)
C     REAL
      my_real 
     .   STI(3,MVSIZ),STIR(3,MVSIZ),VISI(MVSIZ),VISIR(MVSIZ),
     .   USTI(MVSIZ),USTIR(MVSIZ),DF(MVSIZ),AL(MVSIZ),UNUSED(MVSIZ),
     .   UINER(MVSIZ),FR_W_E(MVSIZ),OFF(MVSIZ),BID
      my_real
     .   EXX2(MVSIZ), EYX2(MVSIZ), EZX2(MVSIZ),
     .   EXY2(MVSIZ), EYY2(MVSIZ), EZY2(MVSIZ),
     .   EXZ2(MVSIZ), EYZ2(MVSIZ), EZZ2(MVSIZ),
     .   AL2(MVSIZ),X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),
     .   X2(MVSIZ),Y2(MVSIZ),Z2(MVSIZ),X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),
     .   EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ),
     .   EXX(MVSIZ),EYX(MVSIZ),EZX(MVSIZ),
     .   EXY(MVSIZ),EYY(MVSIZ),EZY(MVSIZ),
     .   EXZ(MVSIZ),EYZ(MVSIZ),EZZ(MVSIZ),
     .   XCR(MVSIZ),XK(MVSIZ),XM(MVSIZ),XC(MVSIZ),RX1(MVSIZ),RX2(MVSIZ),
     .   RY1(MVSIZ),RY2(MVSIZ),RZ1(MVSIZ),RZ2(MVSIZ),XIN(MVSIZ),
     .   AK(MVSIZ),XKM(MVSIZ),XCM(MVSIZ),XKR(MVSIZ),
     .   EX2(MVSIZ),EY2(MVSIZ),EZ2(MVSIZ),VX1(MVSIZ),VX2(MVSIZ),
     .   VY1(MVSIZ),VY2(MVSIZ),VZ1(MVSIZ),VZ2(MVSIZ)
      INTEGER IGTYP,I,I0,NUVAR,NSENSOR
      DOUBLE PRECISION
     .   X1DP(3,MVSIZ),X2DP(3,MVSIZ),X3DP(3,MVSIZ),
     .   ELX(3,MVSIZ),AL2DP(MVSIZ),ALDP(MVSIZ)
      my_real USER_FX(MVSIZ),USER_FY(MVSIZ),USER_FZ(MVSIZ),
     *        USER_MOMX(MVSIZ),USER_MOMY(MVSIZ),USER_MOMZ(MVSIZ),
     *        USER_V_REPCVTX(MVSIZ),USER_V_REPCVTY(MVSIZ),USER_V_REPCVTZ(MVSIZ),
     *        USER_VR_REPCVTX(MVSIZ),USER_VR_REPCVTY(MVSIZ),USER_VR_REPCVTZ(MVSIZ),
     *        USER_EINT(MVSIZ)
      my_real, DIMENSION(:), ALLOCATABLE :: USER_UVAR
C-----------------------------------------------
      TYPE(G_BUFEL_),POINTER :: GBUF
      INTEGER II(6)
!
      CHARACTER OPTION*256
      INTEGER SIZE
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      NSENSOR = SENSORS%NSENSOR
!
      FX1(1:MVSIZ) = ZERO
      FX2(1:MVSIZ) = ZERO
      FY1(1:MVSIZ) = ZERO
      FY2(1:MVSIZ) = ZERO
      FZ1(1:MVSIZ) = ZERO
      FZ2(1:MVSIZ) = ZERO
      MX1(1:MVSIZ) = ZERO
      MX2(1:MVSIZ) = ZERO
      MY1(1:MVSIZ) = ZERO
      MY2(1:MVSIZ) = ZERO
      MZ1(1:MVSIZ) = ZERO
      MZ2(1:MVSIZ) = ZERO
!
      DO I=1,6
        II(I) = (I-1)*NEL + 1
      ENDDO
C
      I0 = IXR(1,1)
      IGTYP = IGEO(11,I0)
C
      BID = ZERO
C
      NUVAR =  NINT(GEO(25,I0))
C
      FR_W_E(1:NEL) = ZERO
C=======================================================================
      IF (IGTYP == 4) THEN
C=======================================================================
        CALL R1COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    NGL,     MGN,
     3   NC1,     NC2,     NEL)
        CALL R1SENS3(
     1   GEO,                GBUF%OFF,   SENSORS%SENSOR_TAB,         GBUF%TOTDEPL(II(1)),
     2   GBUF%LENGTH(II(1)), IGEO,               MGN,                NEL,NSENSOR)
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN) THEN
            OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
          ELSE
C        spring may be activated by sensor and is actually inactive.
            OFF(I)=ZERO
          ENDIF
        ENDDO
C
        CALL R1DEF3(
     1   GEO,                    GBUF%FOR(II(1)),        GBUF%LENGTH(II(1)),     GBUF%EINT,
     2   GBUF%TOTDEPL(II(1)),    NPF,                    TF,                     OFF,
     3   GBUF%DEP_IN_TENS(II(1)),GBUF%FOREP(II(1)),      GBUF%DEP_IN_COMP(II(1)),ANIM,
     4   GBUF%POSX,              FR_WAVE,                IGEO,                   GBUF%LENGTH_ERR,
     5   X1DP,                   X2DP,                   V,                      GBUF%YIELD(II(1)),
     6   NGL,                    MGN,                    EX,                     EY,
     7   EZ,                     XK,                     XM,                     XC,
     8   AK,                     NC1,                    NC2,                    NUVAR,
     9   GBUF%VAR,               GBUF%DEFINI,            NEL,                    NFT)
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R1LEN3(
     1   JFT,      JLT,      GBUF%OFF, DT2T,
     2   NELTST,   ITYPTST,  STI,      MS,
     3   MSRT,     DMELRT,   GBUF%G_DT,GBUF%DT,
     4   NGL,      XK,       XM,       XC,
     5   AK,       NC1,      NC2,      JSMS)
        CALL RBILAN(
     1   GBUF%EINT,         PARTSAV,           IXR,               GEO,
     2   V,                 IPARTR,            GBUF%LENGTH(II(1)),GRESAV,
     3   GRTH,              IGRTH,             GBUF%OFF,          NC1,
     4   NC2,               X,                 VR,                ITASK,
     5   NEL,               IGRE)
        CALL R1TORS(
     1   GBUF%FOR(II(1)),TANI,           H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R1CUM3(
     1   F,              GBUF%FOR(II(1)),STI,            STIFN,
     2   FX1,            FX2,            FY1,            FY2,
     3   FZ1,            FZ2,            MX1,            MX2,
     4   MY1,            MY2,            MZ1,            MZ2,
     5   EX,             EY,             EZ,             NC1,
     6   NC2,            NEL)
        ELSE
          CALL R1CUM3P(
     1   GBUF%FOR(II(1)),STI,            FSKY,           FSKY,
     2   IADR,           FX1,            FX2,            FY1,
     3   FY2,            FZ1,            FZ2,            MX1,
     4   MX2,            MY1,            MY2,            MZ1,
     5   MZ2,            EX,             EY,             EZ,
     6   NEL,            NFT)
       ENDIF

C=======================================================================
      ELSEIF (IGTYP == 26) THEN
C=======================================================================
        CALL R1COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    NGL,     MGN,
     3   NC1,     NC2,     NEL)
        CALL R1SENS3(
     1   GEO,                GBUF%OFF,   SENSORS%SENSOR_TAB,         GBUF%TOTDEPL(II(1)),
     2   GBUF%LENGTH(II(1)), IGEO,               MGN,                NEL      ,NSENSOR)
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /=  -TEN) THEN
            OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
          ELSE
C          spring may be activated by sensor and is actually inactive.
           OFF(I)=ZERO
          ENDIF
        ENDDO
C
        CALL R26DEF3(
     1   GBUF%FOR(II(1)),        GBUF%EINT,              GBUF%TOTDEPL(II(1)),    GBUF%LENGTH(II(1)),
     2   GBUF%DV,                GBUF%FOREP(II(1)),      GBUF%DEP_IN_COMP(II(1)),GBUF%POSX,
     3   GEO,                    IGEO,                   NPF,                    TF,
     4   V,                      OFF,                    ANIM,                   FR_WAVE,
     5   GBUF%LENGTH_ERR,        X1DP,                   X2DP,                   NGL,
     6   MGN,                    EX,                     EY,                     EZ,
     7   XK,                     XM,                     XC,                     AK,
     8   NEL,                    NFT,                    IAD,                    GBUF%RUPTCRIT(II(1)))
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R1LEN3(
     1   JFT,      JLT,      GBUF%OFF, DT2T,
     2   NELTST,   ITYPTST,  STI,      MS,
     3   MSRT,     DMELRT,   GBUF%G_DT,GBUF%DT,
     4   NGL,      XK,       XM,       XC,
     5   AK,       NC1,      NC2,      JSMS)
        CALL RBILAN(
     1   GBUF%EINT,         PARTSAV,           IXR,               GEO,
     2   V,                 IPARTR,            GBUF%LENGTH(II(1)),GRESAV,
     3   GRTH,              IGRTH,             GBUF%OFF,          NC1,
     4   NC2,               X,                 VR,                ITASK,
     5   NEL,               IGRE)
        CALL R1TORS(
     1   GBUF%FOR(II(1)),TANI,           H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R1CUM3(
     1   F,              GBUF%FOR(II(1)),STI,            STIFN,
     2   FX1,            FX2,            FY1,            FY2,
     3   FZ1,            FZ2,            MX1,            MX2,
     4   MY1,            MY2,            MZ1,            MZ2,
     5   EX,             EY,             EZ,             NC1,
     6   NC2,            NEL)
        ELSE
          CALL R1CUM3P(
     1   GBUF%FOR(II(1)),STI,            FSKY,           FSKY,
     2   IADR,           FX1,            FX2,            FY1,
     3   FY2,            FZ1,            FZ2,            MX1,
     4   MX2,            MY1,            MY2,            MZ1,
     5   MZ2,            EX,             EY,             EZ,
     6   NEL,            NFT)
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 27) THEN
C=======================================================================
        CALL R1COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    NGL,     MGN,
     3   NC1,     NC2,     NEL)
        CALL R1SENS3(
     1   GEO,                GBUF%OFF,   SENSORS%SENSOR_TAB,         GBUF%TOTDEPL(II(1)),
     2   GBUF%LENGTH(II(1)), IGEO,               MGN,                NEL       ,NSENSOR )
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN) THEN
            OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
          ELSE
C        spring may be activated by sensor and is actually inactive.
            OFF(I)=ZERO
          ENDIF
        ENDDO
C
        CALL R27DEF3(
     1   GBUF%FOR(II(1)),    GBUF%EINT,          GBUF%TOTDEPL(II(1)),GBUF%LENGTH(II(1)),
     2   GBUF%POSX,          GEO,                IGEO,               NPF,
     3   TF,                 V,                  OFF,                ANIM,
     4   GBUF%LENGTH_ERR,    X1DP,               X2DP,               NGL,
     5   MGN,                EX,                 EY,                 EZ,
     6   XK,                 XM,                 XC,                 AK,
     7   NEL,                NFT,                GBUF%RUPTCRIT(II(1)))
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R1LEN3(
     1   JFT,      JLT,      GBUF%OFF, DT2T,
     2   NELTST,   ITYPTST,  STI,      MS,
     3   MSRT,     DMELRT,   GBUF%G_DT,GBUF%DT,
     4   NGL,      XK,       XM,       XC,
     5   AK,       NC1,      NC2,      JSMS)
        CALL RBILAN(
     1   GBUF%EINT,         PARTSAV,           IXR,               GEO,
     2   V,                 IPARTR,            GBUF%LENGTH(II(1)),GRESAV,
     3   GRTH,              IGRTH,             GBUF%OFF,          NC1,
     4   NC2,               X,                 VR,                ITASK,
     5   NEL,               IGRE)
        CALL R1TORS(
     1   GBUF%FOR(II(1)),TANI,           H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R1CUM3(
     1   F,              GBUF%FOR(II(1)),STI,            STIFN,
     2   FX1,            FX2,            FY1,            FY2,
     3   FZ1,            FZ2,            MX1,            MX2,
     4   MY1,            MY2,            MZ1,            MZ2,
     5   EX,             EY,             EZ,             NC1,
     6   NC2,            NEL)
        ELSE
          CALL R1CUM3P(
     1   GBUF%FOR(II(1)),STI,            FSKY,           FSKY,
     2   IADR,           FX1,            FX2,            FY1,
     3   FY2,            FZ1,            FZ2,            MX1,
     4   MX2,            MY1,            MY2,            MZ1,
     5   MZ2,            EX,             EY,             EZ,
     6   NEL,            NFT)
       ENDIF
C=======================================================================
      ELSEIF (IGTYP == 8) THEN
C=======================================================================
        CALL R2COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    NGL,     X1,
     3   Y1,      Z1,      X2,      Y2,
     4   Z2,      MGN,     RX1,     RY1,
     5   RZ1,     RX2,     RY2,     RZ2,
     6   NC1,     NC2,     NEL)
        CALL R2SENS3(
     1   GEO,                GBUF%OFF,   SENSORS%SENSOR_TAB,         GBUF%TOTDEPL(II(1)),
     2   GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%LENGTH(II(1)), GBUF%LENGTH(II(2)),
     3   GBUF%LENGTH(II(3)), GBUF%TOTROT(II(1)), GBUF%TOTROT(II(2)), GBUF%TOTROT(II(3)),
     4   IGEO,               MGN,                NEL,              NSENSOR)
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN) THEN
            OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
          ELSE
C        spring may be activated by sensor and is actually inactive.
            OFF(I)=ZERO
          ENDIF
        ENDDO
C
        CALL R2DEF3(
     1   SKEW,                   GEO,                    GBUF%FOR(II(1)),        GBUF%FOR(II(2)),
     2   GBUF%FOR(II(3)),        GBUF%EINT,              GBUF%TOTDEPL(II(1)),    GBUF%TOTDEPL(II(2)),
     3   GBUF%TOTDEPL(II(3)),    NPF,                    TF,                     OFF,
     4   GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),GBUF%DEP_IN_COMP(II(1)),
     5   GBUF%DEP_IN_COMP(II(2)),GBUF%DEP_IN_COMP(II(3)),GBUF%FOREP(II(1)),      GBUF%FOREP(II(2)),
     6   GBUF%FOREP(II(3)),      GBUF%LENGTH(II(1)),     GBUF%LENGTH(II(2)),     GBUF%LENGTH(II(3)),
     7   GBUF%MOM(II(1)),        GBUF%MOM(II(2)),        GBUF%MOM(II(3)),        GBUF%TOTROT(II(1)),
     8   GBUF%TOTROT(II(2)),     GBUF%TOTROT(II(3)),     GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),
     9   GBUF%ROT_IN_TENS(II(3)),GBUF%MOMEP(II(1)),      GBUF%MOMEP(II(2)),      GBUF%MOMEP(II(3)),
     A   GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),ANIM,
     B   GBUF%POSX,              GBUF%POSY,              GBUF%POSZ,              GBUF%POSXX,
     C   GBUF%POSYY,             GBUF%POSZZ,             FR_WAVE,                V,
     D   IGEO,                   GBUF%E6,                GBUF%RUPTCRIT,          NEL,
     E   GBUF%LENGTH_ERR,        X1DP,                   X2DP,                   GBUF%YIELD(II(1)),
     F   GBUF%YIELD(II(2)),      GBUF%YIELD(II(3)),      GBUF%YIELD(II(4)),      GBUF%YIELD(II(5)),
     G   GBUF%YIELD(II(6)),      NGL,                    XKR,                    MGN,
     H   EXX,                    EYX,                    EZX,                    EXY,
     I   EYY,                    EZY,                    EXZ,                    EYZ,
     J   EZZ,                    XCR,                    RX1,                    RY1,
     K   RZ1,                    RX2,                    RY2,                    RZ2,
     L   XIN,                    AK,                     XM,                     XKM,
     M   XCM,                    NC1,                    NC2,                    NUVAR,
     N   GBUF%VAR,               GBUF%DEFINI(II(1)),     GBUF%DEFINI(II(2)),     GBUF%DEFINI(II(3)),
     O   GBUF%DEFINI(II(4)),     GBUF%DEFINI(II(5)),     GBUF%DEFINI(II(6)),     IEQUIL,
     P   GBUF%SKEW_ID,           NFT)
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
        
        CALL RSENS_NIC(NEL    ,IXR    ,GBUF%FOR,GBUF%MOM,SKEW  ,
     .                 NSENSOR,SENSORS%SENSOR_TAB)
C
        CALL R2LEN3(
     1   JFT,      JLT,      GBUF%OFF, DT2T,
     2   NELTST,   ITYPTST,  STI,      STIR,
     3   MS,       IN,       MSRT,     DMELRT,
     4   GBUF%G_DT,GBUF%DT,  NGL,      XCR,
     5   XIN,      XM,       XKM,      XCM,
     6   XKR,      NC1,      NC2,      JSMS)
        CALL RBILAN(
     1   GBUF%EINT,PARTSAV,  IXR,      GEO,
     2   V,        IPARTR,   BID,      GRESAV,
     3   GRTH,     IGRTH,    GBUF%OFF, NC1,
     4   NC2,      X,        VR,       ITASK,
     5   NEL,      IGRE)
        CALL R2TORS(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),TANI,           H3D_DATA,
     3   NEL)
        IF (IPARIT == 0) THEN
          CALL R2CUM3(
     1   F,              GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),
     2   AR,             GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     3   STI,            STIR,           STIFN,          STIFR,
     4   FX1,            FX2,            FY1,            FY2,
     5   FZ1,            FZ2,            MX1,            MX2,
     6   MY1,            MY2,            MZ1,            MZ2,
     7   GEO,            X1,             Y1,             Z1,
     8   X2,             Y2,             Z2,             IEQUIL,
     9   EXX,            EYX,            EZX,            EXY,
     A   EYY,            EZY,            EXZ,            EYZ,
     B   EZZ,            NC1,            NC2,            NEL)
        ELSE
          CALL R2CUM3P(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI,            STIR,
     3   FSKY,           FSKY,           IADR,           FX1,
     4   FX2,            FY1,            FY2,            FZ1,
     5   FZ2,            MX1,            MX2,            MY1,
     6   MY2,            MZ1,            MZ2,            GEO,
     7   X1,             Y1,             Z1,             X2,
     8   Y2,             Z2,             IEQUIL,         EXX,
     9   EYX,            EZX,            EXY,            EYY,
     A   EZY,            EXZ,            EYZ,            EZZ,
     B   NEL,            NFT)
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 12) THEN
C=======================================================================
        CALL R3COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    X3DP,    NGL,
     3   MGN,     NC1,     NC2,     NC3,
     4   NEL)
        CALL R1SENS3(
     1   GEO,                GBUF%OFF,   SENSORS%SENSOR_TAB,         GBUF%TOTDEPL(II(1)),
     2   GBUF%LENGTH(II(1)), IGEO,               MGN,                NEL,       NSENSOR )
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN) THEN
            OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
          ELSE
C        spring may be activated by sensor and is actually inactive.
            OFF(I)=ZERO
          ENDIF
        ENDDO
C
        CALL R3DEF3(
     1   GEO,                    GBUF%FOR(II(1)),        GBUF%LENGTH(II(1)),     GBUF%EINT,
     2   GBUF%TOTDEPL(II(1)),    NPF,                    TF,                     OFF,
     3   GBUF%DEP_IN_TENS(II(1)),GBUF%FOREP(II(1)),      GBUF%DEP_IN_COMP(II(1)),GBUF%DFS,
     4   V,                      IXR,                    DF,                     ANIM,
     5   GBUF%POSX,              FR_WAVE,                IGEO,                   GBUF%LENGTH_ERR,
     6   X1DP,                   X2DP,                   X3DP,                   GBUF%YIELD(II(1)),
     7   TABLE,                  GBUF%INIFRIC,           NGL,                    MGN,
     8   EX,                     EY,                     EZ,                     XK,
     9   XM,                     XC,                     AK,                     EX2,
     A   EY2,                    EZ2,                    NC1,                    NC2,
     B   NC3,                    NUVAR,                  GBUF%VAR,               GBUF%DEFINI,
     C   NEL,                    NFT)
c
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
        CALL R3LEN3(
     1   JFT,      JLT,      GBUF%OFF, DT2T,
     2   NELTST,   ITYPTST,  STI,      MS,
     3   MSRT,     DMELRT,   GBUF%G_DT,GBUF%DT,
     4   NGL,      XK,       XM,       XC,
     5   AK,       NC1,      NC2,      NC3,
     6   JSMS)
        CALL R3BILAN(
     1   GBUF%EINT,         PARTSAV,           IXR,               GEO,
     2   V,                 IPARTR,            GBUF%LENGTH(II(1)),GRESAV,
     3   GRTH,              IGRTH,             NC1,               NC2,
     4   NC3,               X,                 VR,                ITASK,
     5   NEL,               IGRE)
        CALL R3TORS(
     1   GBUF%FOR(II(1)),DF,             TANI,           H3D_DATA,
     2   NEL)
        IF (IPARIT == 0) THEN
          CALL R3CUM3(
     1   F,              GBUF%FOR(II(1)),STI,            STIFN,
     2   DF,             EX,             EY,             EZ,
     3   EX2,            EY2,            EZ2,            NC1,
     4   NC2,            NC3,            NEL)
        ELSE
          CALL R3CUM3P(
     1   GBUF%FOR(II(1)),STI,            FSKY,           FSKY,
     2   IADR,           DF,             EX,             EY,
     3   EZ,             EX2,            EY2,            EZ2,
     4   NEL,            NFT)
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 13) THEN
C=======================================================================
       CALL R2COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    NGL,     X1,
     3   Y1,      Z1,      X2,      Y2,
     4   Z2,      MGN,     RX1,     RY1,
     5   RZ1,     RX2,     RY2,     RZ2,
     6   NC1,     NC2,     NEL)
       CALL R2SENS3(
     1   GEO,                GBUF%OFF,   SENSORS%SENSOR_TAB,         GBUF%TOTDEPL(II(1)),
     2   GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%LENGTH(II(1)), GBUF%LENGTH(II(2)),
     3   GBUF%LENGTH(II(3)), GBUF%TOTROT(II(1)), GBUF%TOTROT(II(2)), GBUF%TOTROT(II(3)),
     4   IGEO,               MGN,                NEL               , NSENSOR            )
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN) THEN
            OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
          ELSE
C        spring may be activated by sensor and is actually inactive.
            OFF(I)=ZERO
          ENDIF
        ENDDO
C
        CALL R4EVEC3(
     1   GBUF%SKEW,    V,            EXX2,         EYX2,
     2   EZX2,         EXY2,         EYY2,         EZY2,
     3   EXZ2,         EYZ2,         EZZ2,         AL2DP,
     4   X1DP,         X2DP,         AL2,          ALDP,
     5   GBUF%SKEW_ERR,NGL,          AL,           EXX,
     6   EYX,          EZX,          EXY,          EYY,
     7   EZY,          EXZ,          EYZ,          EZZ,
     8   RX1,          RY1,          RZ1,          RX2,
     9   RY2,          RZ2,          VX1,          VX2,
     A   VY1,          VY2,          VZ1,          VZ2,
     B   NC1,          NC2,          NEL)
        CALL R4DEF3(
     1   SKEW,                   GEO,                    GBUF%FOR(II(1)),        GBUF%FOR(II(2)),
     2   GBUF%FOR(II(3)),        GBUF%EINT,              GBUF%TOTDEPL(II(1)),    GBUF%TOTDEPL(II(2)),
     3   GBUF%TOTDEPL(II(3)),    NPF,                    TF,                     OFF,
     4   GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),GBUF%DEP_IN_COMP(II(1)),
     5   GBUF%DEP_IN_COMP(II(2)),GBUF%DEP_IN_COMP(II(3)),GBUF%FOREP(II(1)),      GBUF%FOREP(II(2)),
     6   GBUF%FOREP(II(3)),      GBUF%LENGTH(II(1)),     GBUF%LENGTH(II(2)),     GBUF%LENGTH(II(3)),
     7   GBUF%MOM(II(1)),        GBUF%MOM(II(2)),        GBUF%MOM(II(3)),        GBUF%TOTROT(II(1)),
     8   GBUF%TOTROT(II(2)),     GBUF%TOTROT(II(3)),     GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),
     9   GBUF%ROT_IN_TENS(II(3)),GBUF%MOMEP(II(1)),      GBUF%MOMEP(II(2)),      GBUF%MOMEP(II(3)),
     A   GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),ANIM,
     B   GBUF%POSX,              GBUF%POSY,              GBUF%POSZ,              GBUF%POSXX,
     C   GBUF%POSYY,             GBUF%POSZZ,             FR_WAVE,                GBUF%E6,
     D   NEL,                    EXX2,                   EYX2,                   EZX2,
     E   EXY2,                   EYY2,                   EZY2,                   EXZ2,
     F   EYZ2,                   EZZ2,                   AL2DP,                  IGEO,
     G   GBUF%RUPTCRIT,          GBUF%LENGTH_ERR,        ALDP,                   GBUF%YIELD(II(1)),
     H   GBUF%YIELD(II(2)),      GBUF%YIELD(II(3)),      GBUF%YIELD(II(4)),      GBUF%YIELD(II(5)),
     I   GBUF%YIELD(II(6)),      NGL,                    MGN,                    EXX,
     J   EYX,                    EZX,                    EXY,                    EYY,
     K   EZY,                    EXZ,                    EYZ,                    EZZ,
     L   XCR,                    RX1,                    RY1,                    RZ1,
     M   RX2,                    RY2,                    RZ2,                    XIN,
     N   AK,                     XM,                     XKM,                    XCM,
     O   XKR,                    VX1,                    VX2,                    VY1,
     P   VY2,                    VZ1,                    VZ2,                    NUVAR,
     Q   GBUF%VAR,               GBUF%DEFINI(II(1)),     GBUF%DEFINI(II(2)),     GBUF%DEFINI(II(3)),
     R   GBUF%DEFINI(II(4)),     GBUF%DEFINI(II(5)),     GBUF%DEFINI(II(6)),     GBUF%FORINI(II(1)),
     S   GBUF%FORINI(II(2)),     GBUF%FORINI(II(3)),     GBUF%FORINI(II(4)),     GBUF%FORINI(II(5)),
     T   GBUF%FORINI(II(6)),     NFT)
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
        CALL R2LEN3(
     1   JFT,      JLT,      GBUF%OFF, DT2T,
     2   NELTST,   ITYPTST,  STI,      STIR,
     3   MS,       IN,       MSRT,     DMELRT,
     4   GBUF%G_DT,GBUF%DT,  NGL,      XCR,
     5   XIN,      XM,       XKM,      XCM,
     6   XKR,      NC1,      NC2,      JSMS)
        CALL RBILAN(
     1   GBUF%EINT,         PARTSAV,           IXR,               GEO,
     2   V,                 IPARTR,            GBUF%LENGTH(II(1)),GRESAV,
     3   GRTH,              IGRTH,             GBUF%OFF,          NC1,
     4   NC2,               X,                 VR,                ITASK,
     5   NEL,               IGRE)
        CALL R4TORS(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),TANI,           AL,
     3   H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R4CUM3(
     1   F,              GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),
     2   AR,             GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     3   STI,            STIR,           STIFN,          STIFR,
     4   FX1,            FX2,            FY1,            FY2,
     5   FZ1,            FZ2,            MX1,            MX2,
     6   MY1,            MY2,            MZ1,            MZ2,
     7   AL,             EXX,            EYX,            EZX,
     8   EXY,            EYY,            EZY,            EXZ,
     9   EYZ,            EZZ,            NC1,            NC2,
     A   NEL)
        ELSE
          CALL R4CUM3P(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI,            STIR,
     3   FSKY,           FSKY,           IADR,           FX1,
     4   FX2,            FY1,            FY2,            FZ1,
     5   FZ2,            MX1,            MX2,            MY1,
     6   MY2,            MZ1,            MZ2,            EXX,
     7   EYX,            EZX,            EXY,            EYY,
     8   EZY,            EXZ,            EYZ,            EZZ,
     9   AL,             NEL,            NFT)
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 25) THEN
C=======================================================================
        CALL R2COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    NGL,     X1,
     3   Y1,      Z1,      X2,      Y2,
     4   Z2,      MGN,     RX1,     RY1,
     5   RZ1,     RX2,     RY2,     RZ2,
     6   NC1,     NC2,     NEL)
        CALL R2SENS3(
     1   GEO,                GBUF%OFF,   SENSORS%SENSOR_TAB,         GBUF%TOTDEPL(II(1)),
     2   GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%LENGTH(II(1)), GBUF%LENGTH(II(2)),
     3   GBUF%LENGTH(II(3)), GBUF%TOTROT(II(1)), GBUF%TOTROT(II(2)), GBUF%TOTROT(II(3)),
     4   IGEO,               MGN,                NEL               , NSENSOR           )
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN) THEN
            OFF(I) = MIN(ONE,ABS(GBUF%OFF(I)))
          ELSE
c          spring may be activated by sensor and is actually inactive.
            OFF(I) = ZERO
          ENDIF
        ENDDO
C
        CALL R4EVEC3(
     1   GBUF%SKEW,    V,            EXX2,         EYX2,
     2   EZX2,         EXY2,         EYY2,         EZY2,
     3   EXZ2,         EYZ2,         EZZ2,         AL2DP,
     4   X1DP,         X2DP,         AL2,          ALDP,
     5   GBUF%SKEW_ERR,NGL,          AL,           EXX,
     6   EYX,          EZX,          EXY,          EYY,
     7   EZY,          EXZ,          EYZ,          EZZ,
     8   RX1,          RY1,          RZ1,          RX2,
     9   RY2,          RZ2,          VX1,          VX2,
     A   VY1,          VY2,          VZ1,          VZ2,
     B   NC1,          NC2,          NEL)
        CALL R6DEF3(
     1   SKEW,                   GEO,                    GBUF%FOR(II(1)),        GBUF%FOR(II(2)),
     2   GBUF%FOR(II(3)),        GBUF%EINT,              GBUF%TOTDEPL(II(1)),    GBUF%TOTDEPL(II(2)),
     3   GBUF%TOTDEPL(II(3)),    NPF,                    TF,                     GBUF%OFF,
     4   GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),GBUF%DEP_IN_COMP(II(1)),
     5   GBUF%DEP_IN_COMP(II(2)),GBUF%DEP_IN_COMP(II(3)),GBUF%FOREP(II(1)),      GBUF%FOREP(II(2)),
     6   GBUF%FOREP(II(3)),      GBUF%LENGTH(II(1)),     GBUF%LENGTH(II(2)),     GBUF%LENGTH(II(3)),
     7   GBUF%MOM(II(1)),        GBUF%MOM(II(2)),        GBUF%MOM(II(3)),        GBUF%TOTROT(II(1)),
     8   GBUF%TOTROT(II(2)),     GBUF%TOTROT(II(3)),     GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),
     9   GBUF%ROT_IN_TENS(II(3)),GBUF%MOMEP(II(1)),      GBUF%MOMEP(II(2)),      GBUF%MOMEP(II(3)),
     A   GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),ANIM,
     B   GBUF%POSX,              GBUF%POSY,              GBUF%POSZ,              GBUF%POSXX,
     C   GBUF%POSYY,             GBUF%POSZZ,             FR_WAVE,                GBUF%E6,
     D   NEL,                    AL2DP,                  EXX2,                   EYX2,
     E   EZX2,                   EXY2,                   EYY2,                   EZY2,
     F   EXZ2,                   EYZ2,                   EZZ2,                   IGEO,
     G   GBUF%LENGTH_ERR,        ALDP,                   GBUF%YIELD(II(1)),      GBUF%YIELD(II(2)),
     H   GBUF%YIELD(II(3)),      GBUF%YIELD(II(4)),      NGL,                    MGN,
     I   XCR,                    RX1,                    RY1,                    RZ1,
     J   RX2,                    RY2,                    RZ2,                    XIN,
     K   AK,                     XM,                     XKM,                    XCM,
     L   XKR,                    VX1,                    VX2,                    VY1,
     M   VY2,                    VZ1,                    VZ2,                    NUVAR,
     N   GBUF%VAR,               GBUF%DEFINI(II(1)),     GBUF%DEFINI(II(2)),     GBUF%DEFINI(II(3)),
     O   GBUF%DEFINI(II(4)),     GBUF%DEFINI(II(5)),     GBUF%DEFINI(II(6)),     GBUF%FORINI(II(1)),
     P   GBUF%FORINI(II(2)),     GBUF%FORINI(II(3)),     GBUF%FORINI(II(4)),     GBUF%FORINI(II(5)),
     Q   GBUF%FORINI(II(6)),     GBUF%RUPTCRIT,          NFT)
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R2LEN3(
     1   JFT,      JLT,      GBUF%OFF, DT2T,
     2   NELTST,   ITYPTST,  STI,      STIR,
     3   MS,       IN,       MSRT,     DMELRT,
     4   GBUF%G_DT,GBUF%DT,  NGL,      XCR,
     5   XIN,      XM,       XKM,      XCM,
     6   XKR,      NC1,      NC2,      JSMS)
        CALL RBILAN(
     1   GBUF%EINT,         PARTSAV,           IXR,               GEO,
     2   V,                 IPARTR,            GBUF%LENGTH(II(1)),GRESAV,
     3   GRTH,              IGRTH,             GBUF%OFF,          NC1,
     4   NC2,               X,                 VR,                ITASK,
     5   NEL,               IGRE)
        CALL R4TORS(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),TANI,           AL,
     3   H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R4CUM3(
     1   F,              GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),
     2   AR,             GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     3   STI,            STIR,           STIFN,          STIFR,
     4   FX1,            FX2,            FY1,            FY2,
     5   FZ1,            FZ2,            MX1,            MX2,
     6   MY1,            MY2,            MZ1,            MZ2,
     7   AL,             EXX,            EYX,            EZX,
     8   EXY,            EYY,            EZY,            EXZ,
     9   EYZ,            EZZ,            NC1,            NC2,
     A   NEL)
        ELSE
          CALL R4CUM3P(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI,            STIR,
     3   FSKY,           FSKY,           IADR,           FX1,
     4   FX2,            FY1,            FY2,            FZ1,
     5   FZ2,            MX1,            MX2,            MY1,
     6   MY2,            MZ1,            MZ2,            EXX,
     7   EYX,            EZX,            EXY,            EYY,
     8   EZY,            EXZ,            EYZ,            EZZ,
     9   AL,             NEL,            NFT)
        ENDIF
C=======================================================================
      ELSEIF (IGTYP >= 29 .AND. IGTYP <= 31) THEN
C=======================================================================
        CALL R4COOR3(
     1   X,        VR,       IXR,      GBUF%SKEW,
     2   NGL,      X1,       Y1,       Z1,
     3   X2,       Y2,       Z2,       MGN,
     4   RX1,      RY1,      RZ1,      RX2,
     5   RY2,      RZ2,      NC1,      NC2,
     6   NEL)
        CALL R5EVEC3(
     1   GBUF%SKEW,V,        NGL,      AL,
     2   X1,       Y1,       Z1,       X2,
     3   Y2,       Z2,       EXX,      EYX,
     4   EZX,      EXY,      EYY,      EZY,
     5   EXZ,      EYZ,      EZZ,      RX1,
     6   RY1,      RZ1,      RX2,      RY2,
     7   RZ2,      VX1,      VX2,      VY1,
     8   VY2,      VZ1,      VZ2,      NC1,
     9   NC2,      NEL)
        CALL R5DEF3(
     1   AL,                    GBUF%V_REPCVT(II(1):), GBUF%V_REPCVT(II(2):), GBUF%V_REPCVT(II(3):),
     2   GBUF%VR_REPCVT(II(1):),GBUF%VR_REPCVT(II(2):),GBUF%VR_REPCVT(II(3):),FR_WAVE,
     3   FR_W_E,                GBUF%EINT,             GBUF%FOR(II(1):),      GBUF%MOM(II(1):),
     4   GBUF%MOM(II(2):),      GBUF%MOM(II(3):),      GBUF%FOR(II(2):),      GBUF%FOR(II(3):),
     5   PARTSAV,               IPARTR,                EXX,                   EYX,
     6   EZX,                   EXY,                   EYY,                   EZY,
     7   EXZ,                   EYZ,                   EZZ,                   RX1,
     8   RY1,                   RZ1,                   RX2,                   RY2,
     9   RZ2,                   VX1,                   VX2,                   VY1,
     A   VY2,                   VZ1,                   VZ2,                   NC1,
     B   NC2,                   NEL)
C
        DO I=JFT,JLT
          OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
        ENDDO
C
        CALL SET_SPRING_ELNUM(JFT,JLT,IXR)
C
        IF( IGTYP==29 .OR. IGTYP==30 .OR. IGTYP==31)THEN
          DO I=1,NEL
             USER_EINT(I) = GBUF%EINT(I)

             USER_FX(I) = GBUF%FOR(II(1)+I-1)   
             USER_FY(I) = GBUF%FOR(II(2)+I-1)   
             USER_FZ(I) = GBUF%FOR(II(3)+I-1)
C
             USER_MOMX(I) = GBUF%MOM(II(1)+I-1)
             USER_MOMY(I) = GBUF%MOM(II(2)+I-1)
             USER_MOMZ(I) = GBUF%MOM(II(3)+I-1)

             USER_V_REPCVTX(I) = GBUF%V_REPCVT(II(1)+I-1)
             USER_V_REPCVTY(I) = GBUF%V_REPCVT(II(2)+I-1)
             USER_V_REPCVTZ(I) = GBUF%V_REPCVT(II(3)+I-1)

             USER_VR_REPCVTX(I) = GBUF%VR_REPCVT(II(1)+I-1)
             USER_VR_REPCVTY(I) = GBUF%VR_REPCVT(II(2)+I-1)
             USER_VR_REPCVTZ(I) = GBUF%VR_REPCVT(II(3)+I-1)
          ENDDO
          ALLOCATE(USER_UVAR(NUVAR*NEL))
          USER_UVAR(1:NUVAR*NEL)=GBUF%VAR(1:NUVAR*NEL)
        ENDIF


        IF (IGTYP == 29) THEN     

          IF (USERL_AVAIL>0) THEN
            CALL ENG_USERLIB_RUSER(IGTYP,
     1                 NEL                  ,I0                   ,USER_UVAR           ,NUVAR               ,
     2                 USER_FX              ,USER_FY              ,USER_FZ             ,USER_MOMX           ,USER_MOMY      ,
     3                 USER_MOMZ            ,USER_EINT            ,OFF                 ,USTI                ,USTIR          ,
     4                 VISI                 ,VISIR                ,UNUSED              ,UINER               ,DT1            ,
     5                 AL                   ,USER_V_REPCVTX       ,USER_V_REPCVTY      ,USER_V_REPCVTZ      ,USER_VR_REPCVTX,
     6                 USER_VR_REPCVTY      ,USER_VR_REPCVTZ      ,FR_W_E              )
          ELSE
            ! ----------------
            ! ERROR to be printed & exit
            OPTION='PROP/USER1 - SPRING'
            SIZE=LEN_TRIM(OPTION)
            CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
            CALL ARRET(2)
            ! ----------------
          ENDIF
        ELSEIF (IGTYP == 30) THEN
          IF (USERL_AVAIL>0) THEN
            CALL ENG_USERLIB_RUSER(IGTYP,
     1              NEL                  ,I0                   ,USER_UVAR           ,NUVAR               ,
     2              USER_FX              ,USER_FY              ,USER_FZ             ,USER_MOMX           ,USER_MOMY      ,
     3              USER_MOMZ            ,USER_EINT            ,OFF                 ,USTI                ,USTIR          ,
     4              VISI                 ,VISIR                ,UNUSED              ,UINER               ,DT1            ,
     5              AL                   ,USER_V_REPCVTX       ,USER_V_REPCVTY      ,USER_V_REPCVTZ      ,USER_VR_REPCVTX,
     6              USER_VR_REPCVTY      ,USER_VR_REPCVTZ      ,FR_W_E              )
          ELSE
            ! ----------------
            ! ERROR to be printed & exit
            OPTION='PROP/USER2 - SPRING'
            SIZE=LEN_TRIM(OPTION)
            CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
            CALL ARRET(2)
            ! ----------------
          ENDIF
        ELSEIF (IGTYP == 31) THEN
          IF (USERL_AVAIL>0) THEN
            CALL ENG_USERLIB_RUSER(IGTYP,
     1               NEL                  ,I0                   ,USER_UVAR           ,NUVAR               ,
     2               USER_FX              ,USER_FY              ,USER_FZ             ,USER_MOMX           ,USER_MOMY,
     3               USER_MOMZ            ,USER_EINT            ,OFF                 ,USTI                ,USTIR,
     4               VISI                 ,VISIR                ,UNUSED              ,UINER               ,DT1,
     5               AL                   ,USER_V_REPCVTX       ,USER_V_REPCVTY      ,USER_V_REPCVTZ      ,USER_VR_REPCVTX,
     6               USER_VR_REPCVTY      ,USER_VR_REPCVTZ      ,FR_W_E              )
          ELSE
            ! ----------------
            ! ERROR to be printed & exit
            OPTION='PROP/USER3 - SPRING'
            SIZE=LEN_TRIM(OPTION)
            CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
            CALL ARRET(2)
            ! ----------------
          ENDIF
        ENDIF


        IF( IGTYP==29 .OR. IGTYP==30 .OR. IGTYP==31)THEN
          DO I=1,NEL
             GBUF%EINT(I) = USER_EINT(I)

             GBUF%FOR(II(1)+I-1) = USER_FX(I)
             GBUF%FOR(II(2)+I-1) = USER_FY(I)
             GBUF%FOR(II(3)+I-1) = USER_FZ(I)
C
             GBUF%MOM(II(1)+I-1) = USER_MOMX(I)
             GBUF%MOM(II(2)+I-1) = USER_MOMY(I)
             GBUF%MOM(II(3)+I-1) = USER_MOMZ(I)

             GBUF%V_REPCVT(II(1)+I-1) = USER_V_REPCVTX(I)
             GBUF%V_REPCVT(II(2)+I-1) = USER_V_REPCVTY(I)
             GBUF%V_REPCVT(II(3)+I-1) = USER_V_REPCVTZ(I)

             GBUF%VR_REPCVT(II(1)+I-1) = USER_VR_REPCVTX(I)
             GBUF%VR_REPCVT(II(2)+I-1) = USER_VR_REPCVTY(I)
             GBUF%VR_REPCVT(II(3)+I-1) = USER_VR_REPCVTZ(I)
          ENDDO
          GBUF%VAR(1:NUVAR*NEL) = USER_UVAR(1:NUVAR*NEL)
          DEALLOCATE(USER_UVAR)
        ENDIF
C
        DO I=JFT,JLT
          IF (OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R5LEN3(
     1   JFT,                  JLT,                  GBUF%OFF,             DT2T,
     2   NELTST,               ITYPTST,              STI,                  STIR,
     3   MS,                   IN,                   USTI,                 USTIR,
     4   VISI,                 VISIR,                GBUF%MASS,            UINER,
     5   FR_WAVE,              FR_W_E,               GBUF%EINT,            GBUF%FOR(II(1)),
     6   GBUF%MOM(II(1)),      GBUF%MOM(II(2)),      GBUF%MOM(II(3)),      GBUF%V_REPCVT(II(1)),
     7   GBUF%V_REPCVT(II(2)), GBUF%V_REPCVT(II(3)), GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     8   GBUF%VR_REPCVT(II(3)),AL,                   GBUF%FOR(II(2)),      GBUF%FOR(II(3)),
     9   PARTSAV,              IPARTR,               MSRT,                 DMELRT,
     A   GBUF%G_DT,            GBUF%DT,              NGL,                  NC1,
     B   NC2,                  JSMS)
     
        CALL R5BILAN(
     1   GBUF%EINT,PARTSAV,  IXR,      GBUF%MASS,
     2   V,        IPARTR,   UINER,    X,
     3   VR,       GRESAV,   GRTH,     IGRTH,
     4   NC1,      NC2,      ITASK,    IAD,
     5   IGRE,     NFT,      NEL)
        CALL R4TORS(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),TANI,           AL,
     3   H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R5CUM3(
     1   F,              GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),
     2   AR,             GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     3   STI,            STIR,           STIFN,          STIFR,
     4   FX1,            FX2,            FY1,            FY2,
     5   FZ1,            FZ2,            MX1,            MX2,
     6   MY1,            MY2,            MZ1,            MZ2,
     7   GBUF%MOM(II(4)),GBUF%MOM(II(5)),AL,             EXX,
     8   EYX,            EZX,            EXY,            EYY,
     9   EZY,            EXZ,            EYZ,            EZZ,
     A   NC1,            NC2,            NEL)
        ELSE
          CALL R5CUM3P(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI,            STIR,
     3   FSKY,           FSKY,           IADR,           FX1,
     4   FX2,            FY1,            FY2,            FZ1,
     5   FZ2,            MX1,            MX2,            MY1,
     6   MY2,            MZ1,            MZ2,            GBUF%MOM(II(4)),
     7   GBUF%MOM(II(5)),EXX,            EYX,            EZX,
     8   EXY,            EYY,            EZY,            EXZ,
     9   EYZ,            EZZ,            AL,             NEL,
     A   NFT)
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 32) THEN
C=======================================================================
        CALL R2COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    NGL,     X1,
     3   Y1,      Z1,      X2,      Y2,
     4   Z2,      MGN,     RX1,     RY1,
     5   RZ1,     RX2,     RY2,     RZ2,
     6   NC1,     NC2,     NEL)
        CALL R4EVEC3(
     1   GBUF%SKEW,    V,            EXX2,         EYX2,
     2   EZX2,         EXY2,         EYY2,         EZY2,
     3   EXZ2,         EYZ2,         EZZ2,         AL2DP,
     4   X1DP,         X2DP,         AL2,          ALDP,
     5   GBUF%SKEW_ERR,NGL,          AL,           EXX,
     6   EYX,          EZX,          EXY,          EYY,
     7   EZY,          EXZ,          EYZ,          EZZ,
     8   RX1,          RY1,          RZ1,          RX2,
     9   RY2,          RZ2,          VX1,          VX2,
     A   VY1,          VY2,          VZ1,          VZ2,
     B   NC1,          NC2,          NEL)
        CALL R5DEF3(
     1   AL,                   GBUF%V_REPCVT(II(1)), GBUF%V_REPCVT(II(2)), GBUF%V_REPCVT(II(3)),
     2   GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_WAVE,
     3   FR_W_E,               GBUF%EINT,            GBUF%FOR(II(1)),      GBUF%MOM(II(1)),
     4   GBUF%MOM(II(2)),      GBUF%MOM(II(3)),      GBUF%FOR(II(2)),      GBUF%FOR(II(3)),
     5   PARTSAV,              IPARTR,               EXX,                  EYX,
     6   EZX,                  EXY,                  EYY,                  EZY,
     7   EXZ,                  EYZ,                  EZZ,                  RX1,
     8   RY1,                  RZ1,                  RX2,                  RY2,
     9   RZ2,                  VX1,                  VX2,                  VY1,
     A   VY2,                  VZ1,                  VZ2,                  NC1,
     B   NC2,                  NEL)
C
        NUVAR =  NINT(GEO(25,I0))
        DO I=JFT,JLT
          OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
        ENDDO
C
        CALL RUSER32(
     1 NEL                  ,IOUT                 ,I0                  ,GBUF%VAR            ,NUVAR                ,
     2 GBUF%FOR(II(1))      ,GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,
     3 GBUF%MOM(II(3))      ,GBUF%EINT            ,OFF                 ,USTI                ,USTIR                ,
     4 VISI                 ,VISIR                ,UNUSED              ,UINER               ,DT1                  ,
     5 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),
     6 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_W_E              ,NSENSOR,SENSORS%SENSOR_TAB          )
C
        DO I=JFT,JLT
         IF (OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R5LEN3(
     1   JFT,                  JLT,                  GBUF%OFF,             DT2T,
     2   NELTST,               ITYPTST,              STI,                  STIR,
     3   MS,                   IN,                   USTI,                 USTIR,
     4   VISI,                 VISIR,                GBUF%MASS,            UINER,
     5   FR_WAVE,              FR_W_E,               GBUF%EINT,            GBUF%FOR(II(1)),
     6   GBUF%MOM(II(1)),      GBUF%MOM(II(2)),      GBUF%MOM(II(3)),      GBUF%V_REPCVT(II(1)),
     7   GBUF%V_REPCVT(II(2)), GBUF%V_REPCVT(II(3)), GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     8   GBUF%VR_REPCVT(II(3)),AL,                   GBUF%FOR(II(2)),      GBUF%FOR(II(3)),
     9   PARTSAV,              IPARTR,               MSRT,                 DMELRT,
     A   GBUF%G_DT,            GBUF%DT,              NGL,                  NC1,
     B   NC2,                  JSMS)

        CALL R5BILAN(
     1   GBUF%EINT,PARTSAV,  IXR,      GBUF%MASS,
     2   V,        IPARTR,   UINER,    X,
     3   VR,       GRESAV,   GRTH,     IGRTH,
     4   NC1,      NC2,      ITASK,    IAD,
     5   IGRE,     NFT,      NEL)
        CALL R4TORS(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),TANI,           AL,
     3   H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R4CUM3(
     1   F,              GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),
     2   AR,             GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     3   STI,            STIR,           STIFN,          STIFR,
     4   FX1,            FX2,            FY1,            FY2,
     5   FZ1,            FZ2,            MX1,            MX2,
     6   MY1,            MY2,            MZ1,            MZ2,
     7   AL,             EXX,            EYX,            EZX,
     8   EXY,            EYY,            EZY,            EXZ,
     9   EYZ,            EZZ,            NC1,            NC2,
     A   NEL)
        ELSE
          CALL R4CUM3P(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI,            STIR,
     3   FSKY,           FSKY,           IADR,           FX1,
     4   FX2,            FY1,            FY2,            FZ1,
     5   FZ2,            MX1,            MX2,            MY1,
     6   MY2,            MZ1,            MZ2,            EXX,
     7   EYX,            EZX,            EXY,            EYY,
     8   EZY,            EXZ,            EYZ,            EZZ,
     9   AL,             NEL,            NFT)
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 33 .OR. IGTYP == 45) THEN
C=======================================================================
C
        NUVAR  = NINT(GEO(25,I0))
	       IF (IGTYP == 45) FLG_KJ2 = 1
C
        CALL RGJOINT(
     1   JFT,                JLT,                IPARTR,             NEL,
     2   NUVAR,              IOUT,               I0,                 IADR,
     3   FSKY,               FSKY,               GBUF%OFF,           GBUF%FOR(II(1)),
     4   GBUF%FOR(II(2)),    GBUF%FOR(II(3)),    GBUF%EINT,          GBUF%TOTDEPL(II(1)),
     5   GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%TOTROT(II(1)), GBUF%TOTROT(II(2)),
     6   GBUF%TOTROT(II(3)), GBUF%MOM(II(1)),    GBUF%MOM(II(2)),    GBUF%MOM(II(3)),
     7   GBUF%VAR,           STIFN,              STIFR,              IXR,
     8   TANI,               RBY,                X,                  V,
     9   VR,                 STI,                STIR,               MS,
     A   IN,                 PARTSAV,            DT1,                DT2T,
     B   F,                  AR,                 FX1,                FX2,
     C   FY1,                FY2,                FZ1,                FZ2,
     D   MX1,                MX2,                MY1,                MY2,
     E   MZ1,                MZ2,                GRESAV,             GRTH,
     F   IGRTH,              MSRT,               DMELRT,             NELTST,
     G   ITYPTST,            IGTYP,              SENSORS%SENSOR_TAB, NC1,
     H   NC2,                XDP,                GBUF%LENGTH_ERR,    H3D_DATA,
     I   JSMS,               IGRE,               NFT,                NSENSOR )
C=======================================================================
      ELSEIF (IGTYP == 35 .OR. IGTYP == 36) THEN
C=======================================================================
        CALL R4COOR3(
     1   X,        VR,       IXR,      GBUF%SKEW,
     2   NGL,      X1,       Y1,       Z1,
     3   X2,       Y2,       Z2,       MGN,
     4   RX1,      RY1,      RZ1,      RX2,
     5   RY2,      RZ2,      NC1,      NC2,
     6   NEL)
        CALL R5EVEC3(
     1   GBUF%SKEW,V,        NGL,      AL,
     2   X1,       Y1,       Z1,       X2,
     3   Y2,       Z2,       EXX,      EYX,
     4   EZX,      EXY,      EYY,      EZY,
     5   EXZ,      EYZ,      EZZ,      RX1,
     6   RY1,      RZ1,      RX2,      RY2,
     7   RZ2,      VX1,      VX2,      VY1,
     8   VY2,      VZ1,      VZ2,      NC1,
     9   NC2,      NEL)
        CALL R5DEF3(
     1   AL,                   GBUF%V_REPCVT(II(1)), GBUF%V_REPCVT(II(2)), GBUF%V_REPCVT(II(3)),
     2   GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_WAVE,
     3   FR_W_E,               GBUF%EINT,            GBUF%FOR(II(1)),      GBUF%MOM(II(1)),
     4   GBUF%MOM(II(2)),      GBUF%MOM(II(3)),      GBUF%FOR(II(2)),      GBUF%FOR(II(3)),
     5   PARTSAV,              IPARTR,               EXX,                  EYX,
     6   EZX,                  EXY,                  EYY,                  EZY,
     7   EXZ,                  EYZ,                  EZZ,                  RX1,
     8   RY1,                  RZ1,                  RX2,                  RY2,
     9   RZ2,                  VX1,                  VX2,                  VY1,
     A   VY2,                  VZ1,                  VZ2,                  NC1,
     B   NC2,                  NEL)
C
        NUVAR = NINT(GEO(25,I0))
        DO I=JFT,JLT
          OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
        ENDDO
C
        IF (IGTYP == 35) THEN
          CALL RUSER35(
     1 NEL                  ,IOUT                 ,I0                  ,GBUF%VAR            ,NUVAR                ,
     2 GBUF%FOR(II(1))      ,GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,
     3 GBUF%MOM(II(3))      ,GBUF%EINT            ,OFF                 ,USTI                ,USTIR                ,
     4 VISI                 ,VISIR                ,UNUSED              ,UINER               ,DT1                  ,
     5 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),
     6 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_W_E              )
        ELSEIF (IGTYP == 36) THEN
          CALL RUSER36(
     1 NEL                  ,I0                   ,GBUF%VAR            ,NUVAR               ,FR_W_E               ,
     2 GBUF%FOR(II(1))      ,GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,
     3 GBUF%MOM(II(3))      ,GBUF%EINT            ,OFF                 ,USTI                ,USTIR                ,
     4 VISI                 ,VISIR                ,UNUSED              ,UINER               ,DT1                  ,
     5 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),
     6 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)))
        ENDIF
C
        DO I=JFT,JLT
          IF (OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R5LEN3(
     1   JFT,                  JLT,                  GBUF%OFF,             DT2T,
     2   NELTST,               ITYPTST,              STI,                  STIR,
     3   MS,                   IN,                   USTI,                 USTIR,
     4   VISI,                 VISIR,                GBUF%MASS,            UINER,
     5   FR_WAVE,              FR_W_E,               GBUF%EINT,            GBUF%FOR(II(1)),
     6   GBUF%MOM(II(1)),      GBUF%MOM(II(2)),      GBUF%MOM(II(3)),      GBUF%V_REPCVT(II(1)),
     7   GBUF%V_REPCVT(II(2)), GBUF%V_REPCVT(II(3)), GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     8   GBUF%VR_REPCVT(II(3)),AL,                   GBUF%FOR(II(2)),      GBUF%FOR(II(3)),
     9   PARTSAV,              IPARTR,               MSRT,                 DMELRT,
     A   GBUF%G_DT,            GBUF%DT,              NGL,                  NC1,
     B   NC2,                  JSMS)

        CALL R5BILAN(
     1   GBUF%EINT,PARTSAV,  IXR,      GBUF%MASS,
     2   V,        IPARTR,   UINER,    X,
     3   VR,       GRESAV,   GRTH,     IGRTH,
     4   NC1,      NC2,      ITASK,    IAD,
     5   IGRE,     NFT,      NEL)
        CALL R4TORS(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),TANI,           AL,
     3   H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R5CUM3(
     1   F,              GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),
     2   AR,             GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     3   STI,            STIR,           STIFN,          STIFR,
     4   FX1,            FX2,            FY1,            FY2,
     5   FZ1,            FZ2,            MX1,            MX2,
     6   MY1,            MY2,            MZ1,            MZ2,
     7   GBUF%MOM(II(4)),GBUF%MOM(II(5)),AL,             EXX,
     8   EYX,            EZX,            EXY,            EYY,
     9   EZY,            EXZ,            EYZ,            EZZ,
     A   NC1,            NC2,            NEL)
        ELSE
          CALL R5CUM3P(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI,            STIR,
     3   FSKY,           FSKY,           IADR,           FX1,
     4   FX2,            FY1,            FY2,            FZ1,
     5   FZ2,            MX1,            MX2,            MY1,
     6   MY2,            MZ1,            MZ2,            GBUF%MOM(II(4)),
     7   GBUF%MOM(II(5)),EXX,            EYX,            EZX,
     8   EXY,            EYY,            EZY,            EXZ,
     9   EYZ,            EZZ,            AL,             NEL,
     A   NFT)
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 44) THEN
C=======================================================================
        CALL R4COOR3(
     1   X,        VR,       IXR,      GBUF%SKEW,
     2   NGL,      X1,       Y1,       Z1,
     3   X2,       Y2,       Z2,       MGN,
     4   RX1,      RY1,      RZ1,      RX2,
     5   RY2,      RZ2,      NC1,      NC2,
     6   NEL)
        CALL R5EVEC3(
     1   GBUF%SKEW,V,        NGL,      AL,
     2   X1,       Y1,       Z1,       X2,
     3   Y2,       Z2,       EXX,      EYX,
     4   EZX,      EXY,      EYY,      EZY,
     5   EXZ,      EYZ,      EZZ,      RX1,
     6   RY1,      RZ1,      RX2,      RY2,
     7   RZ2,      VX1,      VX2,      VY1,
     8   VY2,      VZ1,      VZ2,      NC1,
     9   NC2,      NEL)
        CALL R5DEF3(
     1   AL,                   GBUF%V_REPCVT(II(1)), GBUF%V_REPCVT(II(2)), GBUF%V_REPCVT(II(3)),
     2   GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_WAVE,
     3   FR_W_E,               GBUF%EINT,            GBUF%FOR(II(1)),      GBUF%MOM(II(1)),
     4   GBUF%MOM(II(2)),      GBUF%MOM(II(3)),      GBUF%FOR(II(2)),      GBUF%FOR(II(3)),
     5   PARTSAV,              IPARTR,               EXX,                  EYX,
     6   EZX,                  EXY,                  EYY,                  EZY,
     7   EXZ,                  EYZ,                  EZZ,                  RX1,
     8   RY1,                  RZ1,                  RX2,                  RY2,
     9   RZ2,                  VX1,                  VX2,                  VY1,
     A   VY2,                  VZ1,                  VZ2,                  NC1,
     B   NC2,                  NEL)
C
        NUVAR = NINT(GEO(25,I0))
        DO I=JFT,JLT
          OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
        ENDDO
C
        CALL RUSER44(
     1 NEL                  ,IOUT                 ,I0                  ,GBUF%VAR            ,NUVAR                ,      
     2 GBUF%FOR(II(1))      ,GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,  
     3 GBUF%MOM(II(3))      ,GBUF%EINT            ,OFF                 ,USTI                ,USTIR                ,  
     4 VISI                 ,VISIR                ,UNUSED              ,UINER               ,DT1                  ,  
     5 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),  
     6 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_W_E              )
C
        DO I=JFT,JLT
          IF (OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R5LEN3(
     1   JFT,                  JLT,                  GBUF%OFF,             DT2T,
     2   NELTST,               ITYPTST,              STI,                  STIR,
     3   MS,                   IN,                   USTI,                 USTIR,
     4   VISI,                 VISIR,                GBUF%MASS,            UINER,
     5   FR_WAVE,              FR_W_E,               GBUF%EINT,            GBUF%FOR(II(1)),
     6   GBUF%MOM(II(1)),      GBUF%MOM(II(2)),      GBUF%MOM(II(3)),      GBUF%V_REPCVT(II(1)),
     7   GBUF%V_REPCVT(II(2)), GBUF%V_REPCVT(II(3)), GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     8   GBUF%VR_REPCVT(II(3)),AL,                   GBUF%FOR(II(2)),      GBUF%FOR(II(3)),
     9   PARTSAV,              IPARTR,               MSRT,                 DMELRT,
     A   GBUF%G_DT,            GBUF%DT,              NGL,                  NC1,
     B   NC2,                  JSMS)

        CALL R5BILAN(
     1   GBUF%EINT,PARTSAV,  IXR,      GBUF%MASS,
     2   V,        IPARTR,   UINER,    X,
     3   VR,       GRESAV,   GRTH,     IGRTH,
     4   NC1,      NC2,      ITASK,    IAD,
     5   IGRE,     NFT,      NEL)
        CALL R4TORS(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),TANI,           AL,
     3   H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R5CUM3(
     1   F,              GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),
     2   AR,             GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     3   STI,            STIR,           STIFN,          STIFR,
     4   FX1,            FX2,            FY1,            FY2,
     5   FZ1,            FZ2,            MX1,            MX2,
     6   MY1,            MY2,            MZ1,            MZ2,
     7   GBUF%MOM(II(4)),GBUF%MOM(II(5)),AL,             EXX,
     8   EYX,            EZX,            EXY,            EYY,
     9   EZY,            EXZ,            EYZ,            EZZ,
     A   NC1,            NC2,            NEL)
        ELSE
          CALL R5CUM3P(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI,            STIR,
     3   FSKY,           FSKY,           IADR,           FX1,
     4   FX2,            FY1,            FY2,            FZ1,
     5   FZ2,            MX1,            MX2,            MY1,
     6   MY2,            MZ1,            MZ2,            GBUF%MOM(II(4)),
     7   GBUF%MOM(II(5)),EXX,            EYX,            EZX,
     8   EXY,            EYY,            EZY,            EXZ,
     9   EYZ,            EZZ,            AL,             NEL,
     A   NFT)
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 46) THEN
C=======================================================================
        CALL R4COOR3(
     1   X,        VR,       IXR,      GBUF%SKEW,
     2   NGL,      X1,       Y1,       Z1,
     3   X2,       Y2,       Z2,       MGN,
     4   RX1,      RY1,      RZ1,      RX2,
     5   RY2,      RZ2,      NC1,      NC2,
     6   NEL)
        CALL R5EVEC3(
     1   GBUF%SKEW,V,        NGL,      AL,
     2   X1,       Y1,       Z1,       X2,
     3   Y2,       Z2,       EXX,      EYX,
     4   EZX,      EXY,      EYY,      EZY,
     5   EXZ,      EYZ,      EZZ,      RX1,
     6   RY1,      RZ1,      RX2,      RY2,
     7   RZ2,      VX1,      VX2,      VY1,
     8   VY2,      VZ1,      VZ2,      NC1,
     9   NC2,      NEL)
        CALL R5DEF3(
     1   AL,                   GBUF%V_REPCVT(II(1)), GBUF%V_REPCVT(II(2)), GBUF%V_REPCVT(II(3)),
     2   GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_WAVE,
     3   FR_W_E,               GBUF%EINT,            GBUF%FOR(II(1)),      GBUF%MOM(II(1)),
     4   GBUF%MOM(II(2)),      GBUF%MOM(II(3)),      GBUF%FOR(II(2)),      GBUF%FOR(II(3)),
     5   PARTSAV,              IPARTR,               EXX,                  EYX,
     6   EZX,                  EXY,                  EYY,                  EZY,
     7   EXZ,                  EYZ,                  EZZ,                  RX1,
     8   RY1,                  RZ1,                  RX2,                  RY2,
     9   RZ2,                  VX1,                  VX2,                  VY1,
     A   VY2,                  VZ1,                  VZ2,                  NC1,
     B   NC2,                  NEL)
C
        NUVAR = NINT(GEO(25,I0))
        DO I=JFT,JLT
          OFF(I)=MIN(ONE,ABS(GBUF%OFF(I)))
        ENDDO
C
        CALL RUSER46(                                         
     1 NEL                  ,IOUT                 ,I0                  ,GBUF%VAR            ,NUVAR                ,      
     2 GBUF%FOR(II(1))      ,GBUF%FOR(II(2))      ,GBUF%FOR(II(3))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,  
     3 GBUF%MOM(II(3))      ,GBUF%EINT            ,OFF                 ,USTI                ,USTIR                ,  
     4 VISI                 ,VISIR                ,UNUSED              ,UINER               ,DT1                  ,  
     5 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),  
     6 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_W_E              )
C
        DO I=JFT,JLT
          IF (OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R5LEN3(
     1   JFT,                  JLT,                  GBUF%OFF,             DT2T,
     2   NELTST,               ITYPTST,              STI,                  STIR,
     3   MS,                   IN,                   USTI,                 USTIR,
     4   VISI,                 VISIR,                GBUF%MASS,            UINER,
     5   FR_WAVE,              FR_W_E,               GBUF%EINT,            GBUF%FOR(II(1)),
     6   GBUF%MOM(II(1)),      GBUF%MOM(II(2)),      GBUF%MOM(II(3)),      GBUF%V_REPCVT(II(1)),
     7   GBUF%V_REPCVT(II(2)), GBUF%V_REPCVT(II(3)), GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),
     8   GBUF%VR_REPCVT(II(3)),AL,                   GBUF%FOR(II(2)),      GBUF%FOR(II(3)),
     9   PARTSAV,              IPARTR,               MSRT,                 DMELRT,
     A   GBUF%G_DT,            GBUF%DT,              NGL,                  NC1,
     B   NC2,                  JSMS)

        CALL R5BILAN(
     1   GBUF%EINT,PARTSAV,  IXR,      GBUF%MASS,
     2   V,        IPARTR,   UINER,    X,
     3   VR,       GRESAV,   GRTH,     IGRTH,
     4   NC1,      NC2,      ITASK,    IAD,
     5   IGRE,     NFT,      NEL)
        CALL R4TORS(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),TANI,           AL,
     3   H3D_DATA,       NEL)
        IF (IPARIT == 0) THEN
          CALL R5CUM3(
     1   F,              GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),
     2   AR,             GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),
     3   STI,            STIR,           STIFN,          STIFR,
     4   FX1,            FX2,            FY1,            FY2,
     5   FZ1,            FZ2,            MX1,            MX2,
     6   MY1,            MY2,            MZ1,            MZ2,
     7   GBUF%MOM(II(4)),GBUF%MOM(II(5)),AL,             EXX,
     8   EYX,            EZX,            EXY,            EYY,
     9   EZY,            EXZ,            EYZ,            EZZ,
     A   NC1,            NC2,            NEL)
        ELSE
          CALL R5CUM3P(
     1   GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),
     2   GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI,            STIR,
     3   FSKY,           FSKY,           IADR,           FX1,
     4   FX2,            FY1,            FY2,            FZ1,
     5   FZ2,            MX1,            MX2,            MY1,
     6   MY2,            MZ1,            MZ2,            GBUF%MOM(II(4)),
     7   GBUF%MOM(II(5)),EXX,            EYX,            EZX,
     8   EXY,            EYY,            EZY,            EXZ,
     9   EYZ,            EZZ,            AL,             NEL,
     A   NFT)
        ENDIF
C-----
      ENDIF  ! IGTYP
C-----------------------------------------------
      RETURN
      END SUBROUTINE RFORC3

