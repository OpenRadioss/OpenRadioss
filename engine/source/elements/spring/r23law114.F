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
Chd|  R23LAW114                     source/elements/spring/r23law114.F
Chd|-- called by -----------
Chd|        R23FORC3                      source/elements/spring/r23forc3.F
Chd|-- calls ---------------
Chd|        R23BILAN                      source/elements/spring/r23bilan.F
Chd|        R23COOR3                      source/elements/spring/r23coor3.F
Chd|        R23L114DEF3                   source/elements/spring/r23l114def3.F
Chd|        R23SENS3                      source/elements/spring/r23sens3.F
Chd|        R23_114_COOR3                 source/tools/seatbelts/r23_114_coor3.F
Chd|        R2LEN3                        source/elements/spring/r2len3.F
Chd|        R4CUM3                        source/elements/spring/r4cum3.F
Chd|        R4CUM3P                       source/elements/spring/r4cum3p.F
Chd|        R4EVEC3                       source/elements/spring/r4evec3.F
Chd|        R4TORS                        source/elements/spring/r4tors.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE R23LAW114(
     1   ELBUF_STR,            JFT,                  JLT,                  NEL,
     2   MTN,                  IGEO,                 GEO,                  IPM,
     3   IXR,                  X,                    TABLE,                XDP,
     4   F,                    NPF,                  TF,                   SKEW,
     5   FLG_KJ2,              VR,                   AR,                   V,
     6   DT2T,                 NELTST,               ITYPTST,              STIFN,
     7   STIFR,                MS,                   IN,                   FSKY,
     8   IADR,                 SENSOR_TAB,           OFFSET,               ANIM,
     9   PARTSAV,              IPARTR,               TANI,                 FR_WAVE,
     A   BUFMAT,               BUFGEO,               PM,                   RBY,
     B   FX1,                  FX2,                  FY1,                  FY2,
     C   FZ1,                  FZ2,                  MX1,                  MX2,
     D   MY1,                  MY2,                  MZ1,                  MZ2,
     E   GRESAV,               GRTH,                 IGRTH,                MSRT,
     F   DMELRT,               FLAG_SLIPRING_UPDATE, FLAG_RETRACTOR_UPDATE,H3D_DATA,
     G   JSMS,                 IGRE,                 NFT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE TABLE_MOD
      USE H3D_MOD
      USE SENSOR_MOD
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IGRE
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: JSMS
      INTEGER IXR(NIXR,*), NPF(*),IADR(3,*),IPARTR(*),
     .        IGEO(NPROPGI,*),JFT,JLT,NELTST ,ITYPTST,OFFSET,
     .        NEL,MTN,GRTH(*),IGRTH(*),FLG_KJ2,IPM(NPROPMI,*),FLAG_SLIPRING_UPDATE,
     .        FLAG_RETRACTOR_UPDATE
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(MVSIZ),PID(MVSIZ),NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),
     .        MID(MVSIZ)
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
     .   VY1(MVSIZ),VY2(MVSIZ),VZ1(MVSIZ),VZ2(MVSIZ),VX3(MVSIZ),
     .   VY3(MVSIZ),VZ3(MVSIZ)
      INTEGER IGTYP,I,I0,NUVAR,IADBUF
      DOUBLE PRECISION
     .   X1DP(3,MVSIZ),X2DP(3,MVSIZ),X3DP(3,MVSIZ),
     .   ELX(3,MVSIZ),AL2DP(MVSIZ),ALDP(MVSIZ)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
      TYPE(G_BUFEL_),POINTER :: GBUF
      INTEGER II(9)
C=======================================================================
      GBUF => ELBUF_STR%GBUF      
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
      DO I=1,9
        II(I) = (I-1)*NEL + 1
      ENDDO
C
      I0 = IXR(1,1)
      IGTYP = IGEO(11,I0)
C
      BID = ZERO
C
      FR_W_E(1:NEL) = ZERO
C=======================================================================
C=======================================================================
       CALL R23COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    NGL,     X1,
     3   Y1,      Z1,      X2,      Y2,
     4   Z2,      PID,     MID,     RX1,
     5   RY1,     RZ1,     RX2,     RY2,
     6   RZ2,     NC1,     NC2,     NEL)
       CALL R23SENS3(
     1   GEO,                GBUF%OFF,           SENSOR_TAB,         GBUF%TOTDEPL(II(1)),
     2   GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%LENGTH(II(1)), GBUF%LENGTH(II(2)),
     3   GBUF%LENGTH(II(3)), GBUF%TOTROT(II(1)), GBUF%TOTROT(II(2)), GBUF%TOTROT(II(3)),
     4   IGEO,               PID,                NEL)
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
C
C
        IF (NSLIPRING > 0) THEN
           CALL R23_114_COOR3(
     1   X,       V,       IXR,     XDP,
     2   X3DP,    NC3,     VX3,     VY3,
     3   VZ3,     NEL)
        ENDIF
C
        NUVAR =  NINT(GEO(25,I0)) !! from Mid
        DO I=JFT,JLT
          MID(I)     = IXR(5,I)
          IADBUF = IPM(7,MID(I)) - 1
          NUVAR = MAX(NUVAR, NINT(BUFMAT(IADBUF + 4)))
        ENDDO
!!   is like r4def3 (spring type113)     
        CALL R23L114DEF3(
     1   SKEW,                   IPM,                    IGEO,                   MID,
     2   PID,                    GEO,                    BUFMAT,                 GBUF%FOR(II(1)),
     3   GBUF%FOR(II(2)),        GBUF%FOR(II(3)),        GBUF%EINT,              GBUF%TOTDEPL(II(1)),
     4   GBUF%TOTDEPL(II(2)),    GBUF%TOTDEPL(II(3)),    NPF,                    TF,
     5   OFF,                    GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),
     6   GBUF%DEP_IN_COMP(II(1)),GBUF%DEP_IN_COMP(II(2)),GBUF%DEP_IN_COMP(II(3)),GBUF%FOREP(II(1)),
     7   GBUF%FOREP(II(2)),      GBUF%FOREP(II(3)),      GBUF%LENGTH(II(1)),     GBUF%LENGTH(II(2)),
     8   GBUF%LENGTH(II(3)),     GBUF%MOM(II(1)),        GBUF%MOM(II(2)),        GBUF%MOM(II(3)),
     9   GBUF%TOTROT(II(1)),     GBUF%TOTROT(II(2)),     GBUF%TOTROT(II(3)),     GBUF%ROT_IN_TENS(II(1)),
     A   GBUF%ROT_IN_TENS(II(2)),GBUF%ROT_IN_TENS(II(3)),GBUF%MOMEP(II(1)),      GBUF%MOMEP(II(2)),
     B   GBUF%MOMEP(II(3)),      GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),
     C   ANIM,                   GBUF%POSX,              GBUF%POSY,              GBUF%POSZ,
     D   GBUF%POSXX,             GBUF%POSYY,             GBUF%POSZZ,             FR_WAVE,
     E   GBUF%E6,                NEL,                    EXX2,                   EYX2,
     F   EZX2,                   EXY2,                   EYY2,                   EZY2,
     G   EXZ2,                   EYZ2,                   EZZ2,                   AL2DP,
     H   NGL,                    GBUF%RUPTCRIT,          GBUF%LENGTH_ERR,        ALDP,
     I   GBUF%YIELD(II(1)),      GBUF%YIELD(II(2)),      GBUF%YIELD(II(3)),      GBUF%YIELD(II(4)),
     J   GBUF%YIELD(II(5)),      GBUF%YIELD(II(6)),      EXX,                    EYX,
     K   EZX,                    EXY,                    EYY,                    EZY,
     L   EXZ,                    EYZ,                    EZZ,                    XCR,
     M   RX1,                    RY1,                    RZ1,                    RX2,
     N   RY2,                    RZ2,                    XIN,                    AK,
     O   XM,                     XKM,                    XCM,                    XKR,
     P   VX1,                    VX2,                    VY1,                    VY2,
     Q   VZ1,                    VZ2,                    NUVAR,                  GBUF%VAR,
     R   GBUF%MASS,              GBUF%DEFINI(II(1)),     GBUF%DEFINI(II(2)),     GBUF%DEFINI(II(3)),
     S   GBUF%DEFINI(II(4)),     GBUF%DEFINI(II(5)),     GBUF%DEFINI(II(6)),     GBUF%SLIPRING_STRAND,
     T   GBUF%DFS,               GBUF%RINGSLIP,          GBUF%LENGTH(II(2)),     GBUF%LENGTH(II(3)),
     U   GBUF%SLIPRING_ID,       GBUF%UPDATE,            GBUF%RETRACTOR_ID,      GBUF%ADD_NODE(1),
     V   GBUF%ADD_NODE(NEL+1),   NC1,                    NC2,                    NC3,
     W   X1DP,                   X2DP,                   X3DP,                   VX3,
     X   VY3,                    VZ3,                    FLAG_SLIPRING_UPDATE,   FLAG_RETRACTOR_UPDATE,
     Y   SENSOR_TAB,             GBUF%INTVAR(II(1)),     GBUF%SLIPRING_FRAM_ID,  GBUF%FRAM_FACTOR,
     Z   GBUF%INTVAR(II(2)),     GBUF%INTVAR(II(3)),     GBUF%INTVAR(II(4)),     GBUF%INTVAR(II(5)),
     1   GBUF%INTVAR(II(6)),     GBUF%INTVAR(II(7)),     GBUF%INTVAR(II(8)),     GBUF%INTVAR(II(9)),
     2   NFT)
C
        DO I=JFT,JLT
          IF (GBUF%UPDATE(I) == -1) THEN
             GBUF%OFF(I) = OFF(I)
             GBUF%UPDATE(I) = 0
          ELSEIF  (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) THEN
             GBUF%OFF(I) = OFF(I)
          ENDIF
        ENDDO
C
        CALL R2LEN3(
     1   JFT,      JLT,      GBUF%OFF, DT2T,
     2   NELTST,   ITYPTST,  STI,      STIR,
     3   MS,       IN,       MSRT,     DMELRT,
     4   GBUF%G_DT,GBUF%DT,  NGL,      XCR,
     5   XIN,      XM,       XKM,      XCM,
     6   XKR,      NC1,      NC2,      JSMS)
        CALL R23BILAN(
     1   GBUF%EINT,PARTSAV,  IXR,      GEO,
     2   V,        IPARTR,   GBUF%MASS,GRESAV,
     3   GRTH,     IGRTH,    GBUF%OFF, NC1,
     4   NC2,      X,        VR,       NEL,
     5   IGRE)
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
C-----------------------------------------------
      RETURN
      END SUBROUTINE R23LAW114
