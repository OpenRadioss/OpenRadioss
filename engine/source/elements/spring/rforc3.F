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
Chd|  RFORC3                        source/elements/spring/rforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_RUSER             source/user_interface/dyn_userlib.c
Chd|        R1COOR3                       source/elements/spring/r1coor3.F
Chd|        R1CUM3                        source/elements/spring/r1cum3.F
Chd|        R1CUM3P                       source/elements/spring/r1cum3.F
Chd|        R1DEF3                        source/elements/spring/r1def3.F
Chd|        R1LEN3                        source/elements/spring/r1len3.F
Chd|        R1SENS3                       source/elements/spring/rsens3.F
Chd|        R1TORS                        source/elements/spring/r1tors.F
Chd|        R26DEF3                       source/elements/spring/r26def3.F
Chd|        R27DEF3                       source/elements/spring/r27def3.F
Chd|        R2COOR3                       source/elements/spring/r2coor3.F
Chd|        R2CUM3                        source/elements/spring/r2cum3.F
Chd|        R2CUM3P                       source/elements/spring/r2cum3.F
Chd|        R2DEF3                        source/elements/spring/r2def3.F
Chd|        R2LEN3                        source/elements/spring/r2len3.F
Chd|        R2SENS3                       source/elements/spring/rsens3.F
Chd|        R2TORS                        source/elements/spring/r2tors.F
Chd|        R3BILAN                       source/elements/spring/r3bilan.F
Chd|        R3COOR3                       source/elements/spring/r3coor3.F
Chd|        R3CUM3                        source/elements/spring/r3cum3.F
Chd|        R3CUM3P                       source/elements/spring/r3cum3.F
Chd|        R3DEF3                        source/elements/spring/r3def3.F
Chd|        R3LEN3                        source/elements/spring/r3len3.F
Chd|        R3TORS                        source/elements/spring/r3tors.F
Chd|        R4COOR3                       source/elements/spring/r4coor3.F
Chd|        R4CUM3                        source/elements/spring/r4cum3.F
Chd|        R4CUM3P                       source/elements/spring/r4cum3.F
Chd|        R4DEF3                        source/elements/spring/r4def3.F
Chd|        R4EVEC3                       source/elements/spring/r4evec3.F
Chd|        R4TORS                        source/elements/spring/r4tors.F
Chd|        R5BILAN                       source/elements/spring/rforc3.F
Chd|        R5CUM3                        source/elements/spring/r5cum3.F
Chd|        R5CUM3P                       source/elements/spring/r5cum3.F
Chd|        R5DEF3                        source/elements/spring/rforc3.F
Chd|        R5EVEC3                       source/elements/spring/r5evec3.F
Chd|        R5LEN3                        source/elements/spring/rforc3.F
Chd|        R6DEF3                        source/elements/spring/r6def3.F
Chd|        RBILAN                        source/elements/spring/rbilan.F
Chd|        RGJOINT                       source/elements/joint/rgjoint.F
Chd|        RUSER32                       source/elements/spring/ruser32.F
Chd|        RUSER35                       source/elements/spring/ruser35.F
Chd|        RUSER36                       source/elements/spring/ruser36.F
Chd|        RUSER44                       source/elements/spring/ruser44.F
Chd|        RUSER46                       source/elements/spring/ruser46.F
Chd|        SET_SPRING_ELNUM              source/user_interface/uaccess.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE RFORC3(ELBUF_STR,
     1                  JFT      ,JLT     ,NEL    ,MTN     ,IGEO   ,
     2                  GEO      ,IXR     ,X      ,TABLE   ,XDP    ,
     3                  F        ,NPF     ,TF     ,SKEW    ,FLG_KJ2,
     4                  VR       ,AR      ,V      ,DT2T    ,NELTST ,
     5                  ITYPTST  ,STIFN   ,STIFR  ,MS      ,IN     ,
     6                  FSKY     ,IADR    ,SENSOR ,LSENSOR ,OFFSET ,
     7                  ANIM     ,PARTSAV ,IPARTR ,TANI    ,FR_WAVE,
     8                  BUFMAT   ,BUFGEO  ,PM     ,RBY     ,FX1    ,
     9                  FX2      ,FY1     ,FY2    ,FZ1     ,FZ2    ,
     A                  MX1      ,MX2     ,MY1    ,MY2     ,MZ1    ,
     B                  MZ2      ,GRESAV  ,GRTH   ,IGRTH   ,MSRT   ,
     C                  DMELRT   ,ITASK   ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE TABLE_MOD
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
#include      "scr03_c.inc"
#include      "chara_c.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*), NPF(*),IADR(3,*),LSENSOR(NSENSI,*),IPARTR(*),
     .        IGEO(NPROPGI,*),JFT,JLT,NELTST ,ITYPTST,OFFSET,
     .        NEL,MTN,GRTH(*),IGRTH(*),FLG_KJ2,ITASK
      my_real DT2T ,
     .   GEO(NPROPG,*),X(*),F(*),TF(*),SKEW(LSKEW,*),FSKY(*),
     .   VR(*), V(*), AR(*), STIFN(*),STIFR(*),MS(*), IN(*),
     .   SENSOR(NSENSR,*),ANIM(*),PARTSAV(*),TANI(15,*),
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
      INTEGER IGTYP,I,I0,NUVAR
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
        CALL R1COOR3(X   ,VR  ,IXR  ,XDP  ,X1DP,
     2               X2DP,NGL ,MGN  ,NC1  ,NC2 )
        CALL R1SENS3(GEO               ,GBUF%OFF ,SENSOR ,LSENSOR,GBUF%TOTDEPL(II(1)),
     2               GBUF%LENGTH(II(1)),IGEO      ,MGN   )
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
        CALL R1DEF3(GEO    ,GBUF%FOR(II(1)),GBUF%LENGTH(II(1)),GBUF%EINT       ,GBUF%TOTDEPL(II(1)),
     2              NPF    ,TF             ,OFF        ,GBUF%DEP_IN_TENS(II(1)),GBUF%FOREP(II(1))  ,
     3              GBUF%DEP_IN_COMP(II(1)),ANIM       ,GBUF%POSX    ,FR_WAVE  ,IGEO               ,
     4              GBUF%LENGTH_ERR        ,X1DP       ,X2DP         ,V        ,GBUF%YIELD(II(1))  ,
     5              NGL                    ,MGN        ,EX           ,EY       ,EZ                 ,
     6              XK                     ,XM         ,XC           ,AK       ,NC1                ,
     7              NC2                    ,NUVAR      ,GBUF%VAR     ,GBUF%DEFINI)
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R1LEN3(JFT       ,JLT    ,GBUF%OFF,DT2T ,NELTST ,
     2              ITYPTST   ,STI    ,MS      ,MSRT ,DMELRT ,
     3              GBUF%G_DT ,GBUF%DT,NGL     ,XK   ,XM     ,
     4              XC        ,AK     ,NC1     ,NC2  )
        CALL RBILAN(GBUF%EINT ,PARTSAV           ,IXR    ,GEO  ,V    ,
     2              IPARTR    ,GBUF%LENGTH(II(1)),GRESAV ,GRTH ,IGRTH,
     3              GBUF%OFF  ,NC1               ,NC2    ,X    ,VR   ,ITASK)
        CALL R1TORS(GBUF%FOR(II(1)),TANI,H3D_DATA)
        IF (IPARIT == 0) THEN
          CALL R1CUM3(F     ,GBUF%FOR(II(1)) ,STI   ,STIFN   ,FX1,
     2                FX2   ,FY1             ,FY2   ,FZ1     ,FZ2,
     3                MX1   ,MX2             ,MY1   ,MY2     ,MZ1,
     4                MZ2   ,EX              ,EY    ,EZ      ,NC1,
     5                NC2   )
        ELSE
          CALL R1CUM3P(GBUF%FOR(II(1)),STI ,FSKY ,FSKY ,IADR,
     2                 FX1            ,FX2 ,FY1  ,FY2  ,FZ1 ,
     3                 FZ2            ,MX1 ,MX2  ,MY1  ,MY2 ,
     4                 MZ1            ,MZ2 ,EX   ,EY   ,EZ  )
       ENDIF

C=======================================================================
      ELSEIF (IGTYP == 26) THEN
C=======================================================================
        CALL R1COOR3(X   ,VR  ,IXR  ,XDP  ,X1DP,
     2               X2DP,NGL ,MGN  ,NC1  ,NC2 )
        CALL R1SENS3(GEO               ,GBUF%OFF ,SENSOR ,LSENSOR ,GBUF%TOTDEPL(II(1)),
     2               GBUF%LENGTH(II(1)),IGEO     ,MGN    )
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
        CALL R26DEF3(GBUF%FOR(II(1))  ,GBUF%EINT              ,GBUF%TOTDEPL(II(1)),GBUF%LENGTH(II(1)),GBUF%DV,
     2               GBUF%FOREP(II(1)),GBUF%DEP_IN_COMP(II(1)),GBUF%POSX          ,GEO               ,IGEO   ,
     3               NPF              ,TF                     ,V                  ,OFF               ,ANIM   ,
     4               FR_WAVE          ,GBUF%LENGTH_ERR        ,X1DP               ,X2DP              ,NGL    ,
     5               MGN              ,EX                     ,EY                 ,EZ                ,XK     ,
     6               XM               ,XC                     ,AK                 )
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R1LEN3(JFT       ,JLT    ,GBUF%OFF,DT2T ,NELTST ,
     2              ITYPTST   ,STI    ,MS      ,MSRT ,DMELRT ,
     3              GBUF%G_DT ,GBUF%DT,NGL     ,XK   ,XM     ,
     4              XC        ,AK     ,NC1     ,NC2  )
        CALL RBILAN(GBUF%EINT,PARTSAV           ,IXR    ,GEO  ,V    ,
     2              IPARTR   ,GBUF%LENGTH(II(1)),GRESAV ,GRTH ,IGRTH,
     3              GBUF%OFF ,NC1               ,NC2    ,X    ,VR   ,ITASK)
        CALL R1TORS(GBUF%FOR(II(1)),TANI,H3D_DATA)
        IF (IPARIT == 0) THEN
          CALL R1CUM3(F    ,GBUF%FOR(II(1)),STI  ,STIFN ,FX1,
     2                FX2  ,FY1            ,FY2  ,FZ1   ,FZ2,
     3                MX1  ,MX2            ,MY1  ,MY2   ,MZ1,
     4                MZ2  ,EX             ,EY   ,EZ    ,NC1,
     5                NC2  )
        ELSE
          CALL R1CUM3P(GBUF%FOR(II(1)),STI ,FSKY ,FSKY ,IADR,
     2                 FX1            ,FX2 ,FY1  ,FY2  ,FZ1 ,
     3                 FZ2            ,MX1 ,MX2  ,MY1  ,MY2 ,
     4                 MZ1            ,MZ2 ,EX   ,EY   ,EZ  )
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 27) THEN
C=======================================================================
        CALL R1COOR3(X   ,VR  ,IXR  ,XDP  ,X1DP,
     2               X2DP,NGL ,MGN  ,NC1  ,NC2 )
        CALL R1SENS3(GEO               ,GBUF%OFF ,SENSOR ,LSENSOR,GBUF%TOTDEPL(II(1)),
     2               GBUF%LENGTH(II(1)),IGEO      ,MGN   )
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
        CALL R27DEF3(GBUF%FOR(II(1))  ,GBUF%EINT    ,GBUF%TOTDEPL(II(1)),GBUF%LENGTH(II(1)),
     2               GBUF%POSX        ,GEO          ,IGEO               ,
     3               NPF              ,TF           ,V                  ,OFF               ,ANIM    ,
     4               GBUF%LENGTH_ERR  ,X1DP         ,X2DP               ,NGL               ,
     5               MGN              ,EX           ,EY                 ,EZ                ,XK      ,
     6               XM               ,XC           ,AK                 )
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R1LEN3(JFT       ,JLT    ,GBUF%OFF,DT2T ,NELTST ,
     2              ITYPTST   ,STI    ,MS      ,MSRT ,DMELRT ,
     3              GBUF%G_DT ,GBUF%DT,NGL     ,XK   ,XM     ,
     4              XC        ,AK     ,NC1     ,NC2  )
        CALL RBILAN(GBUF%EINT ,PARTSAV           ,IXR    ,GEO  ,V    ,
     2              IPARTR    ,GBUF%LENGTH(II(1)),GRESAV ,GRTH ,IGRTH,
     3              GBUF%OFF  ,NC1               ,NC2    ,X    ,VR   ,ITASK)
        CALL R1TORS(GBUF%FOR(II(1)),TANI,H3D_DATA)
        IF (IPARIT == 0) THEN
          CALL R1CUM3(F     ,GBUF%FOR(II(1)) ,STI   ,STIFN   ,FX1,
     2                FX2   ,FY1             ,FY2   ,FZ1     ,FZ2,
     3                MX1   ,MX2             ,MY1   ,MY2     ,MZ1,
     4                MZ2   ,EX              ,EY    ,EZ      ,NC1,
     5                NC2   )
        ELSE
          CALL R1CUM3P(GBUF%FOR(II(1)),STI ,FSKY ,FSKY ,IADR,
     2                 FX1            ,FX2 ,FY1  ,FY2  ,FZ1 ,
     3                 FZ2            ,MX1 ,MX2  ,MY1  ,MY2 ,
     4                 MZ1            ,MZ2 ,EX   ,EY   ,EZ  )
       ENDIF
C=======================================================================
      ELSEIF (IGTYP == 8) THEN
C=======================================================================
        CALL R2COOR3(X   ,VR  ,IXR  ,XDP  ,X1DP,
     2               X2DP,NGL ,X1   ,Y1   ,Z1  ,
     3               X2  ,Y2  ,Z2   ,MGN  ,RX1 ,
     4               RY1 ,RZ1 ,RX2  ,RY2  ,RZ2 ,
     5               NC1 ,NC2 )
        CALL R2SENS3(GEO                ,GBUF%OFF           ,SENSOR            ,LSENSOR           ,GBUF%TOTDEPL(II(1)),
     2               GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%LENGTH(II(1)),GBUF%LENGTH(II(2)),GBUF%LENGTH(II(3)) ,
     3               GBUF%TOTROT(II(1)) ,GBUF%TOTROT(II(2)) ,GBUF%TOTROT(II(3)),IGEO              ,MGN                )
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
     1 SKEW                   ,GEO                    ,GBUF%FOR(II(1))        ,GBUF%FOR(II(2))        ,GBUF%FOR(II(3))        ,
     2 GBUF%EINT              ,GBUF%TOTDEPL(II(1))    ,GBUF%TOTDEPL(II(2))    ,GBUF%TOTDEPL(II(3))    ,NPF                    ,
     3 TF                     ,OFF                    ,GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),
     4 GBUF%DEP_IN_COMP(II(1)),GBUF%DEP_IN_COMP(II(2)),GBUF%DEP_IN_COMP(II(3)),GBUF%FOREP(II(1))      ,GBUF%FOREP(II(2))      ,
     5 GBUF%FOREP(II(3))      ,GBUF%LENGTH(II(1))     ,GBUF%LENGTH(II(2))     ,GBUF%LENGTH(II(3))     ,GBUF%MOM(II(1))        ,
     6 GBUF%MOM(II(2))        ,GBUF%MOM(II(3))        ,GBUF%TOTROT(II(1))     ,GBUF%TOTROT(II(2))     ,GBUF%TOTROT(II(3))     ,
     7 GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),GBUF%ROT_IN_TENS(II(3)),GBUF%MOMEP(II(1))      ,GBUF%MOMEP(II(2))      ,
     8 GBUF%MOMEP(II(3))      ,GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),ANIM                   ,
     9 GBUF%POSX              ,GBUF%POSY              ,GBUF%POSZ              ,GBUF%POSXX             ,GBUF%POSYY             ,
     A GBUF%POSZZ             ,FR_WAVE                ,V                      ,IGEO                   ,GBUF%E6                ,
     B GBUF%RUPTCRIT          ,NEL                    ,GBUF%LENGTH_ERR        ,X1DP                   ,X2DP                   ,
     C GBUF%YIELD(II(1))      ,GBUF%YIELD(II(2))      ,GBUF%YIELD(II(3))      ,GBUF%YIELD(II(4))      ,GBUF%YIELD(II(5))      ,
     D GBUF%YIELD(II(6))      ,NGL                    ,XKR                    ,MGN                    ,EXX                    ,
     E EYX                    ,EZX                    ,EXY                    ,EYY                    ,EZY                    ,
     F EXZ                    ,EYZ                    ,EZZ                    ,XCR                    ,RX1                    ,
     G RY1                    ,RZ1                    ,RX2                    ,RY2                    ,RZ2                    ,
     H XIN                    ,AK                     ,XM                     ,XKM                    ,XCM                    ,
     I NC1                    ,NC2                    ,NUVAR                  ,GBUF%VAR               ,GBUF%DEFINI(II(1))     ,
     J GBUF%DEFINI(II(2))     ,GBUF%DEFINI(II(3))     ,GBUF%DEFINI(II(4))     ,GBUF%DEFINI(II(5))     ,GBUF%DEFINI(II(6))     ,
     K IEQUIL                 ,GBUF%SKEW_ID)
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R2LEN3(JFT     ,JLT    ,GBUF%OFF  ,DT2T   ,NELTST ,
     2              ITYPTST ,STI    ,STIR      ,MS     ,IN     ,
     3              MSRT    ,DMELRT ,GBUF%G_DT ,GBUF%DT,NGL    ,
     4              XCR     ,XIN    ,XM        ,XKM    ,XCM    ,
     5              XKR     ,NC1    ,NC2       )
        CALL RBILAN(GBUF%EINT,PARTSAV,IXR    ,GEO  ,V    ,
     2              IPARTR   ,BID    ,GRESAV ,GRTH ,IGRTH,
     3              GBUF%OFF ,NC1    ,NC2    ,X    ,VR   ,ITASK)
        CALL R2TORS(GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2              GBUF%MOM(II(3)),TANI           ,H3D_DATA)
        IF (IPARIT == 0) THEN
          CALL R2CUM3(
     1       F              ,GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),AR  ,
     2       GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI            ,STIR,
     3       STIFN          ,STIFR          ,FX1            ,FX2            ,FY1 ,
     4       FY2            ,FZ1            ,FZ2            ,MX1            ,MX2 ,
     5       MY1            ,MY2            ,MZ1            ,MZ2            ,GEO ,
     6       X1             ,Y1             ,Z1             ,X2             ,Y2  ,
     7       Z2             ,IEQUIL         ,EXX            ,EYX            ,EZX ,
     8       EXY            ,EYY            ,EZY            ,EXZ            ,EYZ ,
     9       EZZ            ,NC1            ,NC2            )
        ELSE
          CALL R2CUM3P(
     1      GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2      GBUF%MOM(II(3)),STI            ,STIR           ,FSKY           ,FSKY           ,
     3      IADR           ,FX1            ,FX2            ,FY1            ,FY2            ,
     4      FZ1            ,FZ2            ,MX1            ,MX2            ,MY1            ,
     5      MY2            ,MZ1            ,MZ2            ,GEO            ,X1             ,
     6      Y1             ,Z1             ,X2             ,Y2             ,Z2             ,
     7      IEQUIL         ,EXX            ,EYX            ,EZX            ,EXY            ,
     8      EYY            ,EZY            ,EXZ            ,EYZ            ,EZZ            )
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 12) THEN
C=======================================================================
        CALL R3COOR3(X    ,VR  ,IXR ,XDP ,X1DP,
     2               X2DP ,X3DP,NGL ,MGN ,NC1 ,
     3               NC2  ,NC3 )
        CALL R1SENS3(GEO               ,GBUF%OFF ,SENSOR ,LSENSOR ,GBUF%TOTDEPL(II(1)),
     2               GBUF%LENGTH(II(1)),IGEO     ,MGN    )
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
     1     GEO            ,GBUF%FOR(II(1))  ,GBUF%LENGTH(II(1)),GBUF%EINT              ,GBUF%TOTDEPL(II(1)),
     2     NPF            ,TF               ,OFF               ,GBUF%DEP_IN_TENS(II(1)),GBUF%FOREP(II(1))  ,
     3     GBUF%DEP_IN_COMP(II(1)),GBUF%DFS ,V                 ,IXR                    ,DF                 ,
     4     ANIM           ,GBUF%POSX        ,FR_WAVE           ,IGEO                   ,GBUF%LENGTH_ERR    ,
     5     X1DP           ,X2DP             ,X3DP              ,GBUF%YIELD(II(1))      ,TABLE              ,
     6     GBUF%INIFRIC   ,NGL              ,MGN               ,EX                     ,EY                 ,
     7     EZ             ,XK               ,XM                ,XC                     ,AK                 ,
     8     EX2            ,EY2              ,EZ2               ,NC1                    ,NC2                ,
     9     NC3            ,NUVAR            ,GBUF%VAR          ,GBUF%DEFINI            )
c
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
        CALL R3LEN3(JFT       ,JLT    ,GBUF%OFF,DT2T ,NELTST ,
     2              ITYPTST   ,STI    ,MS      ,MSRT ,DMELRT ,
     3              GBUF%G_DT ,GBUF%DT,NGL     ,XK   ,XM     ,
     4              XC        ,AK     ,NC1     ,NC2  ,NC3    )
        CALL R3BILAN(GBUF%EINT,PARTSAV           ,IXR    ,GEO  ,V    ,
     2               IPARTR   ,GBUF%LENGTH(II(1)),GRESAV ,GRTH ,IGRTH,
     3               NC1      ,NC2               ,NC3    ,X    ,VR,ITASK)
        CALL R3TORS(GBUF%FOR(II(1)) ,DF     ,TANI     ,H3D_DATA)
        IF (IPARIT == 0) THEN
          CALL R3CUM3(F    ,GBUF%FOR(II(1)) ,STI ,STIFN ,DF  ,
     2                EX   ,EY              ,EZ  ,EX2   ,EY2 ,
     3                EZ2  ,NC1             ,NC2 ,NC3   )
        ELSE
          CALL R3CUM3P(GBUF%FOR(II(1)) ,STI  ,FSKY ,FSKY ,IADR,
     2                 DF              ,EX   ,EY   ,EZ   ,EX2 ,
     3                 EY2             ,EZ2  )
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 13) THEN
C=======================================================================
       CALL R2COOR3(X   ,VR  ,IXR  ,XDP  ,X1DP,
     2              X2DP,NGL ,X1   ,Y1   ,Z1  ,
     3              X2  ,Y2  ,Z2   ,MGN  ,RX1 ,
     4              RY1 ,RZ1 ,RX2  ,RY2  ,RZ2 ,
     5              NC1 ,NC2 )
       CALL R2SENS3(GEO                ,GBUF%OFF           ,SENSOR            ,LSENSOR           ,GBUF%TOTDEPL(II(1)),
     2              GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%LENGTH(II(1)),GBUF%LENGTH(II(2)),GBUF%LENGTH(II(3)) ,
     3              GBUF%TOTROT(II(1)) ,GBUF%TOTROT(II(2)) ,GBUF%TOTROT(II(3)),IGEO              ,MGN                )
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
        CALL R4EVEC3(GBUF%SKEW ,V            ,EXX2 ,EYX2 ,EZX2,
     2               EXY2      ,EYY2         ,EZY2 ,EXZ2 ,EYZ2,
     3               EZZ2      ,AL2DP        ,X1DP ,X2DP ,AL2 ,
     4               ALDP      ,GBUF%SKEW_ERR,NGL  ,AL   ,EXX ,
     5               EYX       ,EZX          ,EXY  ,EYY  ,EZY ,
     6               EXZ       ,EYZ          ,EZZ  ,RX1  ,RY1 ,
     7               RZ1       ,RX2          ,RY2  ,RZ2  ,VX1 ,
     8               VX2       ,VY1          ,VY2  ,VZ1  ,VZ2 ,
     9               NC1       ,NC2          )
        CALL R4DEF3(
     1 SKEW                   ,GEO                    ,GBUF%FOR(II(1))        ,GBUF%FOR(II(2))        ,GBUF%FOR(II(3))        ,
     2 GBUF%EINT              ,GBUF%TOTDEPL(II(1))    ,GBUF%TOTDEPL(II(2))    ,GBUF%TOTDEPL(II(3))    ,NPF                    ,
     3 TF                     ,OFF                    ,GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),
     4 GBUF%DEP_IN_COMP(II(1)),GBUF%DEP_IN_COMP(II(2)),GBUF%DEP_IN_COMP(II(3)),GBUF%FOREP(II(1))      ,GBUF%FOREP(II(2))      ,
     5 GBUF%FOREP(II(3))      ,GBUF%LENGTH(II(1))     ,GBUF%LENGTH(II(2))     ,GBUF%LENGTH(II(3))     ,GBUF%MOM(II(1))        ,
     6 GBUF%MOM(II(2))        ,GBUF%MOM(II(3))        ,GBUF%TOTROT(II(1))     ,GBUF%TOTROT(II(2))     ,GBUF%TOTROT(II(3))     ,
     7 GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),GBUF%ROT_IN_TENS(II(3)),GBUF%MOMEP(II(1))      ,GBUF%MOMEP(II(2))      ,
     8 GBUF%MOMEP(II(3))      ,GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),ANIM                   ,
     9 GBUF%POSX              ,GBUF%POSY              ,GBUF%POSZ              ,GBUF%POSXX             ,GBUF%POSYY             ,
     A GBUF%POSZZ             ,FR_WAVE                ,GBUF%E6                ,NEL                    ,EXX2                   ,
     B EYX2                   ,EZX2                   , EXY2                  ,EYY2                   ,EZY2                   ,
     C EXZ2                   ,EYZ2                   , EZZ2                  ,AL2DP                  ,IGEO                   ,
     D GBUF%RUPTCRIT          ,GBUF%LENGTH_ERR        ,ALDP                   ,GBUF%YIELD(II(1))      ,GBUF%YIELD(II(2))      ,
     E GBUF%YIELD(II(3))      ,GBUF%YIELD(II(4))      ,GBUF%YIELD(II(5))      ,GBUF%YIELD(II(6))      ,NGL                    ,
     F MGN                    ,EXX                    ,EYX                    ,EZX                    ,EXY                    ,
     E EYY                    ,EZY                    ,EXZ                    ,EYZ                    ,EZZ                    ,
     G XCR                    ,RX1                    ,RY1                    ,RZ1                    ,RX2                    ,
     H RY2                    ,RZ2                    ,XIN                    ,AK                     ,XM                     ,
     I XKM                    ,XCM                    ,XKR                    ,VX1                    ,VX2                    ,
     J VY1                    ,VY2                    ,VZ1                    ,VZ2                    ,NUVAR                  ,
     K GBUF%VAR               ,GBUF%DEFINI(II(1))     ,GBUF%DEFINI(II(2))     ,GBUF%DEFINI(II(3))     ,GBUF%DEFINI(II(4))     ,
     L GBUF%DEFINI(II(5))     ,GBUF%DEFINI(II(6))     ,GBUF%FORINI(II(1))     ,GBUF%FORINI(II(2))     ,GBUF%FORINI(II(3))     ,
     M GBUF%FORINI(II(4))     ,GBUF%FORINI(II(5))     ,GBUF%FORINI(II(6))     )
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
        CALL R2LEN3(JFT    ,JLT    ,GBUF%OFF  ,DT2T   ,NELTST ,
     2              ITYPTST,STI    ,STIR      ,MS     ,IN     ,
     3              MSRT   ,DMELRT ,GBUF%G_DT ,GBUF%DT,NGL    ,
     4              XCR    ,XIN    ,XM        ,XKM    ,XCM    ,
     5              XKR    ,NC1    ,NC2       )
        CALL RBILAN(GBUF%EINT,PARTSAV           ,IXR   ,GEO ,V    ,
     2              IPARTR   ,GBUF%LENGTH(II(1)),GRESAV,GRTH,IGRTH,
     3              GBUF%OFF ,NC1               ,NC2   ,X   ,VR   ,ITASK)
        CALL R4TORS(GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2              GBUF%MOM(II(3)),TANI           ,AL             ,H3D_DATA       )
        IF (IPARIT == 0) THEN
          CALL R4CUM3(
     1       F              ,GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),AR  ,
     2       GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI            ,STIR,
     3       STIFN          ,STIFR          ,FX1            ,FX2            ,FY1 ,
     4       FY2            ,FZ1            ,FZ2            ,MX1            ,MX2 ,
     5       MY1            ,MY2            ,MZ1            ,MZ2            ,AL  ,
     6       EXX            ,EYX            ,EZX            ,EXY            ,EYY ,
     7       EZY            ,EXZ            ,EYZ            ,EZZ            ,NC1 ,
     8       NC2            )
        ELSE
          CALL R4CUM3P(
     1       GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2       GBUF%MOM(II(3)),STI            ,STIR           ,FSKY           ,FSKY           ,
     3       IADR           ,FX1            ,FX2            ,FY1            ,FY2            ,
     4       FZ1            ,FZ2            ,MX1            ,MX2            ,MY1            ,
     5       MY2            ,MZ1            ,MZ2            ,EXX            ,EYX            ,
     6       EZX            ,EXY            ,EYY            ,EZY            ,EXZ            ,
     7       EYZ            ,EZZ            ,AL             )
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 25) THEN
C=======================================================================
        CALL R2COOR3(X   ,VR  ,IXR  ,XDP  ,X1DP,
     2               X2DP,NGL ,X1   ,Y1   ,Z1  ,
     3               X2  ,Y2  ,Z2   ,MGN  ,RX1 ,
     4               RY1 ,RZ1 ,RX2  ,RY2  ,RZ2 ,
     5               NC1 ,NC2 )
        CALL R2SENS3(GEO                ,GBUF%OFF           ,SENSOR            ,LSENSOR            ,GBUF%TOTDEPL(II(1)),
     2               GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%LENGTH(II(1)),GBUF%LENGTH(II(2)) ,GBUF%LENGTH(II(3)) ,
     3               GBUF%TOTROT(II(1)) ,GBUF%TOTROT(II(2)) ,GBUF%TOTROT(II(3)),IGEO               ,MGN                )
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
        CALL R4EVEC3(GBUF%SKEW ,V             ,EXX2 ,EYX2 ,EZX2,
     2               EXY2      ,EYY2          ,EZY2 ,EXZ2 ,EYZ2,
     3               EZZ2      ,AL2DP         ,X1DP ,X2DP ,AL2 ,
     4               ALDP      ,GBUF%SKEW_ERR ,NGL  ,AL   ,EXX ,
     5               EYX       ,EZX           ,EXY  ,EYY  ,EZY ,
     6               EXZ       ,EYZ           ,EZZ  ,RX1  ,RY1 ,
     7               RZ1       ,RX2           ,RY2  ,RZ2  ,VX1 ,
     8               VX2       ,VY1           ,VY2  ,VZ1  ,VZ2 ,
     9               NC1       ,NC2           )
        CALL R6DEF3(
     1 SKEW                   ,GEO                    ,GBUF%FOR(II(1))        ,GBUF%FOR(II(2))        ,GBUF%FOR(II(3))        ,
     2 GBUF%EINT              ,GBUF%TOTDEPL(II(1))    ,GBUF%TOTDEPL(II(2))    ,GBUF%TOTDEPL(II(3))    ,NPF                    ,
     3 TF                     ,GBUF%OFF               ,GBUF%DEP_IN_TENS(II(1)),GBUF%DEP_IN_TENS(II(2)),GBUF%DEP_IN_TENS(II(3)),
     4 GBUF%DEP_IN_COMP(II(1)),GBUF%DEP_IN_COMP(II(2)),GBUF%DEP_IN_COMP(II(3)),GBUF%FOREP(II(1))      ,GBUF%FOREP(II(2))      ,
     5 GBUF%FOREP(II(3))      ,GBUF%LENGTH(II(1))     ,GBUF%LENGTH(II(2))     ,GBUF%LENGTH(II(3))     ,GBUF%MOM(II(1))        ,
     6 GBUF%MOM(II(2))        ,GBUF%MOM(II(3))        ,GBUF%TOTROT(II(1))     ,GBUF%TOTROT(II(2))     ,GBUF%TOTROT(II(3))     ,
     7 GBUF%ROT_IN_TENS(II(1)),GBUF%ROT_IN_TENS(II(2)),GBUF%ROT_IN_TENS(II(3)),GBUF%MOMEP(II(1))      ,GBUF%MOMEP(II(2))      ,
     8 GBUF%MOMEP(II(3))      ,GBUF%ROT_IN_COMP(II(1)),GBUF%ROT_IN_COMP(II(2)),GBUF%ROT_IN_COMP(II(3)),ANIM                   ,
     9 GBUF%POSX              ,GBUF%POSY              ,GBUF%POSZ              ,GBUF%POSXX             ,GBUF%POSYY             ,
     A GBUF%POSZZ             ,FR_WAVE                ,GBUF%E6                ,NEL                    ,AL2DP                  ,
     K EXX2                   ,EYX2                   ,EZX2                   ,EXY2                   ,EYY2                   ,      
     L EZY2                   ,EXZ2                   ,EYZ2                   ,EZZ2                   ,IGEO                   ,
     M GBUF%LENGTH_ERR        ,ALDP                   ,GBUF%YIELD(II(1))      ,GBUF%YIELD(II(2))      ,GBUF%YIELD(II(3))      ,
     N GBUF%YIELD(II(4))      ,NGL                    ,MGN                    ,XCR                    ,RX1                    ,
     O RY1                    ,RZ1                    ,RX2                    ,RY2                    ,RZ2                    ,
     P XIN                    ,AK                     ,XM                     ,XKM                    ,XCM                    ,
     Q XKR                    ,VX1                    ,VX2                    ,VY1                    ,VY2                    ,
     R VZ1                    ,VZ2                    ,NUVAR                  ,GBUF%VAR               ,GBUF%DEFINI(II(1))     ,
     S GBUF%DEFINI(II(2))     ,GBUF%DEFINI(II(3))     ,GBUF%DEFINI(II(4))     ,GBUF%DEFINI(II(5))     ,GBUF%DEFINI(II(6))     ,
     T GBUF%FORINI(II(1))     ,GBUF%FORINI(II(2))     ,GBUF%FORINI(II(3))     ,GBUF%FORINI(II(4))     ,GBUF%FORINI(II(5))     ,
     U GBUF%FORINI(II(6))     )
C
        DO I=JFT,JLT
          IF (GBUF%OFF(I) /= -TEN .AND. OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R2LEN3(JFT    ,JLT    ,GBUF%OFF  ,DT2T   ,NELTST ,
     2              ITYPTST,STI    ,STIR      ,MS     ,IN     ,
     3              MSRT   ,DMELRT ,GBUF%G_DT ,GBUF%DT,NGL    ,
     4              XCR    ,XIN    ,XM        ,XKM    ,XCM    ,
     5              XKR    ,NC1    ,NC2       )
        CALL RBILAN(GBUF%EINT,PARTSAV           ,IXR   ,GEO ,V    ,
     2              IPARTR   ,GBUF%LENGTH(II(1)),GRESAV,GRTH,IGRTH,
     3              GBUF%OFF ,NC1               ,NC2   ,X   ,VR   ,ITASK)
        CALL R4TORS(GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2              GBUF%MOM(II(3)),TANI           ,AL             ,H3D_DATA       )
        IF (IPARIT == 0) THEN
          CALL R4CUM3(
     1       F              ,GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),AR  ,
     2       GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI            ,STIR,
     3       STIFN          ,STIFR          ,FX1            ,FX2            ,FY1 ,
     4       FY2            ,FZ1            ,FZ2            ,MX1            ,MX2 ,
     5       MY1            ,MY2            ,MZ1            ,MZ2            ,AL  ,
     6       EXX            ,EYX            ,EZX            ,EXY            ,EYY ,
     7       EZY            ,EXZ            ,EYZ            ,EZZ            ,NC1 ,
     8       NC2            )
        ELSE
          CALL R4CUM3P(
     1       GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2       GBUF%MOM(II(3)),STI            ,STIR           ,FSKY           ,FSKY           ,
     3       IADR           ,FX1            ,FX2            ,FY1            ,FY2            ,
     4       FZ1            ,FZ2            ,MX1            ,MX2            ,MY1            ,
     5       MY2            ,MZ1            ,MZ2            ,EXX            ,EYX            ,
     6       EZX            ,EXY            ,EYY            ,EZY            ,EXZ            ,
     7       EYZ            ,EZZ            ,AL             )
        ENDIF
C=======================================================================
      ELSEIF (IGTYP >= 29 .AND. IGTYP <= 31) THEN
C=======================================================================
        CALL R4COOR3(X   ,VR  ,IXR ,GBUF%SKEW ,NGL,
     .               X1  ,Y1  ,Z1  ,X2        ,Y2 ,
     .               Z2  ,MGN ,RX1 ,RY1       ,RZ1,
     .               RX2 ,RY2 ,RZ2 ,NC1       ,NC2)
        CALL R5EVEC3(GBUF%SKEW ,V    ,NGL  ,AL   ,X1 ,
     .               Y1        ,Z1   ,X2   ,Y2   ,Z2 ,
     .               EXX       ,EYX  ,EZX  ,EXY  ,EYY,
     .               EZY       ,EXZ  ,EYZ  ,EZZ  ,RX1,
     .               RY1       ,RZ1  ,RX2  ,RY2  ,RZ2,
     .               VX1       ,VX2  ,VY1  ,VY2  ,VZ1,
     .               VZ2       ,NC1  ,NC2  )
        CALL R5DEF3(
     1 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),
     2 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_WAVE             ,FR_W_E              ,
     3 GBUF%EINT            ,GBUF%FOR(II(1))      ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))     ,GBUF%MOM(II(3))      ,
     4 GBUF%FOR(II(2))      ,GBUF%FOR(II(3))      ,PARTSAV             ,IPARTR              ,EXX                  ,
     5 EYX                  ,EZX                  ,EXY                 ,EYY                 ,EZY                  ,
     6 EXZ                  ,EYZ                  ,EZZ                 ,RX1                 ,RY1                  ,
     7 RZ1                  ,RX2                  ,RY2                 ,RZ2                 ,VX1                  ,
     8 VX2                  ,VY1                  ,VY2                 ,VZ1                 ,VZ2                  ,
     9 NC1                  ,NC2                  )
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
     1 JFT                 ,JLT                 ,GBUF%OFF             ,DT2T                 ,NELTST               ,
     2 ITYPTST             ,STI                 ,STIR                 ,MS                   ,IN                   ,
     3 USTI                ,USTIR               ,VISI                 ,VISIR                ,GBUF%MASS            ,
     4 UINER               ,FR_WAVE             ,FR_W_E               ,GBUF%EINT            ,
     5 GBUF%FOR(II(1))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,GBUF%V_REPCVT(II(1)) ,
     6 GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),
     7 AL                  ,GBUF%FOR(II(2))     ,GBUF%FOR(II(3))      ,PARTSAV              ,IPARTR               ,
     8 MSRT                ,DMELRT              ,GBUF%G_DT            ,GBUF%DT              ,NGL                  ,
     9 NC1                 ,NC2                 )
        CALL R5BILAN(GBUF%EINT,PARTSAV,IXR ,GBUF%MASS,V     ,
     2               IPARTR   ,UINER  ,X   ,VR       ,GRESAV,
     3               GRTH     ,IGRTH  ,NC1 ,NC2      ,ITASK)
        CALL R4TORS(GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2              GBUF%MOM(II(3)),TANI           ,AL             ,H3D_DATA       )
        IF (IPARIT == 0) THEN
          CALL R5CUM3(
     1      F              ,GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),AR             ,
     2      GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI            ,STIR           ,
     3      STIFN          ,STIFR          ,FX1            ,FX2            ,FY1            ,
     4      FY2            ,FZ1            ,FZ2            ,MX1            ,MX2            ,
     5      MY1            ,MY2            ,MZ1            ,MZ2            ,GBUF%MOM(II(4)),
     6      GBUF%MOM(II(5)),AL             ,EXX            ,EYX            ,EZX            ,
     7      EXY            ,EYY            ,EZY            ,EXZ            ,EYZ            ,
     8      EZZ            ,NC1            ,NC2            )
        ELSE
          CALL R5CUM3P(
     1      GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2      GBUF%MOM(II(3)),STI            ,STIR           ,FSKY           ,FSKY           ,
     3      IADR           ,FX1            ,FX2            ,FY1            ,FY2            ,
     4      FZ1            ,FZ2            ,MX1            ,MX2            ,MY1            ,
     5      MY2            ,MZ1            ,MZ2            ,GBUF%MOM(II(4)),GBUF%MOM(II(5)),
     6      EXX            ,EYX            ,EZX            ,EXY            ,EYY            ,
     7      EZY            ,EXZ            ,EYZ            ,EZZ            ,AL             )
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 32) THEN
C=======================================================================
        CALL R2COOR3(X   ,VR  ,IXR  ,XDP  ,X1DP,
     2               X2DP,NGL ,X1   ,Y1   ,Z1  ,
     3               X2  ,Y2  ,Z2   ,MGN  ,RX1 ,
     4               RY1 ,RZ1 ,RX2  ,RY2  ,RZ2 ,
     5               NC1 ,NC2 )
        CALL R4EVEC3(GBUF%SKEW ,V             ,EXX2   ,EYX2, EZX2,
     2               EXY2      ,EYY2          ,EZY2   ,EXZ2, EYZ2,
     3               EZZ2      ,AL2DP         ,X1DP   ,X2DP, AL2 ,
     4               ALDP      ,GBUF%SKEW_ERR ,NGL    ,AL   ,EXX ,
     5               EYX       ,EZX           ,EXY    ,EYY  ,EZY ,
     6               EXZ       ,EYZ           ,EZZ    ,RX1  ,RY1 ,
     7               RZ1       ,RX2           ,RY2    ,RZ2  ,VX1 ,
     8               VX2       ,VY1           ,VY2    ,VZ1  ,VZ2 ,
     9               NC1       ,NC2           )
        CALL R5DEF3(
     1 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),
     2 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_WAVE             ,FR_W_E              ,
     3 GBUF%EINT            ,GBUF%FOR(II(1))      ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))     ,GBUF%MOM(II(3))      ,
     4 GBUF%FOR(II(2))      ,GBUF%FOR(II(3))      ,PARTSAV             ,IPARTR              ,EXX                  ,
     5 EYX                  ,EZX                  ,EXY                 ,EYY                 ,EZY                  ,
     6 EXZ                  ,EYZ                  ,EZZ                 ,RX1                 ,RY1                  ,
     7 RZ1                  ,RX2                  ,RY2                 ,RZ2                 ,VX1                  ,
     8 VX2                  ,VY1                  ,VY2                 ,VZ1                 ,VZ2                  ,
     9 NC1                  ,NC2                  )
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
     6 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_W_E              )
C
        DO I=JFT,JLT
         IF (OFF(I) < ONE) GBUF%OFF(I) = OFF(I)
        ENDDO
C
        CALL R5LEN3(
     1 JFT                 ,JLT                 ,GBUF%OFF             ,DT2T                 ,NELTST               ,
     2 ITYPTST             ,STI                 ,STIR                 ,MS                   ,IN                   ,
     3 USTI                ,USTIR               ,VISI                 ,VISIR                ,GBUF%MASS            ,
     4 UINER               ,FR_WAVE             ,FR_W_E               ,GBUF%EINT            ,
     5 GBUF%FOR(II(1))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,GBUF%V_REPCVT(II(1)) ,
     6 GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),
     7 AL                  ,GBUF%FOR(II(2))     ,GBUF%FOR(II(3))      ,PARTSAV              ,IPARTR               ,
     8 MSRT                ,DMELRT              ,GBUF%G_DT            ,GBUF%DT              ,NGL                  ,
     9 NC1                 ,NC2                 )
        CALL R5BILAN(GBUF%EINT,PARTSAV,IXR  ,GBUF%MASS,V     ,
     2               IPARTR   ,UINER  ,X    ,VR       ,GRESAV,
     3               GRTH     ,IGRTH  ,NC1  ,NC2      ,ITASK)
        CALL R4TORS(GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2             GBUF%MOM(II(3)) ,TANI           ,AL             ,H3D_DATA       )
        IF (IPARIT == 0) THEN
          CALL R4CUM3(
     1       F              ,GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),AR  ,
     2       GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI            ,STIR,
     3       STIFN          ,STIFR          ,FX1            ,FX2            ,FY1 ,
     4       FY2            ,FZ1            ,FZ2            ,MX1            ,MX2 ,
     5       MY1            ,MY2            ,MZ1            ,MZ2            ,AL  ,
     6       EXX            ,EYX            ,EZX            ,EXY            ,EYY ,
     7       EZY            ,EXZ            ,EYZ            ,EZZ            ,NC1 ,
     8       NC2            )
        ELSE
          CALL R4CUM3P(
     1       GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2       GBUF%MOM(II(3)),STI            ,STIR           ,FSKY           ,FSKY           ,
     3       IADR           ,FX1            ,FX2            ,FY1            ,FY2            ,
     4       FZ1            ,FZ2            ,MX1            ,MX2            ,MY1            ,
     5       MY2            ,MZ1            ,MZ2            ,EXX            ,EYX            ,
     6       EZX            ,EXY            ,EYY            ,EZY            ,EXZ            ,
     7       EYZ            ,EZZ            ,AL             )
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 33 .OR. IGTYP == 45) THEN
C=======================================================================
C
        NUVAR  = NINT(GEO(25,I0))
	IF (IGTYP == 45) FLG_KJ2 = 1
C
        CALL RGJOINT(
     1     JFT                ,JLT                ,IPARTR             ,NEL               ,NUVAR             ,
     2     IOUT               ,I0                 ,IADR               ,FSKY              ,FSKY              ,
     3     GBUF%OFF           ,GBUF%FOR(II(1))    ,GBUF%FOR(II(2))    ,GBUF%FOR(II(3))   ,GBUF%EINT         ,
     4     GBUF%TOTDEPL(II(1)),GBUF%TOTDEPL(II(2)),GBUF%TOTDEPL(II(3)),GBUF%TOTROT(II(1)),GBUF%TOTROT(II(2)),
     5     GBUF%TOTROT(II(3)) ,GBUF%MOM(II(1))    ,GBUF%MOM(II(2))    ,GBUF%MOM(II(3))   ,GBUF%VAR          ,
     6     STIFN              ,STIFR              ,IXR                ,TANI              ,RBY               ,
     7     X                  ,V                  ,VR                 ,STI               ,STIR              ,
     8     MS                 ,IN                 ,PARTSAV            ,DT1               ,DT2T              ,
     9     F                  ,AR                 ,FX1                ,FX2               ,FY1               ,
     A     FY2                ,FZ1                ,FZ2                ,MX1               ,MX2               ,
     B     MY1                ,MY2                ,MZ1                ,MZ2               ,GRESAV            ,
     C     GRTH               ,IGRTH              ,MSRT               ,DMELRT            ,NELTST            ,
     D     ITYPTST            ,IGTYP              ,SENSOR             ,LSENSOR           ,NC1               ,
     E     NC2                ,XDP                ,GBUF%LENGTH_ERR    ,H3D_DATA)
C=======================================================================
      ELSEIF (IGTYP == 35 .OR. IGTYP == 36) THEN
C=======================================================================
        CALL R4COOR3(X   ,VR  ,IXR ,GBUF%SKEW ,NGL,
     .               X1  ,Y1  ,Z1  ,X2        ,Y2 ,
     .               Z2  ,MGN ,RX1 ,RY1       ,RZ1,
     .               RX2 ,RY2 ,RZ2 ,NC1       ,NC2)
        CALL R5EVEC3(GBUF%SKEW ,V    ,NGL  ,AL   ,X1 ,
     .               Y1        ,Z1   ,X2   ,Y2   ,Z2 ,
     .               EXX       ,EYX  ,EZX  ,EXY  ,EYY,
     .               EZY       ,EXZ  ,EYZ  ,EZZ  ,RX1,
     .               RY1       ,RZ1  ,RX2  ,RY2  ,RZ2,
     .               VX1       ,VX2  ,VY1  ,VY2  ,VZ1,
     .               VZ2       ,NC1  ,NC2  )
        CALL R5DEF3(
     1 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),
     2 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_WAVE             ,FR_W_E              ,
     3 GBUF%EINT            ,GBUF%FOR(II(1))      ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))     ,GBUF%MOM(II(3))      ,
     4 GBUF%FOR(II(2))      ,GBUF%FOR(II(3))      ,PARTSAV             ,IPARTR              ,EXX                  ,
     5 EYX                  ,EZX                  ,EXY                 ,EYY                 ,EZY                  ,
     6 EXZ                  ,EYZ                  ,EZZ                 ,RX1                 ,RY1                  ,
     7 RZ1                  ,RX2                  ,RY2                 ,RZ2                 ,VX1                  ,
     8 VX2                  ,VY1                  ,VY2                 ,VZ1                 ,VZ2                  ,
     9 NC1                  ,NC2                  )
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
     1 JFT                 ,JLT                 ,GBUF%OFF             ,DT2T                 ,NELTST               ,
     2 ITYPTST             ,STI                 ,STIR                 ,MS                   ,IN                   ,
     3 USTI                ,USTIR               ,VISI                 ,VISIR                ,GBUF%MASS            ,
     4 UINER               ,FR_WAVE             ,FR_W_E               ,GBUF%EINT            ,
     5 GBUF%FOR(II(1))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,GBUF%V_REPCVT(II(1)) ,
     6 GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),
     7 AL                  ,GBUF%FOR(II(2))     ,GBUF%FOR(II(3))      ,PARTSAV              ,IPARTR               ,
     8 MSRT                ,DMELRT              ,GBUF%G_DT            ,GBUF%DT              ,NGL                  ,
     9 NC1                 ,NC2                 )
        CALL R5BILAN(GBUF%EINT,PARTSAV,IXR  ,GBUF%MASS,V     ,
     2               IPARTR   ,UINER  ,X    ,VR       ,GRESAV,
     3               GRTH     ,IGRTH  ,NC1  ,NC2      ,ITASK)
        CALL R4TORS(GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2              GBUF%MOM(II(3)),TANI           ,AL             ,H3D_DATA       )
        IF (IPARIT == 0) THEN
          CALL R5CUM3(
     1      F              ,GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),AR             ,
     2      GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI            ,STIR           ,
     3      STIFN          ,STIFR          ,FX1            ,FX2            ,FY1            ,
     4      FY2            ,FZ1            ,FZ2            ,MX1            ,MX2            ,
     5      MY1            ,MY2            ,MZ1            ,MZ2            ,GBUF%MOM(II(4)),
     8      GBUF%MOM(II(5)),AL             ,EXX            ,EYX            ,EZX            ,
     7      EXY            ,EYY            ,EZY            ,EXZ            ,EYZ            ,
     8      EZZ            ,NC1            ,NC2            )
        ELSE
          CALL R5CUM3P(
     1      GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2      GBUF%MOM(II(3)),STI            ,STIR           ,FSKY           ,FSKY           ,
     3      IADR           ,FX1            ,FX2            ,FY1            ,FY2            ,
     4      FZ1            ,FZ2            ,MX1            ,MX2            ,MY1            ,
     5      MY2            ,MZ1            ,MZ2            ,GBUF%MOM(II(4)),GBUF%MOM(II(5)),
     6      EXX            ,EYX            ,EZX            ,EXY            ,EYY            ,
     7      EZY            ,EXZ            ,EYZ            ,EZZ            ,AL             )
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 44) THEN
C=======================================================================
        CALL R4COOR3(X   ,VR  ,IXR ,GBUF%SKEW ,NGL,
     .               X1  ,Y1  ,Z1  ,X2        ,Y2 ,
     .               Z2  ,MGN ,RX1 ,RY1       ,RZ1,
     .               RX2 ,RY2 ,RZ2 ,NC1       ,NC2)
        CALL R5EVEC3(GBUF%SKEW ,V    ,NGL  ,AL   ,X1 ,
     .               Y1        ,Z1   ,X2   ,Y2   ,Z2 ,
     .               EXX       ,EYX  ,EZX  ,EXY  ,EYY,
     .               EZY       ,EXZ  ,EYZ  ,EZZ  ,RX1,
     .               RY1       ,RZ1  ,RX2  ,RY2  ,RZ2,
     .               VX1       ,VX2  ,VY1  ,VY2  ,VZ1,
     .               VZ2       ,NC1  ,NC2  )
        CALL R5DEF3(
     1 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),
     2 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_WAVE             ,FR_W_E              ,
     3 GBUF%EINT            ,GBUF%FOR(II(1))      ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))     ,GBUF%MOM(II(3))      ,
     4 GBUF%FOR(II(2))      ,GBUF%FOR(II(3))      ,PARTSAV             ,IPARTR              ,EXX                  ,
     5 EYX                  ,EZX                  ,EXY                 ,EYY                 ,EZY                  ,
     6 EXZ                  ,EYZ                  ,EZZ                 ,RX1                 ,RY1                  ,
     7 RZ1                  ,RX2                  ,RY2                 ,RZ2                 ,VX1                  ,
     8 VX2                  ,VY1                  ,VY2                 ,VZ1                 ,VZ2                  ,
     9 NC1                  ,NC2                  )
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
     1 JFT                 ,JLT                 ,GBUF%OFF             ,DT2T                 ,NELTST               ,
     2 ITYPTST             ,STI                 ,STIR                 ,MS                   ,IN                   ,
     3 USTI                ,USTIR               ,VISI                 ,VISIR                ,GBUF%MASS            ,
     4 UINER               ,FR_WAVE             ,FR_W_E               ,GBUF%EINT            ,
     5 GBUF%FOR(II(1))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,GBUF%V_REPCVT(II(1)) ,
     6 GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),
     7 AL                  ,GBUF%FOR(II(2))     ,GBUF%FOR(II(3))      ,PARTSAV              ,IPARTR               ,
     8 MSRT                ,DMELRT              ,GBUF%G_DT            ,GBUF%DT              ,NGL                  ,
     9 NC1                 ,NC2                 )
        CALL R5BILAN(GBUF%EINT,PARTSAV,IXR  ,GBUF%MASS,V     ,
     2               IPARTR   ,UINER  ,X    ,VR       ,GRESAV,
     3               GRTH     ,IGRTH  ,NC1  ,NC2      ,ITASK)
        CALL R4TORS(GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2              GBUF%MOM(II(3)),TANI           ,AL             ,H3D_DATA       )
        IF (IPARIT == 0) THEN
          CALL R5CUM3(
     1      F              ,GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),AR             ,
     2      GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI            ,STIR           ,
     3      STIFN          ,STIFR          ,FX1            ,FX2            ,FY1            ,
     4      FY2            ,FZ1            ,FZ2            ,MX1            ,MX2            ,
     5      MY1            ,MY2            ,MZ1            ,MZ2            ,GBUF%MOM(II(4)),
     6      GBUF%MOM(II(5)),AL             ,EXX            ,EYX            ,EZX            ,
     7      EXY            ,EYY            ,EZY            ,EXZ            ,EYZ            ,
     8      EZZ            ,NC1            ,NC2            )
        ELSE
          CALL R5CUM3P(
     1      GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2      GBUF%MOM(II(3)),STI            ,STIR           ,FSKY           ,FSKY           ,
     3      IADR           ,FX1            ,FX2            ,FY1            ,FY2            ,
     4      FZ1            ,FZ2            ,MX1            ,MX2            ,MY1            ,
     5      MY2            ,MZ1            ,MZ2            ,GBUF%MOM(II(4)),GBUF%MOM(II(5)),
     6      EXX            ,EYX            ,EZX            ,EXY            ,EYY            ,
     7      EZY            ,EXZ            ,EYZ            ,EZZ            ,AL             )
        ENDIF
C=======================================================================
      ELSEIF (IGTYP == 46) THEN
C=======================================================================
        CALL R4COOR3(X   ,VR  ,IXR ,GBUF%SKEW ,NGL,
     .               X1  ,Y1  ,Z1  ,X2        ,Y2 ,
     .               Z2  ,MGN ,RX1 ,RY1       ,RZ1,
     .               RX2 ,RY2 ,RZ2 ,NC1       ,NC2)
        CALL R5EVEC3(GBUF%SKEW ,V    ,NGL  ,AL   ,X1 ,
     .               Y1        ,Z1   ,X2   ,Y2   ,Z2 ,
     .               EXX       ,EYX  ,EZX  ,EXY  ,EYY,
     .               EZY       ,EXZ  ,EYZ  ,EZZ  ,RX1,
     .               RY1       ,RZ1  ,RX2  ,RY2  ,RZ2,
     .               VX1       ,VX2  ,VY1  ,VY2  ,VZ1,
     .               VZ2       ,NC1  ,NC2  )
        CALL R5DEF3(
     1 AL                   ,GBUF%V_REPCVT(II(1)) ,GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),
     2 GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),FR_WAVE             ,FR_W_E              ,
     3 GBUF%EINT            ,GBUF%FOR(II(1))      ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))     ,GBUF%MOM(II(3))      ,
     4 GBUF%FOR(II(2))      ,GBUF%FOR(II(3))      ,PARTSAV             ,IPARTR              ,EXX                  ,
     5 EYX                  ,EZX                  ,EXY                 ,EYY                 ,EZY                  ,
     6 EXZ                  ,EYZ                  ,EZZ                 ,RX1                 ,RY1                  ,
     7 RZ1                  ,RX2                  ,RY2                 ,RZ2                 ,VX1                  ,
     8 VX2                  ,VY1                  ,VY2                 ,VZ1                 ,VZ2                  ,
     9 NC1                  ,NC2                  )
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
     1 JFT                 ,JLT                 ,GBUF%OFF             ,DT2T                 ,NELTST               ,
     2 ITYPTST             ,STI                 ,STIR                 ,MS                   ,IN                   ,
     3 USTI                ,USTIR               ,VISI                 ,VISIR                ,GBUF%MASS            ,
     4 UINER               ,FR_WAVE             ,FR_W_E               ,GBUF%EINT            ,
     5 GBUF%FOR(II(1))     ,GBUF%MOM(II(1))     ,GBUF%MOM(II(2))      ,GBUF%MOM(II(3))      ,GBUF%V_REPCVT(II(1)) ,
     6 GBUF%V_REPCVT(II(2)),GBUF%V_REPCVT(II(3)),GBUF%VR_REPCVT(II(1)),GBUF%VR_REPCVT(II(2)),GBUF%VR_REPCVT(II(3)),
     7 AL                  ,GBUF%FOR(II(2))     ,GBUF%FOR(II(3))      ,PARTSAV              ,IPARTR               ,
     8 MSRT                ,DMELRT              ,GBUF%G_DT            ,GBUF%DT              ,NGL                  ,
     9 NC1                 ,NC2                 )
        CALL R5BILAN(GBUF%EINT,PARTSAV,IXR  ,GBUF%MASS,V     ,
     2               IPARTR   ,UINER  ,X    ,VR       ,GRESAV ,
     3               GRTH     ,IGRTH  ,NC1  ,NC2      ,ITASK)
        CALL R4TORS(GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2              GBUF%MOM(II(3)),TANI           ,AL             ,H3D_DATA       )
        IF (IPARIT == 0) THEN
          CALL R5CUM3(
     1      F              ,GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),AR             ,
     2      GBUF%MOM(II(1)),GBUF%MOM(II(2)),GBUF%MOM(II(3)),STI            ,STIR           ,
     3      STIFN          ,STIFR          ,FX1            ,FX2            ,FY1            ,
     4      FY2            ,FZ1            ,FZ2            ,MX1            ,MX2            ,
     5      MY1            ,MY2            ,MZ1            ,MZ2            ,GBUF%MOM(II(4)),
     6      GBUF%MOM(II(5)),AL             ,EXX            ,EYX            ,EZX            ,
     7      EXY            ,EYY            ,EZY            ,EXZ            ,EYZ            ,
     8      EZZ            ,NC1            ,NC2            )
        ELSE
          CALL R5CUM3P(
     1      GBUF%FOR(II(1)),GBUF%FOR(II(2)),GBUF%FOR(II(3)),GBUF%MOM(II(1)),GBUF%MOM(II(2)),
     2      GBUF%MOM(II(3)),STI            ,STIR           ,FSKY           ,FSKY           ,
     3      IADR           ,FX1            ,FX2            ,FY1            ,FY2            ,
     4      FZ1            ,FZ2            ,MX1            ,MX2            ,MY1            ,
     5      MY2            ,MZ1            ,MZ2            ,GBUF%MOM(II(4)),GBUF%MOM(II(5)),
     6      EXX            ,EYX            ,EZX            ,EXY            ,EYY            ,
     7      EZY            ,EXZ            ,EYZ            ,EZZ            ,AL             )
        ENDIF
C-----
      ENDIF  ! IGTYP
C-----------------------------------------------
      RETURN
      END SUBROUTINE RFORC3
Chd|====================================================================
Chd|  R5BILAN                       source/elements/spring/rforc3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE R5BILAN(EINT   ,PARTSAV,IXR  ,UMAS ,V     ,
     2                   IPARTR ,UINER  ,X    ,VR   ,GRESAV,
     3                   GRTH   ,IGRTH  ,NC1  ,NC2  ,ITASK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "scr11_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARTR(*),IXR(NIXR,*),GRTH(*),IGRTH(*),
     .        NC1(*),NC2(*),ITASK
C     REAL
      my_real
     .   UMAS(*),EINT(*),PARTSAV(NPSAV,*),V(3,*),VR(3,*),X(3,*),
     .   UINER(MVSIZ),GRESAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX, FLAG
      my_real
     .   REINTT 
      my_real
     .   X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ),
     .   X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ),
     .   V1(MVSIZ,3),V2(MVSIZ,3),
     .   VR1(MVSIZ,3),VR2(MVSIZ,3),
     .   IN2,EI(MVSIZ),REK(MVSIZ),
     .   EK(MVSIZ), XM(MVSIZ), YM(MVSIZ), ZM(MVSIZ), XMAS2(MVSIZ),
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ), 
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ),RBIDON(1),OFF(MVSIZ),
     .   XX(MVSIZ) , YY(MVSIZ) , ZZ(MVSIZ) ,
     .   XY(MVSIZ) , YZ(MVSIZ) , ZX(MVSIZ) ,
     .   XX2(MVSIZ), YY2(MVSIZ), ZZ2(MVSIZ),
     .   VXA(MVSIZ), VYA(MVSIZ), VZA(MVSIZ),
     .   VA2(MVSIZ), VRA2(MVSIZ)
C-----------------------------------------------
      FLAG = 0
      RBIDON = ZERO
      OFF(1:MVSIZ) = ZERO
      REINTT = ZERO
C
      DO I=LFT,LLT
        REINTT=REINTT + EINT(I)
      ENDDO
C
#include "atomic.inc"
        REINT = REINT + REINTT
#include "atomend.inc"
C
      DO I=LFT,LLT
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        V1(I,1)=V(1,NC1(I))
        V1(I,2)=V(2,NC1(I))
        V1(I,3)=V(3,NC1(I))
        V2(I,1)=V(1,NC2(I))
        V2(I,2)=V(2,NC2(I))
        V2(I,3)=V(3,NC2(I))
        VR1(I,1)=VR(1,NC1(I))
        VR1(I,2)=VR(2,NC1(I))
        VR1(I,3)=VR(3,NC1(I))
        VR2(I,1)=VR(1,NC2(I))
        VR2(I,2)=VR(2,NC2(I))
        VR2(I,3)=VR(3,NC2(I))
      ENDDO

      DO I=LFT,LLT
        XX(I)= X1(I)+X2(I)
        YY(I)= Y1(I)+Y2(I)
        ZZ(I)= Z1(I)+Z2(I)
        XX2(I) = X1(I)*X1(I)+X2(I)*X2(I)
        YY2(I) = Y1(I)*Y1(I)+Y2(I)*Y2(I)
        ZZ2(I) = Z1(I)*Z1(I)+Z2(I)*Z2(I)
        XY(I)  = X1(I)*Y1(I)+X2(I)*Y2(I)
        YZ(I)  = Y1(I)*Z1(I)+Y2(I)*Z2(I)
        ZX(I)  = Z1(I)*X1(I)+Z2(I)*X2(I)
        VXA(I)=V1(I,1)+V2(I,1)
        VYA(I)=V1(I,2)+V2(I,2)
        VZA(I)=V1(I,3)+V2(I,3)
        VA2(I)=V1(I,1)*V1(I,1)+V2(I,1)*V2(I,1)
     .        +V1(I,2)*V1(I,2)+V2(I,2)*V2(I,2)
     .        +V1(I,3)*V1(I,3)+V2(I,3)*V2(I,3)
        VRA2(I) = VR1(I,1)*VR1(I,1)+VR2(I,1)*VR2(I,1)
     .           +VR1(I,2)*VR1(I,2)+VR2(I,2)*VR2(I,2)
     .           +VR1(I,3)*VR1(I,3)+VR2(I,3)*VR2(I,3)
      ENDDO
C
      DO I=LFT,LLT
        XMAS2(I)=UMAS(I)*HALF
        XM(I)= XMAS2(I)*VXA(I)
        YM(I)= XMAS2(I)*VYA(I)
        ZM(I)= XMAS2(I)*VZA(I)
        EK(I)= XMAS2(I)*VA2(I)*HALF
      ENDDO
C
      IF (NPSAV >= 21) THEN
        DO I=LFT,LLT
          XCG(I)= XMAS2(I)*XX(I)
          YCG(I)= XMAS2(I)*YY(I)
          ZCG(I)= XMAS2(I)*ZZ(I)
          IN2 = UINER(I)*HALF
          IXY(I) = -XMAS2(I)*XY(I)
          IYZ(I) = -XMAS2(I)*YZ(I)
          IZX(I) = -XMAS2(I)*ZX(I)
          IXX(I) = UINER(I) + XMAS2(I)*(YY2(I) + ZZ2(I))
          IYY(I) = UINER(I) + XMAS2(I)*(ZZ2(I) + XX2(I))
          IZZ(I) = UINER(I) + XMAS2(I)*(XX2(I) + YY2(I))
C
          XXM(I)= XMAS2(I)*
     .      (V1(I,3)*Y1(I)-V1(I,2)*Z1(I)
     .      +V2(I,3)*Y2(I)-V2(I,2)*Z2(I))
     .           +IN2*(VR1(I,1)+VR2(I,1))
          YYM(I)= XMAS2(I)*
     .      (V1(I,1)*Z1(I)-V1(I,3)*X1(I)
     .      +V2(I,1)*Z2(I)-V2(I,3)*X2(I))
     .           +IN2*(VR1(I,2)+VR2(I,2))
          ZZM(I)= XMAS2(I)*
     .      (V1(I,2)*X1(I)-V1(I,1)*Y1(I)
     .      +V2(I,2)*X2(I)-V2(I,1)*Y2(I))
     .           +IN2*(VR1(I,3)+VR2(I,3))
          REK(I)= IN2*VRA2(I)*HALF
        ENDDO
C
      IF (IGRE /= 0) THEN
          DO I=LFT,LLT
            OFF(I)     = ONE
          ENDDO
          CALL GRELEM_SAV(LFT   ,LLT   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EINT  ,EK    ,XM    ,YM    ,
     3                    ZM    ,UMAS  ,XCG   ,YCG   ,ZCG   ,
     4                    XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                    IZZ   ,IXY   ,IYZ   ,IZX   ,RBIDON,
     6                    REK   ,FLAG  )
      ENDIF
C
        DO I=LFT,LLT
          MX = IPARTR(I)
          PARTSAV(1,MX)=PARTSAV(1,MX) + EINT(I)
          PARTSAV(2,MX)=PARTSAV(2,MX) + EK(I)
          PARTSAV(3,MX)=PARTSAV(3,MX) + XM(I)
          PARTSAV(4,MX)=PARTSAV(4,MX) + YM(I)
          PARTSAV(5,MX)=PARTSAV(5,MX) + ZM(I)
          PARTSAV(6,MX)=PARTSAV(6,MX) + UMAS(I)
          PARTSAV(9,MX) =PARTSAV(9,MX)  + XCG(I)
          PARTSAV(10,MX)=PARTSAV(10,MX) + YCG(I)
          PARTSAV(11,MX)=PARTSAV(11,MX) + ZCG(I)
          PARTSAV(12,MX)=PARTSAV(12,MX) + XXM(I)
          PARTSAV(13,MX)=PARTSAV(13,MX) + YYM(I)
          PARTSAV(14,MX)=PARTSAV(14,MX) + ZZM(I)
          PARTSAV(15,MX)=PARTSAV(15,MX) + IXX(I)
          PARTSAV(16,MX)=PARTSAV(16,MX) + IYY(I)
          PARTSAV(17,MX)=PARTSAV(17,MX) + IZZ(I)
          PARTSAV(18,MX)=PARTSAV(18,MX) + IXY(I)
          PARTSAV(19,MX)=PARTSAV(19,MX) + IYZ(I)
          PARTSAV(20,MX)=PARTSAV(20,MX) + IZX(I)
C COQUE          PARTSAV(21,MX)=PARTSAV(21,MX) + REI(I)
          PARTSAV(22,MX)=PARTSAV(22,MX) + REK(I)
        ENDDO
      ENDIF
      OFF(1:MVSIZ) = ONE
      CALL SENSOR_ENERGY_BILAN(LFT,LLT,EINT,EK,OFF,IPARTR,ITASK)  
C---
      RETURN
      END
Chd|====================================================================
Chd|  R5DEF3                        source/elements/spring/rforc3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R5DEF3(
     1                  XL     ,VX2L  ,RY1L    ,RZ1L   ,RX2L  ,
     2                  RY2L   ,RZ2L  ,FR_WAVE ,FR_W_E ,
     3                  EINT   ,FX    ,XMOM    ,YMOM   ,ZMOM  ,
     4                  FY     ,FZ    ,PARTSAV ,IPARTR ,EXX   ,
     5                  EYX    ,EZX   ,EXY     ,EYY    ,EZY   ,
     6                  EXZ    ,EYZ   ,EZZ     ,RX1    ,RY1   ,
     7                  RZ1    ,RX2   ,RY2     ,RZ2    ,VX1   ,
     8                  VX2    ,VY1   ,VY2     ,VZ1    ,VZ2   ,
     9                  NC1    ,NC2   )
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARTR(*),NC1(*),NC2(*)
C     REAL
      my_real
     .   XL(*), VX2L(*),RY1L(*), RZ1L(*), RX2L(*), RY2L(*), RZ2L(*),
     .   FR_WAVE(*) ,FR_W_E(*),EINT(*) ,  
     .   FX(*), FY(*), FZ(*), XMOM(*), YMOM(*),ZMOM(*),PARTSAV(NPSAV,*),
     .   EXX(MVSIZ), EYX(MVSIZ), EZX(MVSIZ),
     .   EXY(MVSIZ), EYY(MVSIZ), EZY(MVSIZ),
     .   EXZ(MVSIZ), EYZ(MVSIZ), EZZ(MVSIZ), RX1(MVSIZ), RX2(MVSIZ),
     .   RY1(MVSIZ), RY2(MVSIZ), RZ1(MVSIZ), RZ2(MVSIZ), VX1(MVSIZ),
     .   VX2(MVSIZ), VY1(MVSIZ), VY2(MVSIZ), VZ1(MVSIZ), VZ2(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX
C     REAL
      my_real
     .     VX1L, VY1L, VY2L, VZ1L, VZ2L,THETA , XLDEMI,XSIGN
C-----------------------------------------------
C--------------------------------------------
C     VITESSES REPERE CONVECTEE
C--------------------------------------------
      DO I=LFT,LLT
        RX2L(I) = EXX(I)*(RX2(I)-RX1(I))
     .          + EYX(I)*(RY2(I)-RY1(I))
     .          + EZX(I)*(RZ2(I)-RZ1(I))
        RY1L(I) = EXY(I)*RX1(I)+EYY(I)*RY1(I)+EZY(I)*RZ1(I)
        RY2L(I) = EXY(I)*RX2(I)+EYY(I)*RY2(I)+EZY(I)*RZ2(I)
        RZ1L(I) = EXZ(I)*RX1(I)+EYZ(I)*RY1(I)+EZZ(I)*RZ1(I)
        RZ2L(I) = EXZ(I)*RX2(I)+EYZ(I)*RY2(I)+EZZ(I)*RZ2(I)
        VX2L(I) = EXX(I)*(VX2(I)-VX1(I))
     .          + EYX(I)*(VY2(I)-VY1(I))
     .          + EZX(I)*(VZ2(I)-VZ1(I))
        VY2L   = EXY(I)*(VX2(I)-VX1(I))
     .         + EYY(I)*(VY2(I)-VY1(I))
     .         + EZY(I)*(VZ2(I)-VZ1(I))
        XSIGN  = SIGN(ONE, XL(I) - HALF*VX2L(I)*DT1)
        XLDEMI = XSIGN/MAX(EM15,ABS(XL(I) - HALF*VX2L(I)*DT1))
        THETA  = VY2L * XLDEMI
        RZ1L(I) = RZ1L(I) - THETA
        RZ2L(I) = RZ2L(I) - THETA
        VZ2L   = EXZ(I)*(VX2(I)-VX1(I))
     .         + EYZ(I)*(VY2(I)-VY1(I))
     .         + EZZ(I)*(VZ2(I)-VZ1(I))
        THETA  = VZ2L * XLDEMI
        RY1L(I) = RY1L(I) + THETA
        RY2L(I) = RY2L(I) + THETA
        VX2L(I) = VX2L(I)
     .    - HALF*DT1*XLDEMI*(VY2L*VY2L+VZ2L*VZ2L)
      ENDDO
C--------------------------------------------
C     Energy
C--------------------------------------------
      DO I=LFT,LLT
        EINT(I) = EINT(I)
     .+ HALF*DT1 * (VX2L(I) * FX(I) + RX2L(I) * XMOM(I) 
     .          + (RY2L(I) - RY1L(I)) * YMOM(I) 
     .          + (RZ2L(I) - RZ1L(I)) * ZMOM(I)
     .          + HALF * (RY2L(I) + RY1L(I)) * FZ(I) * XL(I)
     .          - HALF * (RZ2L(I) + RZ1L(I)) * FY(I) * XL(I) )
      ENDDO
C
      IF (NPSAV >= 21) THEN
        DO I=LFT,LLT
          MX = IPARTR(I)
          PARTSAV(23,MX)=PARTSAV(23,MX)
     .          + HALF*DT1 * (RX2L(I) * XMOM(I)
     .          + (RY2L(I) - RY1L(I)) * YMOM(I) 
     .          + (RZ2L(I) - RZ1L(I)) * ZMOM(I)
     .          + HALF * (RY2L(I) + RY1L(I)) * FZ(I) * XL(I)
     .          - HALF * (RZ2L(I) + RZ1L(I)) * FY(I) * XL(I) )
        ENDDO
      ENDIF
C--------------------------------------------
C     Front wave
C--------------------------------------------
      IF (IFRWV /= 0) THEN
        DO I=LFT,LLT
          FR_W_E(I)=MAX(FR_WAVE(NC1(I)),FR_WAVE(NC2(I)),ZERO)
        ENDDO
      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  R5LEN3                        source/elements/spring/rforc3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R5LEN3(
     1                  JFT    ,JLT     ,OFF   ,DT2T   ,NELTST ,
     2                  ITYPTST,STI     ,STIR  ,MS     ,IN     ,
     3                  USTI   ,USTIR   ,VISI  ,VISIR  ,UMAS   ,
     4                  UINER  ,FR_WAVE ,FR_W_E,EINT   ,
     5                  FX     ,XMOM    ,YMOM  ,ZMOM   ,VX     ,
     6                  RY1    ,RZ1     ,RX    ,RY2    ,RZ2    ,
     7                  XL     ,FY      ,FZ    ,PARTSAV,IPARTR ,
     8                  MSRT   ,DMELRT  ,G_DT  ,DTEL   ,NGL    ,
     9                  NC1    ,NC2     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(INOUT) :: DTEL(JFT:JLT)
      INTEGER,INTENT(IN)    :: G_DT
      INTEGER JFT,JLT,NELTST ,ITYPTST
      INTEGER IPARTR(*),NGL(*),NC1(*),NC2(*)
      my_real DT2T,
     .   OFF(*), STI(3,*), STIR(3,*), MS(*), IN(*),
     .   USTI(*) ,USTIR(*), VISI(*) ,VISIR(*)  ,UMAS(*) ,
     .   UINER(*),FR_WAVE(*) ,FR_W_E(*) ,EINT(*) ,  
     .   FX(*), FY(*), FZ(*), XMOM(*), YMOM(*),ZMOM(*),XL(*),
     .   VX(*), RY1(*), RZ1(*), RX(*), RY2(*), RZ2(*),PARTSAV(NPSAV,*),
     .   MSRT(*), DMELRT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX
      my_real
     .   DT(MVSIZ), DTC(MVSIZ), 
     .     DTINV, A, MASS2, IN2, DTA, DTB, MX2
C--------------------------------------------
C     OFF
C--------------------------------------------
      DO I=JFT,JLT
        FX(I) = FX(I)*OFF(I)
        FY(I) = FY(I)*OFF(I)
        FZ(I) = FZ(I)*OFF(I)
        XMOM(I) = XMOM(I)*OFF(I)
        YMOM(I) = YMOM(I)*OFF(I)
        ZMOM(I) = ZMOM(I)*OFF(I)
      ENDDO
C--------------------------------------------
C     Energy
C--------------------------------------------
      DO I=JFT,JLT
        EINT(I) = EINT(I)
     .+ HALF*DT1 * (VX(I) * FX(I) + RX(I) * XMOM(I)
     .          + (RY2(I) - RY1(I)) * YMOM(I) 
     .          + (RZ2(I) - RZ1(I)) * ZMOM(I)
     .          + HALF * (RY2(I) + RY1(I)) * FZ(I) * XL(I)
     .          - HALF * (RZ2(I) + RZ1(I)) * FY(I) * XL(I) )
      ENDDO
      IF (NPSAV >= 21) THEN
        DO I=JFT,JLT
          MX = IPARTR(I)
          PARTSAV(23,MX)=PARTSAV(23,MX)
     .          + HALF*DT1 * (RX(I) * XMOM(I) 
     .          + (RY2(I) - RY1(I)) * YMOM(I) 
     .          + (RZ2(I) - RZ1(I)) * ZMOM(I)
     .          + HALF * (RY2(I) + RY1(I)) * FZ(I) * XL(I)
     .          - HALF * (RZ2(I) + RZ1(I)) * FY(I) * XL(I) )
        ENDDO
      ENDIF
C--------------------------------------------
C     Front wave
C--------------------------------------------
      IF (IFRWV /= 0) THEN
#include "lockon.inc"
        DO I=JFT,JLT
          IF (FR_WAVE(NC1(I)) == ZERO)FR_WAVE(NC1(I))=-FR_W_E(I)
          IF (FR_WAVE(NC2(I)) == ZERO)FR_WAVE(NC2(I))=-FR_W_E(I)
        ENDDO
#include "lockoff.inc"
      ENDIF
C--------------------------------------------
C     time step
C--------------------------------------------
      IF (NODADT /= 0 .OR. IDTMINS == 2) THEN
        DO I=JFT,JLT
          USTI(I) =USTI(I) *MAX(ZERO,OFF(I))
          USTIR(I)=USTIR(I)*MAX(ZERO,OFF(I))
          VISI(I) =VISI(I) *MAX(ZERO,OFF(I))
          VISIR(I)=VISIR(I)*MAX(ZERO,OFF(I))
        ENDDO
        IF (DT1 == ZERO)THEN
          DO I=JFT,JLT
            IF (VISIR(I) < EM15) UINER(I) =ONE
            STI(1,I)  = USTI(I) 
            STIR(1,I) = USTIR(I) 
            IF (UMAS(I) > EM15)
     .         STI(1,I) = STI(1,I) + FOUR*VISI(I)**2 / UMAS(I)
            IF (UINER(I) > EM15)
     .         STIR(1,I) = STIR(1,I) + FOUR*VISIR(I)**2 / UINER(I)
            STI(2,I)  = STI(1,I)
            STIR(2,I) = STIR(1,I)
          ENDDO
        ELSE
          DO I=JFT,JLT
            STI(1,I)  = USTI(I) + TWO*VISI(I)/DT1
            STIR(1,I) = USTIR(I)+ TWO*VISIR(I)/DT1
            STI(2,I)  = STI(1,I)
            STIR(2,I) = STIR(1,I)
          ENDDO
        ENDIF
C
        IF (IDTMINS == 2 .AND. JSMS /= 0) THEN
          DTA=DTMINS/DTFACS
          DTB=DTA*DTA
          DO I=JFT,JLT
            IF (OFF(I) <= ZERO) CYCLE
             DT(I)=EP20
             IF (VISI(I)+USTI(I) >= EM15) THEN
               USTI(I)= MAX(EM15,USTI(I))
               DMELRT(I)=MAX(DMELRT(I),
     .           VISI(I)*DTA+HALF*USTI(I)*DTB-HALF*MSRT(I))
C                 MX2 = 2*(Mn+2*DeltaM)
                 MX2 =MSRT(I)+TWO*DMELRT(I)
                 DT(I)=DTFACS*
     .               MX2 /MAX(EM15,SQRT(VISI(I)*VISI(I)+MX2*USTI(I))+VISI(I))
             ENDIF
           ENDDO
C
           DO I=JFT,JLT
            IF (OFF(I) <= ZERO) CYCLE
            IF (DT(I) < DT2T) THEN
              DT2T=DT(I)
              NELTST =NGL(I)
              ITYPTST=6
            ENDIF
          ENDDO
        ENDIF ! IF (IDTMINS == 2 .AND. JSMS /= 0)
      ENDIF ! IF (NODADT /= 0 .OR. IDTMINS == 2)
C
      IF (NODADT /= 0 .OR. (IDTMINS == 2. AND. JSMS /= 0)) RETURN
C
      DO I=JFT,JLT
        IF (VISI(I)+USTI(I) < EM15) UMAS(I) =ONE
      ENDDO
C
      DO I=JFT,JLT
        USTI(I)= MAX(EM15,USTI(I))
        DT(I)=(SQRT(VISI(I)*VISI(I)+UMAS(I)*USTI(I))-VISI(I))/USTI(I)
        DTC(I)=HALF*UMAS(I)/ MAX(EM15,VISI(I))
        DT(I)= MIN(DT(I),DTC(I))
      ENDDO
C
      IF (IDTMINS /= 2) THEN
        DO I=JFT,JLT
          STI(1,I) = ZERO
          STI(2,I) = ZERO
          STIR(1,I) = ZERO
          STIR(2,I) = ZERO
          IF (DT(I) == ZERO) DT(I)=DTC(I)
          IF (OFF(I) <= ZERO) CYCLE     
          STI(1,I) = UMAS(I) / DT(I)**2
          STI(2,I) = STI(1,I)
        ENDDO
      ENDIF
C
      DO I=JFT,JLT
        IF (VISIR(I)+USTIR(I) < EM15) UINER(I)=ONE
      ENDDO
C
      DO I=JFT,JLT
        USTIR(I)= MAX(EM15,USTIR(I))
        DTC(I)=(SQRT(VISIR(I)*VISIR(I)+UINER(I)*USTIR(I))-VISIR(I))
     .         /USTIR(I)
        DT(I)= MIN(DT(I),DTC(I))
        DTC(I)=HALF*UINER(I)/ MAX(EM15,VISIR(I))
        DT(I)= MIN(DT(I),DTC(I))
      ENDDO
C
      DO I=JFT,JLT
        IF (OFF(I) <= ZERO) CYCLE
        IF (DT(I) == ZERO) DT(I)=DTC(I)
        DT(I)=DTFAC1(6)*DT(I)
        IF (IDTMIN(6) == 1 .AND. DT(I) < DTMIN1(6)) THEN
          TSTOP = TT
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
        ELSEIF (IDTMIN(6) == 5 .AND. DT(I) < DTMIN1(6)) THEN
          MSTOP = 2
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPRING ELEMENT'
#include "lockoff.inc"
        ELSEIF (IDTMIN(6) == 2 .AND. DT(I) < DTMIN1(6)) THEN
          OFF(I)=ZERO
#include "lockon.inc"
          WRITE(IOUT,*) '-- DELETE OF SPRING ELEMENT NUMBER',NGL(I)
#include "lockoff.inc"
          IDEL7NOK = 1
        ENDIF
        IF (DT(I) >= DT2T) CYCLE
        DT2T=DT(I)
        NELTST =NGL(I)
        ITYPTST=6
      ENDDO
C------------------------------
        IF(G_DT.NE.ZERO)THEN
           DO I=JFT,JLT
             DTEL(I) = DT(I)
           ENDDO
        ENDIF
C------------------------------
      RETURN
      END
