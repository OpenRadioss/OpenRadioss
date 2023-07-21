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
Chd|  MULAWGLC                      source/materials/mat_share/mulawglc.F
Chd|-- called by -----------
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|-- calls ---------------
Chd|        ROTO                          source/airbag/roto.F          
Chd|        SIGEPS01G                     source/materials/mat/mat001/sigeps01g.F
Chd|        SIGEPS02G                     source/materials/mat/mat002/sigeps02g.F
Chd|        SIGEPS22G                     source/materials/mat/mat022/sigeps22g.F
Chd|        SIGEPS36G                     source/materials/mat/mat036/sigeps36g.F
Chd|        SIGEPS43G                     source/materials/mat/mat043/sigeps43g.F
Chd|        SIGEPS56G                     source/materials/mat/mat056/sigeps56g.F
Chd|        SIGEPS60G                     source/materials/mat/mat060/sigeps60g.F
Chd|        SIGEPS86G                     source/materials/mat/mat086/sigeps86g.F
Chd|        UROTO                         source/airbag/uroto.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE MULAWGLC(ELBUF_STR,
     1           JFT    ,JLT    ,PM     ,FOR   ,MOM   ,THK   ,
     2           EINT   ,OFF    ,GSTR   ,DIR   ,SHF   ,
     3           MAT    ,AREA   ,EXX    ,EYY   ,EXY   ,NEL   ,
     4           EXZ    ,EYZ    ,KXX    ,KYY   ,KXY   ,DM    ,
     5           PID    ,TF     ,NPF    ,MTN   ,DT1C  ,A1    ,
     6           BUFMAT ,SSP    ,RHO    ,VISCMX,IOFC  ,A2    ,
     7           INDX   ,NGL    ,ZCFAC  ,GS    ,SIGY  ,G     ,
     8           THK0   ,EPSP   ,IPLA   ,IGEO  ,IPM   ,TABLE ,
     9           IR     ,IS     ,F_DEF  ,ISMSTR,NU    ,VOL0  ,
     A           KFTS   )
C-----------------------------------------------
      USE TABLE_MOD
      USE ELBUFDEF_MOD
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
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, MTN, IOFC, IPLA,NEL,IR,IS,ISMSTR,KFTS
      INTEGER MAT(*), PID(*), NPF(*),NGL(*), INDX(*),IPM(NPROPMI,*)
      my_real DM
      my_real FOR(NEL,5), MOM(NEL,3), THK(*), EINT(JLT,2),PM(NPROPM,*),
     .   OFF(*), GSTR(NEL,8), DIR(*),VISCMX(*),
     .   AREA(*),TF(*),DT1C(*),
     .   EXX(*), EYY(*), EXY(*), EXZ(*), EYZ(*), EPSP(*),
     .   KXX(*), KYY(*), KXY(*),BUFMAT(*),SSP(*),RHO(*),
     .   ZCFAC(MVSIZ,2),GS(*),SIGY(*),THK0(*),SHF(*),F_DEF(MVSIZ,8),
     .   A1(MVSIZ),A2(MVSIZ),G(MVSIZ),NU(MVSIZ),VOL0(*)
      TYPE(TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGTYP, I, NUVAR ,VP,NINDX,IGEO(NPROPGI,*),NVARTMP,
     .        MX,IOFF_DUCT(MVSIZ),ISRATE
      my_real 
     .   DEGMB(MVSIZ) ,DEGFX(MVSIZ) ,THKN(MVSIZ),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),EPSXX(MVSIZ) ,EPSYY(MVSIZ) ,EPSXY(MVSIZ),
     .   EPSYZ(MVSIZ) ,EPSZX(MVSIZ) ,EPSPXX(MVSIZ),EPSPYY(MVSIZ),
     .   EPSPXY(MVSIZ),EPSPYZ(MVSIZ),EPSPZX(MVSIZ),SIGOXX(MVSIZ),
     .   SIGOYY(MVSIZ),SIGOXY(MVSIZ),SIGOYZ(MVSIZ),SIGOZX(MVSIZ),
     .   SIGNXX(MVSIZ),SIGNYY(MVSIZ),SIGNXY(MVSIZ),SIGNYZ(MVSIZ),
     .   SIGNZX(MVSIZ),SIGVXX(MVSIZ),SIGVYY(MVSIZ),SIGVXY(MVSIZ),
     .   SIGVYZ(MVSIZ),SIGVZX(MVSIZ),TENS(5,MVSIZ), 
     .   EPS_M2,EPS_K2,YOUNG, VISC, VOL2, ASRATE
      my_real
     .   DEPBXX(MVSIZ),DEPBYY(MVSIZ),DEPBXY(MVSIZ),
     .   MOMOXX(MVSIZ),MOMOYY(MVSIZ),MOMOXY(MVSIZ),
     .   MOMNXX(MVSIZ),MOMNYY(MVSIZ),MOMNXY(MVSIZ),
     .   ETSE(MVSIZ)  ,EPSPL(MVSIZ),EPSP_LOC(MVSIZ)
      my_real,DIMENSION(:) ,POINTER :: UVAR
      my_real, DIMENSION(MVSIZ) :: DT_INV
C-------------------------------------
      INTEGER, DIMENSION(:), POINTER :: VARTMP
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C-----------------------------------------------
C   S o u r c e   L i n es
C-----------------------------------------------
C
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,1)
C
C     for user material laws : pid(i)=pid(1)
      IGTYP  = IGEO(11,PID(1))
      IF(MTN==43.OR.MTN==56.OR.MTN==60.OR.MTN==86) THEN
        NUVAR  = ELBUF_STR%BUFLY(1)%NVAR_MAT
        UVAR  =>ELBUF_STR%BUFLY(1)%MAT(IR,IS,1)%VAR
      ENDIF
      IF(MTN==36) THEN
        NVARTMP= ELBUF_STR%BUFLY(1)%NVARTMP
        VARTMP=>ELBUF_STR%BUFLY(1)%MAT(IR,IS,1)%VARTMP
      ENDIF
      IOFF_DUCT(1:MVSIZ) = 0
      VISCMX(1:MVSIZ) = ZERO
C
      DO I=JFT,JLT
        DEGMB(I) = FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
     +           + FOR(I,4)*EYZ(I)+FOR(I,5)*EXZ(I)
        DEGFX(I) = MOM(I,1)*KXX(I)+MOM(I,2)*KYY(I)+MOM(I,3)*KXY(I)
      ENDDO
C
      DO I=JFT,JLT
        THKN(I) = THK(I)
      ENDDO
!       compute the inverse of dt and save the result 
      DO I=JFT,JLT
        DT_INV(I) = DT1C(I)/MAX(DT1C(I)**2,EM20)
      ENDDO
!
      ETSE(1:MVSIZ) = ZERO      
C-----------------------------------------------------------
C     EPS POINT EQUIVALENT (au sens energetique)
C-----------------------------------------------------------
C     e = 1/t integ[1/2 E (eps_m + k z)^2 dz ]
C     e = 1/2 E eps_eq^2
C     eps_eq = sqrt[ eps_m^2 + 1/12 k^2t^2 ]
      MX = MAT(JFT)
!
      ISRATE = IPM(3,MX)
      IF (ISRATE == 1) THEN
!#include "vectorize.inc" 
        DO I=JFT,JLT
          EPS_K2 = (KXX(I)*KXX(I)+KYY(I)*KYY(I)+KXX(I)*KYY(I)
     .           + FOURTH*KXY(I)*KXY(I)) *THK(I)*THK(I)*ONE_OVER_9
          EPS_M2 = FOUR_OVER_3*(EXX(I)*EXX(I)+EYY(I)*EYY(I)+EXX(I)*EYY(I)
     .           + FOURTH*EXY(I)*EXY(I))
          EPSP_LOC(I) = SQRT(EPS_K2 + EPS_M2)*DT_INV(I)
        END DO
      ELSE IF (ISRATE > 1) THEN
!#include "vectorize.inc" 
        DO I=JFT,JLT
          EPS_M2  = FOUR_OVER_3*(EXX(I)*EXX(I)+EYY(I)*EYY(I)+EXX(I)*EYY(I)
     .            + FOURTH*EXY(I)*EXY(I))
          EPSP_LOC(I) = SQRT(EPS_M2)*DT_INV(I)
        END DO
      ENDIF
!
      IF (ISRATE > 0) THEN    ! strain rate filtering with exponential average
        DO I=JFT,JLT
          ASRATE = MIN(ONE,PM(9,MX)*DT1C(I))
          EPSP(I) = ASRATE*EPSP_LOC(I) + (ONE-ASRATE)*EPSP(I)
          EPSPL(I) = EPSP(I)
        END DO
      ENDIF
C-----------------------
C     GLOBAL PLASTICITY
C-----------------------
      DO I=JFT,JLT
        SIGNXX(I) = ZERO
        SIGNYY(I) = ZERO
        SIGNXY(I) = ZERO
        SIGNYZ(I) = ZERO
        SIGNZX(I) = ZERO
        SIGVXX(I) = ZERO
        SIGVYY(I) = ZERO
        SIGVXY(I) = ZERO
        SIGVYZ(I) = ZERO
        SIGVZX(I) = ZERO
      ENDDO
      DO I=JFT,JLT
        MOMNXX(I) = ZERO
        MOMNYY(I) = ZERO
        MOMNXY(I) = ZERO          
      ENDDO
C
      IF (IGTYP == 1) THEN
        DO I=JFT,JLT
          DEPSXX(I) = EXX(I)
          DEPSYY(I) = EYY(I)
          DEPSXY(I) = EXY(I)
          DEPSYZ(I) = EYZ(I)
          DEPSZX(I) = EXZ(I)
          EPSXX(I) = GSTR(I,1)
          EPSYY(I) = GSTR(I,2)
          EPSXY(I) = GSTR(I,3)
          EPSYZ(I) = GSTR(I,4)
          EPSZX(I) = GSTR(I,5)
          SIGOXX(I) = FOR(I,1) 
          SIGOYY(I) = FOR(I,2)
          SIGOXY(I) = FOR(I,3)
          SIGOYZ(I) = FOR(I,4)
          SIGOZX(I) = FOR(I,5)
        ENDDO
        DO I=JFT,JLT
          DEPBXX(I) = KXX(I)
          DEPBYY(I) = KYY(I)
          DEPBXY(I) = KXY(I)
          MOMOXX(I) = MOM(I,1) 
          MOMOYY(I) = MOM(I,2)
          MOMOXY(I) = MOM(I,3)
         ENDDO
      ELSE
        DO I=JFT,JLT
          TENS(1,I) = EXX(I)
          TENS(2,I) = EYY(I)
          TENS(3,I) = HALF*EXY(I)
          TENS(4,I) = HALF*EYZ(I)
          TENS(5,I) = HALF*EXZ(I)          
        ENDDO
        CALL ROTO(JFT,JLT,TENS,DIR,NEL)
        DO I=JFT,JLT
          DEPSXX(I) = TENS(1,I)
          DEPSYY(I) = TENS(2,I)
          DEPSXY(I) = TWO*TENS(3,I)
          DEPSYZ(I) = TWO*TENS(4,I)
          DEPSZX(I) = TWO*TENS(5,I)          
        ENDDO
        DO I=JFT,JLT
          TENS(1,I) = GSTR(I,1)
          TENS(2,I) = GSTR(I,2)
          TENS(3,I) = HALF*GSTR(I,3)
          TENS(4,I) = HALF*GSTR(I,4)
          TENS(5,I) = HALF*GSTR(I,5)          
        ENDDO
        CALL ROTO(JFT,JLT,TENS,DIR,NEL)
        DO I=JFT,JLT
          EPSXX(I) = TENS(1,I)
          EPSYY(I) = TENS(2,I)
          EPSXY(I) = TWO*TENS(3,I)
          EPSYZ(I) = TWO*TENS(4,I)
          EPSZX(I) = TWO*TENS(5,I)          
        ENDDO
        DO I=JFT,JLT
          TENS(1,I) = FOR(I,1)
          TENS(2,I) = FOR(I,2)
          TENS(3,I) = FOR(I,3)
          TENS(4,I) = FOR(I,4)
          TENS(5,I) = FOR(I,5)
        ENDDO
        CALL ROTO(JFT,JLT,TENS,DIR,NEL)
        DO I=JFT,JLT
          SIGOXX(I) = TENS(1,I)
          SIGOYY(I) = TENS(2,I)
          SIGOXY(I) = TENS(3,I)
          SIGOYZ(I) = TENS(4,I)
          SIGOZX(I) = TENS(5,I)
        ENDDO
        DO I=JFT,JLT
          TENS(1,I) = KXX(I)
          TENS(2,I) = KYY(I)
          TENS(3,I) = KXY(I)
          TENS(4,I) = ZERO
          TENS(5,I) = ZERO          
        ENDDO
        CALL ROTO(JFT,JLT,TENS,DIR,NEL)
        DO I=JFT,JLT
          DEPBXX(I) = TENS(1,I)
          DEPBYY(I) = TENS(2,I)
          DEPBXY(I) = TENS(3,I)
        ENDDO
        DO I=JFT,JLT
          TENS(1,I) = MOM(I,1)
          TENS(2,I) = MOM(I,2)
          TENS(3,I) = MOM(I,3)
          TENS(4,I) = ZERO
          TENS(5,I) = ZERO          
        ENDDO
        CALL ROTO(JFT,JLT,TENS,DIR,NEL)
        DO I=JFT,JLT
          MOMOXX(I) = TENS(1,I)
          MOMOYY(I) = TENS(2,I)
          MOMOXY(I) = TENS(3,I)
        ENDDO
      ENDIF ! IF (IGTYP == 1)
C
      DO I=JFT,JLT
        EPSPXX(I) = DEPSXX(I)*DT_INV(I)
        EPSPYY(I) = DEPSYY(I)*DT_INV(I)
        EPSPXY(I) = DEPSXY(I)*DT_INV(I)
        EPSPYZ(I) = DEPSYZ(I)*DT_INV(I)
        EPSPZX(I) = DEPSZX(I)*DT_INV(I)
      ENDDO
C-------------------------------------------
C      ELASTIC STRESS + 
C      PLASTICLY ADMISSIBLE STRESS
C-------------------------------------------
      IF (MTN == 1) THEN
        CALL SIGEPS01G(JFT     ,JLT     ,G       ,THKN    ,OFF     ,
     1                 GS      ,A1      ,A2      ,NU      ,THK0    ,
     2                 NEL     ,F_DEF   ,ISMSTR  ,DEPSXX  ,DEPSYY  ,
     3                 DEPSXY  ,DEPSYZ  ,DEPSZX  ,DEPBXX  ,DEPBYY  ,
     4                 DEPBXY  ,SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,
     5                 SIGOZX  ,MOMOXX  ,MOMOYY  ,MOMOXY  ,SIGNXX  ,
     6                 SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,MOMNXX  ,
     7                 MOMNYY  ,MOMNXY  )
      ELSEIF (MTN == 2) THEN
        VP = IPM(255,MX)
        CALL SIGEPS02G(ELBUF_STR,
     1                 JFT     ,JLT     ,PM      ,FOR     ,MOM     ,
     2                 THKN    ,EINT    ,OFF     ,DT1C    ,ISRATE  ,
     3                 G       ,A1      ,A2      ,VOL0    ,NU      ,
     4                 THK0    ,GS      ,EPSP    ,IOFC    ,KFTS    ,
     5                 NGL     ,INDX    ,IPLA    ,IR      ,IS      ,
     6                 DEGMB   ,DEGFX   ,DEPSXX  ,DEPSYY  ,MX      ,
     7                 DEPSXY  ,DEPSYZ  ,DEPSZX  ,DEPBXX  ,DEPBYY  ,
     8                 DEPBXY  ,SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,
     9                 SIGOZX  ,MOMOXX  ,MOMOYY  ,MOMOXY  ,SIGNXX  ,
     A                 SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,MOMNXX  ,
     B                 MOMNYY  ,MOMNXY  ,ETSE    ,EXZ     ,EYZ     ,
     C                 NEL    ,IOFF_DUCT,VP      , NUVAR  ,UVAR )
      ELSEIF (MTN == 22) THEN
        CALL SIGEPS22G(ELBUF_STR,
     1            JFT     ,JLT     ,PM      ,FOR     ,MOM      ,
     2            THKN    ,OFF     ,DT1C    ,NU      ,THK0     ,
     3            GS      ,EPSP    ,IOFC    ,INDX    ,NEL      ,
     4            NGL     ,EXZ     ,EYZ     ,MX      ,IOFF_DUCT,
     5            DEGMB   ,DEGFX   ,DEPSXX  ,DEPSYY  ,DEPSXY   ,
     6            DEPSYZ  ,DEPSZX  ,DEPBXX  ,DEPBYY  ,DEPBXY   ,
     7            SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX   ,
     8            MOMOXX  ,MOMOYY  ,MOMOXY  ,SIGNXX  ,SIGNYY   ,
     9            SIGNXY  ,SIGNYZ  ,SIGNZX  ,MOMNXX  ,MOMNYY   ,
     A            MOMNXY  )
      ELSEIF (MTN == 36) THEN
        CALL SIGEPS36G(
     1     JLT    ,NPF     ,
     2     TF     ,TT     ,BUFMAT  ,RHO     ,
     3     AREA   ,EINT   ,THK0    ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     5     DEPBXX ,DEPBYY ,DEPBXY  ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     7     MOMOXX ,MOMOYY ,MOMOXY  ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     8     MOMNXX ,MOMNYY ,MOMNXY  ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SSP    ,VISCMX ,THKN    ,LBUF%PLA     ,
     B     OFF    ,NGL    ,IPM     ,MAT     ,ETSE   ,
     C     GS     ,SIGY   ,EPSPL   ,ISRATE  ,IPLA, 
     D     SHF    ,NVARTMP,VARTMP)
      ELSEIF (MTN == 43) THEN
        CALL SIGEPS43G(
     1     JLT    ,NUVAR  ,NPF     ,
     2     TF     ,TT     ,BUFMAT  ,RHO     ,
     3     AREA   ,EINT   ,THK0    ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     5     DEPBXX ,DEPBYY ,DEPBXY  ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     7     MOMOXX ,MOMOYY ,MOMOXY  ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     8     MOMNXX ,MOMNYY ,MOMNXY  ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SSP    ,VISCMX ,THKN    ,LBUF%PLA,UVAR   ,
     B     OFF    ,NGL    ,IPM     ,MAT     ,ETSE   ,
     C     GS     ,SIGY   ,SHF     ,LBUF%SEQ,EPSP   )
      ELSEIF (MTN == 56) THEN
        CALL SIGEPS56G(
     1     JLT    ,NUVAR  ,NPF     ,
     2     TF     ,TT     ,BUFMAT  ,RHO     ,
     3     AREA   ,EINT   ,THK0    ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     5     DEPBXX ,DEPBYY ,DEPBXY  ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     7     MOMOXX ,MOMOYY ,MOMOXY  ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     8     MOMNXX ,MOMNYY ,MOMNXY  ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SSP    ,VISCMX ,THKN    ,LBUF%PLA,UVAR   ,
     B     OFF    ,NGL    ,IPM     ,MAT     ,ETSE   ,
     C     GS     ,SIGY   ,EPSPL   ,ISRATE  ,IPLA)
      ELSEIF (MTN == 60) THEN
        CALL SIGEPS60G(
     1     JLT    ,NUVAR  ,NPF     ,
     2     TF     ,TT     ,BUFMAT  ,RHO     ,
     3     AREA   ,EINT   ,THK0    ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     5     DEPBXX ,DEPBYY ,DEPBXY  ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     7     MOMOXX ,MOMOYY ,MOMOXY  ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     8     MOMNXX ,MOMNYY ,MOMNXY  ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SSP    ,VISCMX ,THKN    ,LBUF%PLA,UVAR   ,
     B     OFF    ,NGL    ,IPM      ,MAT     ,ETSE   ,
     C     GS     ,SIGY   ,EPSPL   ,ISRATE  ,IPLA    ,
     D     SHF    )
      ELSEIF (MTN == 86) THEN
        CALL SIGEPS86G(
     1     JLT    ,NUVAR  ,NPF     ,
     2     TF     ,TT     ,BUFMAT  ,RHO     ,
     3     AREA   ,EINT   ,THK0    ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     5     DEPBXX ,DEPBYY ,DEPBXY  ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     7     MOMOXX ,MOMOYY ,MOMOXY  ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     8     MOMNXX ,MOMNYY ,MOMNXY  ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SSP    ,VISCMX ,THKN    ,LBUF%PLA,UVAR   ,
     B     OFF    ,NGL    ,IPM     ,MAT     ,ETSE   ,
     C     GS     ,SIGY   ,EPSPL   ,ISRATE  ,IPLA)
      ENDIF ! IF (MTN == 1) THEN
C-----------------------
C      ORTHOTROPE
C-----------------------
      IF (IGTYP /= 1) THEN
        DO I=JFT,JLT
          TENS(1,I)= SIGNXX(I)
          TENS(2,I)= SIGNYY(I)
          TENS(3,I)= SIGNXY(I)
          TENS(4,I)= SIGNYZ(I)
          TENS(5,I)= SIGNZX(I)
        ENDDO
        CALL UROTO(JFT,JLT,TENS,DIR,NEL)
        DO I=JFT,JLT
          SIGNXX(I)= TENS(1,I)
          SIGNYY(I)= TENS(2,I)
          SIGNXY(I)= TENS(3,I)
          SIGNYZ(I)= TENS(4,I)
          SIGNZX(I)= TENS(5,I)
        ENDDO
        DO I=JFT,JLT
          TENS(1,I)= SIGVXX(I)
          TENS(2,I)= SIGVYY(I)
          TENS(3,I)= SIGVXY(I)
          TENS(4,I)= SIGVYZ(I)
          TENS(5,I)= SIGVZX(I)
        ENDDO
        CALL UROTO(JFT,JLT,TENS,DIR,NEL)
        DO I=JFT,JLT
          SIGVXX(I)= TENS(1,I)
          SIGVYY(I)= TENS(2,I)
          SIGVXY(I)= TENS(3,I)
          SIGVYZ(I)= TENS(4,I)
          SIGVZX(I)= TENS(5,I)
        ENDDO
        DO I=JFT,JLT
          TENS(1,I)= MOMNXX(I)
          TENS(2,I)= MOMNYY(I)
          TENS(3,I)= MOMNXY(I)
          TENS(4,I)= ZERO
          TENS(5,I)= ZERO 
        ENDDO
        CALL UROTO(JFT,JLT,TENS,DIR,NEL)
        DO I=JFT,JLT
          MOMNXX(I)= TENS(1,I)
          MOMNYY(I)= TENS(2,I)
          MOMNXY(I)= TENS(3,I)
        ENDDO
      ENDIF ! IF (IGTYP /= 1)
C-----------------------
C      FORCES ET MOMENTS
C-----------------------
      DO I=JFT,JLT
        FOR(I,1) = SIGNXX(I)+SIGVXX(I)
        FOR(I,2) = SIGNYY(I)+SIGVYY(I)
        FOR(I,3) = SIGNXY(I)+SIGVXY(I)
        FOR(I,4) = SIGNYZ(I)+SIGVYZ(I)
        FOR(I,5) = SIGNZX(I)+SIGVZX(I)
        MOM(I,1) = MOMNXX(I)
        MOM(I,2) = MOMNYY(I)
        MOM(I,3) = MOMNXY(I)
      ENDDO
C----------------------
C     THICKNESS UPDATE
C----------------------
      DO I=JFT,JLT
        THK(I) = MAX(THKN(I),EM30)
      ENDDO
C-----------------------------------------------
C     FACTEURS POUR COQUES B.L. (Zeng&Combescure)
C-----------------------------------------------
      DO I=JFT,JLT
        ZCFAC(I,1) = ETSE(I)
        ZCFAC(I,2) = ZCFAC(I,1)
      ENDDO
C---------------------------
C     VISCOSITE DE MEMBRANE
C---------------------------
      IF (DM > ZERO) THEN
        DO I=JFT,JLT                                                   
          VISCMX(I) = MAX(VISCMX(I),DM)
          VISC = ONEP414*DM*RHO(I)*SSP(I)*SQRT(AREA(I))*DT_INV(I)        
          FOR(I,1) = FOR(I,1) + VISC*(EXX(I)+HALF*EYY(I))		  
          FOR(I,2) = FOR(I,2) + VISC*(EYY(I)+HALF*EXX(I))		  
          FOR(I,3) = FOR(I,3) + VISC* EXY(I)*THIRD			     
        ENDDO
      ENDIF
C
      DO I=JFT,JLT
        FOR(I,1)=FOR(I,1)*OFF(I)
        FOR(I,2)=FOR(I,2)*OFF(I)
        FOR(I,3)=FOR(I,3)*OFF(I)
        FOR(I,4)=FOR(I,4)*OFF(I)
        FOR(I,5)=FOR(I,5)*OFF(I)
        MOM(I,1)=MOM(I,1)*OFF(I)
        MOM(I,2)=MOM(I,2)*OFF(I)
        MOM(I,3)=MOM(I,3)*OFF(I)
      ENDDO
C
      DO I=JFT,JLT
        DEGMB(I) = DEGMB(I)+      FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)
     .          + FOR(I,3)*EXY(I)+FOR(I,4)*EYZ(I)+FOR(I,5)*EXZ(I)
        DEGFX(I) = DEGFX(I)+MOM(I,1)*KXX(I)+MOM(I,2)*KYY(I)
     .                     +MOM(I,3)*KXY(I)
        VOL2 = HALF*THK0(I)*AREA(I)*OFF(I)
        EINT(I,1) = EINT(I,1) + DEGMB(I)*VOL2
        EINT(I,2) = EINT(I,2) + DEGFX(I)*THK0(I)*VOL2
      ENDDO
C----------------------------
C     TEST DE RUPTURE DUCTILE
C     INDX utilise dans IELOF
C----------------------------
      NINDX=0
      DO I=JFT,JLT
        IF (OFF(I) == FOUR_OVER_5 . AND. IOFF_DUCT(I) == 0) THEN
          OFF(I)= ZERO
          NINDX=NINDX+1
          INDX(NINDX)=I
        ENDIF
      ENDDO
      IF (NINDX > 0) THEN
        IDEL7NOK = 1
        IF (IMCONV == 1) THEN
          DO I = 1, NINDX
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(INDX(I))
            WRITE(ISTDO,1100) NGL(INDX(I)),TT
#include "lockoff.inc"
          ENDDO
        ENDIF
      ENDIF
      IOFC = NINDX
C-----------
 1000 FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT :',I10,' AT TIME :',G11.4)
C-----------
      RETURN
      END
