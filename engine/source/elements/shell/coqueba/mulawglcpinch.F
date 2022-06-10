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
Chd|  MULAWGLCPINCH                 source/elements/shell/coqueba/mulawglcpinch.F
Chd|-- called by -----------
Chd|        CMAIN3PINCH                   source/elements/shell/coqueba/cmain3pinch.F
Chd|-- calls ---------------
Chd|        SIGEPS01GPINCH                source/materials/mat/mat001/sigeps01gpinch.F
Chd|        SIGEPS91GPINCH                source/materials/mat/mat091/sigeps91gpinch.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE MULAWGLCPINCH(ELBUF_STR,
     1           JFT    ,JLT    ,PM     ,FOR   ,MOM   ,THK   ,
     2           EINT   ,OFF    ,GSTR   ,PLA   ,DIR   ,SHF   ,
     3           MAT    ,AREA   ,EXX    ,EYY   ,EXY   ,NEL   ,
     4           EXZ    ,EYZ    ,KXX    ,KYY   ,KXY   ,DM    ,
     5           PID    ,TF     ,NPF    ,MTN   ,DT1C  ,A1    ,
     6           BUFMAT ,SSP    ,RHO    ,VISCMX,IOFC  ,A2    ,
     7           INDX   ,NGL    ,ZCFAC  ,GS    ,SIGY  ,G     ,
     8           THK0   ,EPSP   ,IPLA   ,IGEO  ,IPM   ,TABLE ,
     9           IR     ,IS     ,F_DEF  ,ISMSTR,NU    ,VOL0  ,
     A           KFTS   ,EPINCHZZ       ,EPINCHXZ            ,
     B           EPINCHYZ       ,FORP   ,MOMP  ,ALDT  ,
     C           EZZAVG ,AREAPINCH)
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
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com20_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, MTN, IOFC, IPLA,NEL,IR,IS,ISMSTR,KFTS
      INTEGER MAT(*), PID(*), NPF(*),NGL(*), INDX(*),IPM(NPROPMI,*)
      my_real DM
      my_real FOR(NEL,5), MOM(NEL,3), THK(*), EINT(JLT,2),PM(NPROPM,*),
     .   OFF(*), GSTR(NEL,8), PLA(*), DIR(*),VISCMX(*),
     .   AREA(*),TF(*),DT1C(*),
     .   EXX(*), EYY(*), EXY(*), EXZ(*), EYZ(*), EPSP(*),
     .   KXX(*), KYY(*), KXY(*),BUFMAT(*),SSP(*),RHO(*),
     .   ZCFAC(MVSIZ,2),GS(*),SIGY(*),THK0(*),SHF(*),F_DEF(MVSIZ,8),
     .   A1(MVSIZ),A2(MVSIZ),G(MVSIZ),NU(MVSIZ),VOL0(*),
     .   EPINCHZZ(MVSIZ),EPINCHXZ(MVSIZ),EPINCHYZ(MVSIZ),
     .   FORP(NEL),MOMP(NEL,2),ALDT(MVSIZ),EZZAVG(MVSIZ),AREAPINCH(MVSIZ)
      TYPE(TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGTYP, I, NUVAR ,NINDX,IGEO(NPROPGI,*),
     .        MX,IOFF_DUCT(MVSIZ),ISRATE
C     REAL
      my_real 
     .   DEGMB(MVSIZ) ,DEGFX(MVSIZ) ,THKN(MVSIZ),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSZZ(MVSIZ),
     .   DEPSXY(MVSIZ),DEPSYZ(MVSIZ),DEPSZX(MVSIZ),
     .   SIGOXX(MVSIZ),SIGOYY(MVSIZ),SIGOZZ(MVSIZ),
     .   SIGOXY(MVSIZ),SIGOYZ(MVSIZ),SIGOZX(MVSIZ),
     .   SIGNXX(MVSIZ),SIGNYY(MVSIZ),SIGNZZ(MVSIZ),
     .   SIGNXY(MVSIZ),SIGNYZ(MVSIZ),SIGNZX(MVSIZ),
     .   SIGVXX(MVSIZ),SIGVYY(MVSIZ),SIGVXY(MVSIZ),
     .   EPS_M2,EPS_K2,YOUNG, VISC, VOL2, ASRATE
      my_real
     .   DEPBXX(MVSIZ),DEPBYY(MVSIZ),DEPBXY(MVSIZ),
     .   DEPPXZ(MVSIZ),DEPPYZ(MVSIZ),
     .   MOMOXX(MVSIZ),MOMOYY(MVSIZ),MOMOXY(MVSIZ),
     .   MOMOPXZ(MVSIZ),MOMOPYZ(MVSIZ),
     .   MOMNXX(MVSIZ),MOMNYY(MVSIZ),MOMNXY(MVSIZ),
     .   MOMNPXZ(MVSIZ),MOMNPYZ(MVSIZ),
     .   ETSE(MVSIZ)  ,EPSPL(MVSIZ),EPSP_LOC(MVSIZ)
      my_real,
     .  DIMENSION(:) ,POINTER :: UVAR
      my_real, DIMENSION(MVSIZ) :: DT_INV
C-------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C-------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,1)
C
C     pour les lois user pid(i)=pid(1)
      IGTYP = IGEO(11,PID(1))
      NUVAR = ELBUF_STR%BUFLY(1)%NVAR_MAT
      UVAR  =>ELBUF_STR%BUFLY(1)%MAT(IR,IS,1)%VAR
      IOFF_DUCT(1:MVSIZ) = 0
      VISCMX(1:MVSIZ) = ZERO
C
      DO I=JFT,JLT
        DEGMB(I) = FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
     .           + FOR(I,4)*EYZ(I)+FOR(I,5)*EXZ(I)
     .           + HALF*FORP(I)*EPINCHZZ(I)
        DEGFX(I) = MOM(I,1)*KXX(I)+MOM(I,2)*KYY(I)+MOM(I,3)*KXY(I)
     .           + HALF*MOMP(I,1)*EPINCHXZ(I)+HALF*MOMP(I,2)*EPINCHYZ(I)
      ENDDO
C
      DO I=JFT,JLT
        THKN(I) = THK(I)
      ENDDO
!       compute the inverse of dt and save the result 
      DO I=JFT,JLT
        DT_INV(I) = DT1C(I)/MAX(DT1C(I)**2,EM20)
      ENDDO
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
C     PLASTICITE GLOBALE
C-----------------------
      DO I=JFT,JLT
        SIGNXX(I) = ZERO
        SIGNYY(I) = ZERO
        SIGNZZ(I) = ZERO
        SIGNXY(I) = ZERO
        SIGNYZ(I) = ZERO
        SIGNZX(I) = ZERO
      ENDDO
      DO I=JFT,JLT
        MOMNXX(I) = ZERO
        MOMNYY(I) = ZERO
        MOMNXY(I) = ZERO
        MOMNPXZ(I)=ZERO
        MOMNPYZ(I)=ZERO          
      ENDDO
C
      DO I=JFT,JLT
        DEPSXX(I) = EXX(I)
        DEPSYY(I) = EYY(I)
        DEPSZZ(I) = EPINCHZZ(I)
        DEPSXY(I) = EXY(I)
        DEPSYZ(I) = EYZ(I)
        DEPSZX(I) = EXZ(I)
        SIGOXX(I) = FOR(I,1) 
        SIGOYY(I) = FOR(I,2)
        SIGOZZ(I) = FORP(I)
        SIGOXY(I) = FOR(I,3)
        SIGOYZ(I) = FOR(I,4)
        SIGOZX(I) = FOR(I,5)
      ENDDO
C
      DO I=JFT,JLT
        DEPBXX(I) = KXX(I)
        DEPBYY(I) = KYY(I)
        DEPBXY(I) = KXY(I)
        DEPPXZ(I) = EPINCHXZ(I)
        DEPPYZ(I) = EPINCHYZ(I)
        MOMOXX(I) = MOM(I,1) 
        MOMOYY(I) = MOM(I,2)
        MOMOXY(I) = MOM(I,3)
        MOMOPXZ(I)= MOMP(I,1)
        MOMOPYZ(I)= MOMP(I,2)
      ENDDO
C
      IF(MTN == 1) THEN
        CALL SIGEPS01GPINCH(
     1                 JFT     ,JLT     ,G       ,THKN    ,OFF     ,
     2                 GS      ,A1      ,A2      ,NU      ,THK0    ,
     3                 NEL     ,SSP     ,RHO     ,
     4                 DEPSXX  ,DEPSYY  ,DEPSZZ  ,
     5                 DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     6                 DEPBXX  ,DEPBYY  ,DEPBXY  ,
     7                 DEPPXZ  ,DEPPYZ  ,
     8                 SIGOXX  ,SIGOYY  ,SIGOZZ  ,
     9                 SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     A                 MOMOXX  ,MOMOYY  ,MOMOXY  ,
     B                 MOMOPXZ ,MOMOPYZ ,
     C                 SIGNXX  ,SIGNYY  ,SIGNZZ  ,
     D                 SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     E                 MOMNXX  ,MOMNYY  ,MOMNXY  ,
     F                 MOMNPXZ ,MOMNPYZ)
      ELSEIF(MTN == 91) THEN
        CALL SIGEPS91GPINCH(
     1                 JFT     ,JLT     ,NUVAR   ,BUFMAT   ,RHO      ,
     2                 THKN    ,THK0    ,NEL     ,SSP      ,AREA     ,
     3                 DEPSXX  ,DEPSYY  ,DEPSZZ  ,
     4                 DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5                 DEPBXX  ,DEPBYY  ,DEPBXY  ,
     6                 DEPPXZ  ,DEPPYZ  ,
     7                 SIGOXX  ,SIGOYY  ,SIGOZZ  ,
     8                 SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     9                 MOMOXX  ,MOMOYY  ,MOMOXY  ,
     A                 MOMOPXZ ,MOMOPYZ ,
     B                 SIGNXX  ,SIGNYY  ,SIGNZZ  ,
     C                 SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     D                 MOMNXX  ,MOMNYY  ,MOMNXY  ,
     E                 MOMNPXZ ,MOMNPYZ ,TT      ,UVAR     ,DT_INV   ,
     F                 VISCMX  ,ALDT    ,VOL0    ,IPM      ,MAT      ,
     G                 PLA     ,DEGMB   ,DEGFX   ,
     H                 NGL     ,EZZAVG  ,AREAPINCH)
      ENDIF
C-----------------------
C      FORCES ET MOMENTS
C-----------------------
      DO I=JFT,JLT
        FOR(I,1) = SIGNXX(I)
        FOR(I,2) = SIGNYY(I)
        FOR(I,3) = SIGNXY(I)
        FOR(I,4) = SIGNYZ(I)
        FOR(I,5) = SIGNZX(I)
        MOM(I,1) = MOMNXX(I)
        MOM(I,2) = MOMNYY(I)
        MOM(I,3) = MOMNXY(I)
        FORP(I)  = SIGNZZ(I)
        MOMP(I,1)= MOMNPXZ(I)
        MOMP(I,2)= MOMNPYZ(I)
      ENDDO
C
      DO I=JFT,JLT
        FOR(I,1)  =FOR(I,1)*OFF(I)
        FOR(I,2)  =FOR(I,2)*OFF(I)
        FOR(I,3)  =FOR(I,3)*OFF(I)
        FOR(I,4)  =FOR(I,4)*OFF(I)
        FOR(I,5)  =FOR(I,5)*OFF(I)
        MOM(I,1)  =MOM(I,1)*OFF(I)
        MOM(I,2)  =MOM(I,2)*OFF(I)
        MOM(I,3)  =MOM(I,3)*OFF(I)
        FORP(I)   =FORP(I)*OFF(I)
        MOMP(I,1) =MOMP(I,1)*OFF(I)
        MOMP(I,2) =MOMP(I,2)*OFF(I)
      ENDDO
C
      DO I=JFT,JLT
        DEGMB(I) = DEGMB(I)+FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
     .           + FOR(I,4)*EYZ(I)+FOR(I,5)*EXZ(I)
     .           + HALF*FORP(I)*EPINCHZZ(I)
        DEGFX(I) = DEGFX(I)+MOM(I,1)*KXX(I)+MOM(I,2)*KYY(I)+MOM(I,3)*KXY(I)
     .           + HALF*MOMP(I,1)*EPINCHXZ(I)+HALF*MOMP(I,2)*EPINCHYZ(I)
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
