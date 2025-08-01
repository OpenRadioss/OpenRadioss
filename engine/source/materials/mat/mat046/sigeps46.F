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
!||    sigeps46   ../engine/source/materials/mat/mat046/sigeps46.F
!||--- called by ------------------------------------------------------
!||    m46law     ../engine/source/materials/mat/mat046/m46law.F
!||--- uses       -----------------------------------------------------
!||    ale_mod    ../common_source/modules/ale/ale_mod.F
!||====================================================================
      SUBROUTINE SIGEPS46 (
     1     NEL    ,NUPARAM,NUVAR   ,
     2     TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     , IPT   ,
     B     IPM    ,MAT    ,WXX     ,WYY,WZZ ,MTN     ,DELTAX ,
     C     AIRE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr06_c.inc"
#include      "scr14_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,MAT(NEL),NGL(NEL),IPM(NPROPMI,*),IPT
      my_real TIME,TIMESTEP,UPARAM(*),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   DELTAX(NEL),AIRE(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),WXX(NEL),WYY(NEL),WZZ(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER MTN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ISGS,IADBUF,K
      my_real 
     . VIS,C1,SMAG2,
     . DAV(MVSIZ),DXX(MVSIZ),DYY(MVSIZ),DZZ(MVSIZ),
     . DXY(MVSIZ),DYZ(MVSIZ),DZX(MVSIZ),SS(MVSIZ),
     . CA,VIS2(MVSIZ),VIS2N,VISCPRESSION,
     . VIS1(MVSIZ),DELTA(MVSIZ),FAC,KAPA,E,C,VIS1O,VIS1N,Y2P,YPM
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C CONSTANT
C
      YPM=ONZEP225
      KAPA=ZEP4187
      E=NINEP793
C
C INITIALIZATION
C
      IF(TIME==ZERO .AND. ALE%GLOBAL%INCOMP==1)THEN
       DO I=1,NEL
        SIGOXX(I)=THIRD*(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))
        SIGOYY(I)=SIGOXX(I)
        SIGOZZ(I)=SIGOXX(I)
       ENDDO
      ENDIF
C
      IF(TIME==ZERO .AND. MTN==47)THEN
       DO I=1,NEL
        UVAR(I,1)=YPM
       ENDDO
      ENDIF
C-----------------------------------------------
C     VISCOUS FLOW (L.E.S) 
C-----------------------------------------------
       IADBUF = IPM(7,MAT(1))-1
       VIS  = UPARAM(IADBUF+1)
       C1   = UPARAM(IADBUF+2)
       ISGS = INT(UPARAM(IADBUF+3))
       SMAG2= UPARAM(IADBUF+4)
       CA   = UPARAM(IADBUF+5)
C
C PRESSURE
C
      IF(ALE%GLOBAL%INCOMP==1)THEN
       DO I=1,NEL
        SIGNXX(I)=SIGOXX(I)+C1*(DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))
        SIGNYY(I)=SIGNXX(I)
        SIGNZZ(I)=SIGNXX(I)
        SIGNXY(I)=ZERO
        SIGNYZ(I)=ZERO
        SIGNZX(I)=ZERO
        SOUNDSP(I) = SQRT(C1/RHO(I))
       ENDDO
      ELSE
       DO I=1,NEL
        SIGNXX(I)=C1*(ONE -RHO(I)/RHO0(I))
        SIGNYY(I)=SIGNXX(I)
        SIGNZZ(I)=SIGNXX(I)
        SIGNXY(I)=ZERO
        SIGNYZ(I)=ZERO
        SIGNZX(I)=ZERO
        SOUNDSP(I) = SQRT(C1/RHO(I))
       ENDDO
      ENDIF
C
C VISCOUS STRESS
C TURBULENT VISCOSITY IN SUC CELL
C
      DO I=1,NEL
        DAV(I)=THIRD*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
        DXY(I)=HALF*EPSPXY(I)
        DYZ(I)=HALF*EPSPYZ(I)
        DZX(I)=HALF*EPSPZX(I)
        SS(I)=SQRT(TWO*(EPSPXX(I)**2+EPSPYY(I)**2+EPSPZZ(I)**2+DXY(I)**2+DYZ(I)**2+DZX(I)**2))
        DXX(I)=EPSPXX(I)-DAV(I)
        DYY(I)=EPSPYY(I)-DAV(I)
        DZZ(I)=EPSPZZ(I)-DAV(I)
      ENDDO
      
      IF(MTN==46)THEN
        IF(N2D==0)THEN
          IF(ISGS==3)THEN
             DO I=1,NEL
              DELTA(I)=DELTAX(I)**2
             ENDDO
          ELSE
             DO I=1,NEL
              DELTA(I)=VOLUME(I)**TWO_THIRD
             ENDDO
            ENDIF
        ELSE
          IF(ISGS==3)THEN
             DO I=1,NEL
              DELTA(I)=DELTAX(I)**2
             ENDDO
            ELSE
              DO I=1,NEL
              DELTA(I)=AIRE(I)
              ENDDO
            ENDIF
        ENDIF
        DO I=1,NEL
         VIS1(I)=VIS+SMAG2*SS(I)*DELTA(I)
         VIS2(I)=CA*VIS1(I)
         UVAR(I,1)=VIS1(I)/VIS
        ENDDO
      ELSE
      
C BOUNDARY LAYER
        DO I=1,NEL
         IF(ISGS/=0)THEN
         C=SS(I)*KAPA*DELTAX(I)*DELTAX(I)/VIS
         Y2P=UVAR(I,1)

C ITERATION FOR Y2+ ET EQUIVALENT VISCOSITY (SAME AS K-EPSILON WITH DOMAIN CLOSURE)

          Y2P=C/LOG(E*Y2P)
          Y2P=MAX(Y2P,YPM)
          VIS1N=KAPA*Y2P/LOG(E*Y2P)
          VIS2N=ZERO
          IF(ISGS>=2)VIS2N=KAPA*Y2P
          UVAR(I,1)=Y2P
         ELSE
          VIS1N=ONE
          VIS2N=ZERO
          UVAR(I,1)=ONE
         ENDIF
         VIS1(I)=VIS*VIS1N
         VIS2(I)=VIS*VIS2N
        ENDDO
      ENDIF
C
      DO I=1,NEL
        VIS1(I)=RHO(I)*VIS1(I)
        VIS2(I)=THREE*RHO(I)*VIS2(I)
        VISCPRESSION=VIS2(I)*DAV(I)
        VISCMAX(I) = VIS1(I)+HALF*VIS2(I)
        VIS1(I)=2.*VIS1(I)
        SIGVXX(I)=VIS1(I)*DXX(I)+VISCPRESSION
        SIGVYY(I)=VIS1(I)*DYY(I)+VISCPRESSION
        SIGVZZ(I)=VIS1(I)*DZZ(I)+VISCPRESSION
        SIGVXY(I)=VIS1(I)*DXY(I)
        SIGVYZ(I)=VIS1(I)*DYZ(I)
        SIGVZX(I)=VIS1(I)*DZX(I)
      ENDDO
C
C VORTICITY STORAGE
C
      IF((ANIM_E(10)==1 .OR. ANIM_SE(10)==1) .AND. TIMESTEP/=0.)THEN
        FAC=FOUR/TIMESTEP
        IF(N2D==0)THEN
         DO  I=1,NEL
          UVAR(I,2)=FAC*SQRT(WXX(I)**2+WYY(I)**2+WZZ(I)**2)
         ENDDO
        ELSE
         DO  I=1,NEL
          UVAR(I,2)=FAC*WZZ(I)
         ENDDO
        ENDIF
      ENDIF
C
      RETURN
      END
