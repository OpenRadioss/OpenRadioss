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
!||    daasolv                ../engine/source/fluid/daasolv.F
!||--- called by ------------------------------------------------------
!||    flow0                  ../engine/source/fluid/flow0.F
!||--- calls      -----------------------------------------------------
!||    arret                  ../engine/source/system/arret.F
!||    finter                 ../engine/source/tools/curve/finter.F
!||    finter_smooth          ../engine/source/tools/curve/finter_smooth.F
!||--- uses       -----------------------------------------------------
!||    python_funct_mod       ../common_source/modules/python_mod.F90
!||    sensor_mod             ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE DAASOLV(NDIM,   NNO,    NEL,   
     .                   IFLOW,  IBUF,   ELEM,  SHELL_GA,
     .                   RFLOW,  NORMAL, TA,    AREAF, COSG,  DIST,  
     .                   MFLE,   ACCF,   PM,    PTI,   X, V, A, NPC, TF, 
     .                   NBGAUGE,LGAUGE, GAUGE, NSENSOR, SENSOR_TAB, IGRV, AGRV,
     .                   CBEM   ,IPIV  , NFUNCT, PYTHON,WFEXT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
      USE PYTHON_FUNCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
!#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDIM, NNO, NEL, NBGAUGE, NSENSOR, NFUNCT
      INTEGER IFLOW(*), IBUF(*), ELEM(NDIM,*), SHELL_GA(*), NPC(*), LGAUGE(3,*)
      INTEGER IGRV(NIGRV,*), IPIV(*)
      my_real X(3,*), V(3,*), A(3,*), TF(*), RFLOW(*)
      my_real NORMAL(3,*), TA(*), AREAF(*), COSG(*), DIST(*), PM(*), ACCF(*), PTI(*)
      my_real MFLE(NEL,*), GAUGE(LLGAUGE,*),AGRV(LFACGRV,*)
      my_real CBEM(NEL,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE (PYTHON_), INTENT(INOUT) :: PYTHON
      DOUBLE PRECISION,INTENT(INOUT) :: WFEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#if defined(MYREAL8) && !defined(WITHOUT_LINALG)
      INTEGER  I, J, K, I1, I2, I3, I4, I5, N1, N2, N3, N4, II, JJ, KK
      INTEGER  IFPRES, JFORM, KFORM, IPRES, IWAVE, INTEGR, FREESURF, AFTERFLOW
      INTEGER  GRAV_ID, IFUNC, ISENS, LENBUF, NNO_L, NEL_L, NELMAX, INFO,ISMOOTH
      my_real  X1,  Y1,  Z1,  X2,  Y2,  Z2,  X3,  Y3,  Z3,  X4,  Y4,  Z4,
     .         X0,  Y0,  Z0,  X12, Y12, Z12, X13, Y13, Z13, X24, Y24, Z24,    
     .         NRX, NRY, NRZ, AREA2, WF(2), WI(4,2),
     .         PM1, DSC, VX0, VY0, VZ0
      my_real  RHO, SSP, RHOC, TTA, DYDX, FF, SFPRES, DPPDT, PMAX, THETA, NORM, FAC1, TS, GRAVITY
      my_real  DIRWIX, DIRWIY, DIRWIZ, DIRWRX, DIRWRY, DIRWRZ
      my_real  OFF(NEL), PP(NEL), VI(NEL), PS(NEL), FEL(NEL), PR(NEL), VR(NEL)
      my_real  DPIDT(NEL), DPRDT(NEL), DPMDT(NEL), HH(NEL)
      my_real  XA,  YA,  ZA,  FCX,  FCY, PMIN, PTOT
      my_real  APMAX, ATHETA, RATIO

      my_real  FINTER,FINTER_SMOOTH
      EXTERNAL FINTER,FINTER_SMOOTH
      my_real  VECT(NEL), VECT1(NEL), VEL(NEL)
      my_real  XL(3,NNO), VL(3,NNO)

C Coordonnees et vitesses locales
      DO I=1,NNO
         II=IBUF(I)
         XL(1,I)=X(1,II)
         XL(2,I)=X(2,II)
         XL(3,I)=X(3,II)
         VL(1,I)=V(1,II)
         VL(2,I)=V(2,II)
         VL(3,I)=V(3,II)
      ENDDO

      JFORM  = IFLOW(4)
      IPRES  = IFLOW(21)
      IWAVE  = IFLOW(22)
      KFORM  = IFLOW(23)
      INTEGR = IFLOW(24)
      FREESURF  = IFLOW(25)
      AFTERFLOW = IFLOW(26)
      GRAV_ID   = IFLOW(27)
      NELMAX    = IFLOW(28)
      RHOC   = RFLOW(1)
      SSP    = RFLOW(2)
      RHO    = RFLOW(5)
      FAC1   = RFLOW(8)
      DSC    = RFLOW(12)
      XA     = RFLOW(16)
      YA     = RFLOW(17)
      ZA     = RFLOW(18)
      PMIN   = RFLOW(22)
      APMAX  = RFLOW(23)
      ATHETA = RFLOW(24)

      IF(JFORM == 1) THEN
        DO I = 1,NEL
           I1 = ELEM(1,I)
           I2 = ELEM(2,I)
           I3 = ELEM(3,I)
           X1 = XL(1,I1)
           X2 = XL(1,I2)
           X3 = XL(1,I3)
           Y1 = XL(2,I1)
           Y2 = XL(2,I2)
           Y3 = XL(2,I3)
           Z1 = XL(3,I1)
           Z2 = XL(3,I2)
           Z3 = XL(3,I3)
           X12= X2-X1
           Y12= Y2-Y1
           Z12= Z2-Z1
           X13= X3-X1
           Y13= Y3-Y1
           Z13= Z3-Z1
           NRX= Y12*Z13-Z12*Y13
           NRY= Z12*X13-X12*Z13
           NRZ= X12*Y13-Y12*X13
           AREA2 = SQRT(NRX**2+NRY**2+NRZ**2)
           NORMAL(1,I) = NRX/AREA2
           NORMAL(2,I) = NRY/AREA2
           NORMAL(3,I) = NRZ/AREA2
           AREAF(I) = HALF*AREA2
           IF(IWAVE == 1)THEN
C Spherical wave
             X0=THIRD*X1+THIRD*X2+THIRD*X3
             Y0=THIRD*Y1+THIRD*Y2+THIRD*Y3
             Z0=THIRD*Z1+THIRD*Z2+THIRD*Z3
             DIRWIX=X0-RFLOW(9)
             DIRWIY=Y0-RFLOW(10)
             DIRWIZ=Z0-RFLOW(11)
             NORM=SQRT(DIRWIX**2+DIRWIY**2+DIRWIZ**2)
             DIRWIX=DIRWIX/NORM
             DIRWIY=DIRWIY/NORM
             DIRWIZ=DIRWIZ/NORM
             IF(FREESURF == 2) THEN
               DIRWRX=X0-RFLOW(13)
               DIRWRY=Y0-RFLOW(14)
               DIRWRZ=Z0-RFLOW(15)
               NORM=SQRT(DIRWRX**2+DIRWRY**2+DIRWRZ**2)
               DIRWRX=DIRWRX/NORM
               DIRWRY=DIRWRY/NORM
               DIRWRZ=DIRWRZ/NORM
                     COSG(I+NEL)=NORMAL(1,I)*DIRWRX+NORMAL(2,I)*DIRWRY+NORMAL(3,I)*DIRWRZ
             ENDIF
C hydrostatic pressure
             HH(I)=ZERO
             IF(GRAV_ID > 0) THEN
               HH(I)=MAX(ZERO,(XA-X0)*RFLOW(19)+(YA-Y0)*RFLOW(20)+(ZA-Z0)*RFLOW(21))
             ENDIF
           ELSEIF(IWAVE == 2) THEN
C Onde plane
             DIRWIX=RFLOW(9)
             DIRWIY=RFLOW(10)
             DIRWIZ=RFLOW(11)
C hydrostatic pressure
             HH(I)=ZERO
           ENDIF
                 COSG(I)=NORMAL(1,I)*DIRWIX+NORMAL(2,I)*DIRWIY+NORMAL(3,I)*DIRWIZ
C vitesse N-1/2
           VX0 = THIRD*VL(1,I1)+THIRD*VL(1,I2)+THIRD*VL(1,I3)
           VY0 = THIRD*VL(2,I1)+THIRD*VL(2,I2)+THIRD*VL(2,I3)
           VZ0 = THIRD*VL(3,I1)+THIRD*VL(3,I2)+THIRD*VL(3,I3)
           VEL(I) = VX0*NORMAL(1,I)+VY0*NORMAL(2,I)+VZ0*NORMAL(3,I)
        ENDDO
      ELSEIF(JFORM == 2) THEN
        WI(1,1)=FOURTH
        WI(2,1)=FOURTH
        WI(3,1)=FOURTH
        WI(4,1)=FOURTH
        WI(1,2)=THIRD
        WI(2,2)=THIRD
        WI(3,2)=ONE_OVER_6
        WI(4,2)=ONE_OVER_6
        DO I = 1,NEL
           I1 = ELEM(1,I)
           I2 = ELEM(2,I)
           I3 = ELEM(3,I)
           I4 = ELEM(4,I)
           I5 = ELEM(5,I)
           X1 = XL(1,I1)
           X2 = XL(1,I2)
           X3 = XL(1,I3)
           X4 = XL(1,I4)
           Y1 = XL(2,I1)
           Y2 = XL(2,I2)
           Y3 = XL(2,I3)
           Y4 = XL(2,I4)
           Z1 = XL(3,I1)
           Z2 = XL(3,I2)
           Z3 = XL(3,I3)
           Z4 = XL(3,I4)
           X13= X3-X1
           Y13= Y3-Y1
           Z13= Z3-Z1
           X24= X4-X2
           Y24= Y4-Y2
           Z24= Z4-Z2
           NRX=Y13*Z24-Z13*Y24
           NRY=Z13*X24-X13*Z24
           NRZ=X13*Y24-Y13*X24
           AREA2 = SQRT(NRX**2+NRY**2+NRZ**2)
           NORMAL(1,I) = NRX/AREA2
           NORMAL(2,I) = NRY/AREA2
           NORMAL(3,I) = NRZ/AREA2
           AREAF(I) = HALF*AREA2
           IF(IWAVE == 1)THEN
C Spherical wave
             X0=WI(1,I5)*X1+WI(2,I5)*X2+WI(3,I5)*X3+WI(4,I5)*X4
             Y0=WI(1,I5)*Y1+WI(2,I5)*Y2+WI(3,I5)*Y3+WI(4,I5)*Y4
             Z0=WI(1,I5)*Z1+WI(2,I5)*Z2+WI(3,I5)*Z3+WI(4,I5)*Z4
             DIRWIX=X0-RFLOW(9)
             DIRWIY=Y0-RFLOW(10)
             DIRWIZ=Z0-RFLOW(11)
             NORM=SQRT(DIRWIX**2+DIRWIY**2+DIRWIZ**2)
             DIRWIX=DIRWIX/NORM
             DIRWIY=DIRWIY/NORM
             DIRWIZ=DIRWIZ/NORM
             IF(FREESURF == 2) THEN
               DIRWRX=X0-RFLOW(13)
               DIRWRY=Y0-RFLOW(14)
               DIRWRZ=Z0-RFLOW(15)
               NORM=SQRT(DIRWRX**2+DIRWRY**2+DIRWRZ**2)
               DIRWRX=DIRWRX/NORM
               DIRWRY=DIRWRY/NORM
               DIRWRZ=DIRWRZ/NORM
                     COSG(I+NEL)=NORMAL(1,I)*DIRWRX+NORMAL(2,I)*DIRWRY+NORMAL(3,I)*DIRWRZ
             ENDIF
C hydrostatic pressure
             HH(I)=ZERO
             IF(GRAV_ID > 0) THEN
               HH(I)=MAX(ZERO,(XA-X0)*RFLOW(19)+(YA-Y0)*RFLOW(20)+(ZA-Z0)*RFLOW(21))
             ENDIF
           ELSEIF(IWAVE == 2) THEN
C Onde plane
             DIRWIX=RFLOW(9)
             DIRWIY=RFLOW(10)
             DIRWIZ=RFLOW(11)
C hydrostatic pressure
             HH(I)=ZERO
           ENDIF
           COSG(I)=NORMAL(1,I)*DIRWIX+NORMAL(2,I)*DIRWIY+NORMAL(3,I)*DIRWIZ
C vitesse N-1/2
           VX0 = WI(1,I5)*VL(1,I1)+WI(2,I5)*VL(1,I2)+WI(3,I5)*VL(1,I3)+WI(4,I5)*VL(1,I4)
           VY0 = WI(1,I5)*VL(2,I1)+WI(2,I5)*VL(2,I2)+WI(3,I5)*VL(2,I3)+WI(4,I5)*VL(2,I4)
           VZ0 = WI(1,I5)*VL(3,I1)+WI(2,I5)*VL(3,I2)+WI(3,I5)*VL(3,I3)+WI(4,I5)*VL(3,I4)
           VEL(I) = VX0*NORMAL(1,I)+VY0*NORMAL(2,I)+VZ0*NORMAL(3,I)
        ENDDO

      ENDIF
C-----------------------------------------------
C    1. Pression incidente
C-----------------------------------------------
      DO I=1,NEL
         OFF(I) = ZERO
         TTA = TT-TA(I)
         IF(TTA > ZERO) OFF(I) = ONE
      ENDDO
      IF(IPRES == 1) THEN
        IF(IWAVE == 1)THEN
          DO I=1,NEL
             TTA   = TT-TA(I)
             RATIO = DSC/DIST(I)
             PMAX  = RFLOW(6)*RATIO**APMAX
             THETA = RFLOW(7)*RATIO**ATHETA
             PP(I) = PMAX*EXP(-TTA/THETA)*OFF(I)
             DPIDT(I) = -PP(I)*COSG(I)/THETA
          ENDDO
        ELSEIF(IWAVE == 2) THEN
          PMAX  = RFLOW(6)
          THETA = RFLOW(7)
          DO I=1,NEL
             TTA = TT-TA(I)
             PP(I) = PMAX*EXP(-TTA/THETA)*OFF(I)
             DPIDT(I) = -PP(I)*COSG(I)/THETA
          ENDDO
        ENDIF
      ELSEIF(IPRES == 2) THEN
        IFPRES = IFLOW(7)
        SFPRES = RFLOW(3)
        IF(IWAVE == 1)THEN
          DO I=1,NEL
             TTA   = TT-TA(I)
             RATIO = DSC/DIST(I)
             IF(IFPRES > 0) THEN
                ISMOOTH = NPC(2*NFUNCT+IFPRES+1)
!!                PP(I) = SFPRES*FINTER(IFPRES,TTA,NPC,TF,DPPDT)*RATIO*OFF(I)
                IF (ISMOOTH == 0) THEN
                  PP(I) = SFPRES*FINTER(IFPRES,TTA,NPC,TF,DPPDT)*RATIO*OFF(I)
                ELSE IF(ISMOOTH > 0) THEN
                  PP(I) = SFPRES*FINTER_SMOOTH(IFPRES,TTA,NPC,TF,DPPDT)*RATIO*OFF(I)
                ELSE 
                  ISMOOTH = -ISMOOTH ! function id
                  CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,TTA,PP(I))
                  PP(I) = PP(I)*SFPRES*RATIO*OFF(I)
                  CALL PYTHON_DERIV_FUNCT1D(PYTHON, ISMOOTH,TTA,DPPDT)
                ENDIF ! IF (ISMOOTH == 0)
                DPIDT(I) = DPPDT*COSG(I)*RATIO*OFF(I)
             ELSE
                PP(I) = SFPRES*RATIO*OFF(I)
                DPIDT(I) = ZERO
             ENDIF
          ENDDO
        ELSEIF(IWAVE == 2) THEN
          DO I=1,NEL
             TTA = TT-TA(I)
             IF(IFPRES > 0) THEN
                ISMOOTH = NPC(2*NFUNCT+IFPRES+1)
!!                PP(I) = SFPRES*FINTER(IFPRES,TTA,NPC,TF,DPPDT)*OFF(I)
                IF (ISMOOTH == 0) THEN
                  PP(I) = SFPRES*FINTER(IFPRES,TTA,NPC,TF,DPPDT)*OFF(I)
                ELSE IF (ISMOOTH > 0) THEN
                  PP(I) = SFPRES*FINTER_SMOOTH(IFPRES,TTA,NPC,TF,DPPDT)*OFF(I)
                ELSE
                  ISMOOTH = -ISMOOTH ! function id
                  CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,TTA,PP(I))
                  PP(I) = PP(I)*SFPRES*OFF(I)
                  CALL PYTHON_DERIV_FUNCT1D(PYTHON, ISMOOTH,TTA,DPPDT)
                ENDIF ! IF (ISMOOTH == 0)
                DPIDT(I) = DPPDT*COSG(I)*OFF(I)
             ELSE
                PP(I) = SFPRES*OFF(I)
                DPIDT(I) = ZERO
             ENDIF
          ENDDO
        ENDIF
      ENDIF    
C-----------------------------------------------
C     Vitesse onde incidente
C-----------------------------------------------
      DO I=1,NEL
         VI(I)=PP(I)*COSG(I)/RHOC
      ENDDO
      IF(AFTERFLOW == 2) THEN
         IF(IWAVE == 1)THEN
C add afterflow velocity, no afterflow velocity for plane wave
           IF(IPRES == 1) THEN
              DO I=1,NEL
                 RATIO = DSC/DIST(I)
                 PMAX  = RFLOW(6)*RATIO**APMAX
                 THETA = RFLOW(7)*RATIO**ATHETA
                 VI(I) = VI(I) + (PMAX-PP(I))*COSG(I)*THETA/(RHO*DIST(I))
              ENDDO
           ELSEIF(IPRES == 2) THEN
              DO I=1,NEL
                 PTI(I) = PTI(I) + PP(I)*DT1
                   VI(I) = VI(I) + PTI(I)*COSG(I)/(RHO*DIST(I))
              ENDDO
           ENDIF
         ENDIF
      ENDIF
C-----------------------------------------------
C   2. Pression Onde Reflechie
C-----------------------------------------------
      IF(FREESURF == 2) THEN
        DO I=1,NEL
           OFF(I) = ZERO
           TTA = TT-TA(I+NEL)
           IF(TTA > ZERO) OFF(I) = ONE
        ENDDO
        IF(IPRES == 1) THEN
          DO I=1,NEL
             J = I+NEL
             TTA = TT-TA(J)
             RATIO = DSC/DIST(J)
             PMAX  = RFLOW(6)*RATIO**APMAX
             THETA = RFLOW(7)*RATIO**ATHETA
             PR(I) = -PMAX*EXP(-TTA/THETA)*OFF(I)
             DPRDT(I) = -PR(I)*COSG(J)/THETA
          ENDDO
        ELSEIF(IPRES == 2) THEN
          DO I=1,NEL
             J = I+NEL
             TTA = TT-TA(J)
             RATIO = DSC/DIST(J)
             IF(IFPRES > 0) THEN
                ISMOOTH = NPC(2*NFUNCT+IFPRES+1)
!!                PR(I) = -SFPRES*FINTER(IFPRES,TTA,NPC,TF,DPPDT)*RATIO*OFF(I)
                IF (ISMOOTH == 0) THEN
                  PR(I) = -SFPRES*FINTER(IFPRES,TTA,NPC,TF,DPPDT)*RATIO*OFF(I)
                ELSE IF (ISMOOTH > 0) THEN
                  PR(I) = -SFPRES*FINTER_SMOOTH(IFPRES,TTA,NPC,TF,DPPDT)*RATIO*OFF(I)
                ELSE
                  ISMOOTH = -ISMOOTH ! function id
                  CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,TTA,PR(I))
                  PR(I) = -PR(I)*SFPRES*RATIO*OFF(I)
                  CALL PYTHON_DERIV_FUNCT1D(PYTHON, ISMOOTH,TTA,DPPDT)
                ENDIF ! IF (ISMOOTH == 0)
                DPRDT(I) = DPPDT*COSG(J)*OFF(I)
             ELSE
                PR(I) = -SFPRES*RATIO*OFF(I)
                DPRDT(I) = ZERO
             ENDIF
          ENDDO
        ENDIF    
C-----------------------------------------------
C    Vitesse onde reflechie
C-----------------------------------------------
        DO I=1,NEL
           VR(I)=PR(I)*COSG(I+NEL)/RHOC
        ENDDO
      ELSE  ! pas de surface libre
        DO I=1,NEL
          PR(I) = ZERO
          VR(I) = ZERO
          DPRDT(I) = ZERO
        ENDDO
      ENDIF
C-----------------------------------------------
C    3. Scattered pressure Schema Euler ordre 1
C-----------------------------------------------
      IF(KFORM == 1) THEN
C DAA-1
        IF(NEL < NELMAX) THEN
          DO I=1,NEL
             DPMDT(I) = RHOC*ACCF(I)
             VECT(I)  = RHOC*AREAF(I)*(PM(I)-RHOC*(VI(I)+VR(I)))
          ENDDO
          VECT(1:NEL) = MATMUL(MFLE(1:NEL, 1:NEL), VECT(1:NEL))
          DO I=1,NEL
             DPMDT(I) = DPMDT(I) - VECT(I)
          ENDDO
          IF(INTEGR == 1) THEN
            DO I=1,NEL
               PM(I) = PM(I) + DPMDT(I)*DT1
               PS(I) = PM(I) - RHOC*(VI(I)+VR(I))
            ENDDO

          ELSEIF(INTEGR == 2) THEN
C Prediction
            DO I=1,NEL
               PS(I) = PM(I) + DPMDT(I)*DT1*HALF - RHOC*(VI(I)+VR(I))
            ENDDO
C Correction
            DO I=1,NEL
               DPMDT(I) = RHOC*ACCF(I)
               VECT(I)  = RHOC*AREAF(I)*PS(I)
            ENDDO
            VECT(1:NEL) = MATMUL(MFLE(1:NEL, 1:NEL), VECT(1:NEL))
            DO I=1,NEL
               DPMDT(I) = DPMDT(I) - VECT(I)
            ENDDO
            DO I=1,NEL
               PM(I) = PM(I) + DPMDT(I)*DT1
               PS(I) = PM(I) - RHOC*(VI(I)+VR(I)) 
            ENDDO
          ENDIF
C
        ELSE  ! resolution systeme  MFLE*X=Y
          IF(DT1 == ZERO) THEN
C         L U decomposition
             CALL DGETRF(NEL, NEL, MFLE, NEL, IPIV, INFO)
          ENDIF
          DO I=1,NEL
           VECT(I) = TWO*SSP*(RHOC*(VI(I)+VR(I))-PM(I))
          ENDDO
          CALL DGEMV('T',NEL, NEL, ONE, CBEM, NEL, VECT, 1, ZERO, VECT1, 1)
C         Resolution
          CALL DGETRS('N', NEL, 1, MFLE, NEL, IPIV, VECT1, NEL, INFO)
          VECT(1:NEL) = MATMUL(CBEM(1:NEL,1:NEL), VECT1(1:NEL))
          DO I=1,NEL
             IF(PP(I) /= ZERO) THEN
               DPMDT(I) = RHOC*ACCF(I) + VECT(I)
             ELSE
               DPMDT(I)=ZERO
             ENDIF
          ENDDO
          IF(INTEGR == 1) THEN
             DO I=1,NEL
                PM(I) = PM(I) + DPMDT(I)*DT1
                PS(I) = PM(I) - RHOC*(VI(I)+VR(I))
             ENDDO

          ELSEIF(INTEGR == 2) THEN
C Prediction
             DO I=1,NEL
                PS(I) = PM(I) + DPMDT(I)*DT1*HALF - RHOC*(VI(I)+VR(I))
             ENDDO
C Correction
             DO I=1,NEL
              VECT(I) = -TWO*SSP*PS(I)
             ENDDO
             CALL DGEMV('T',NEL, NEL, ONE, CBEM, NEL, VECT, 1, ZERO, VECT1, 1)
C         Resolution
             CALL DGETRS('N', NEL, 1, MFLE, NEL, IPIV, VECT1, NEL, INFO)
             VECT(1:NEL) = MATMUL(CBEM(1:NEL,1:NEL), VECT1(1:NEL))
             DO I=1,NEL
                DPMDT(I) = RHOC*ACCF(I) + VECT(I)
             ENDDO
             DO I=1,NEL
                PM(I) = PM(I) + DPMDT(I)*DT1
                PS(I) = PM(I) - RHOC*(VI(I)+VR(I))
             ENDDO
          ENDIF
        ENDIF
C
      ELSEIF(KFORM == 2) THEN
C High Frequency Approximation
        DO I=1,NEL
           DPMDT(I) = RHOC*ACCF(I)
        ENDDO
        DO I=1,NEL
           PM(I) = PM(I) + DPMDT(I)*DT1
           PS(I) = PM(I) - RHOC*VI(I) 
        ENDDO
C
      ELSEIF(KFORM == 3) THEN
C Virtual Mass Approximation
        DO I=1,NEL
           PS(I) = ZERO
           DO J=1,NEL
              PS(I) = PS(I) + MFLE(I,J)*(ACCF(J)-(DPIDT(J)+DPRDT(J))/RHOC)
           ENDDO
           PS(I) = PS(I)/AREAF(I)
        ENDDO
      ENDIF
C-----------------------------------------------
C    4. gravity
C-----------------------------------------------
      GRAVITY = ZERO
      IF(GRAV_ID > 0) THEN
        FCY = AGRV(1,GRAV_ID)
        FCX = AGRV(2,GRAV_ID)
        IFUNC = IGRV(3,GRAV_ID)
        ISENS = 0
        DO K=1,NSENSOR
          IF(IGRV(6,GRAV_ID) == SENSOR_TAB(K)%SENS_ID) ISENS=K  ! do it in starter !!!
        ENDDO
        IF(ISENS==0)THEN
          TS = TT
        ELSE
          TS = TT-SENSOR_TAB(ISENS)%TSTART
        ENDIF
        ISMOOTH = 0
        IF (IFUNC > 0) THEN
          ISMOOTH = NPC(2*NFUNCT+IFUNC+1)
!!        GRAVITY = FCY*FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
          IF (ISMOOTH == 0) THEN
            GRAVITY = FCY*FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
          ELSE IF(ISMOOTH > 0) THEN
            GRAVITY = FCY*FINTER_SMOOTH(IFUNC,TS*FCX,NPC,TF,DYDX)
          ELSE
            ISMOOTH = -ISMOOTH ! function id
            CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,TS*FCX,GRAVITY)
            GRAVITY = GRAVITY*FCY
          ENDIF ! IF (ISMOOTH = 0)
        ELSE
          GRAVITY = FCY
        ENDIF
      ENDIF
C-----------------------------------------------
C    5. compute total pressure forces
C-----------------------------------------------
      DO I=1,NEL
         PTOT = PP(I)+PS(I)+PR(I)+RHO*HH(I)*ABS(GRAVITY)
         FEL(I) = AREAF(I)*MAX(PTOT,PMIN)
      ENDDO
C    
      IF(JFORM == 1) THEN
         DO I=1,NEL
             I1 = ELEM(1,I)
             I2 = ELEM(2,I)
             I3 = ELEM(3,I)
             N1 = IBUF(I1)
             N2 = IBUF(I2)
             N3 = IBUF(I3)
             NRX= NORMAL(1,I)
             NRY= NORMAL(2,I)
             NRZ= NORMAL(3,I)
             FF = FAC1*THIRD*FEL(I)
             WFEXT   = WFEXT-FEL(I)*VEL(I)*DT1
             A(1,N1) = A(1,N1) + FF*NRX
             A(2,N1) = A(2,N1) + FF*NRY
             A(3,N1) = A(3,N1) + FF*NRZ
             A(1,N2) = A(1,N2) + FF*NRX
             A(2,N2) = A(2,N2) + FF*NRY
             A(3,N2) = A(3,N2) + FF*NRZ
             A(1,N3) = A(1,N3) + FF*NRX
             A(2,N3) = A(2,N3) + FF*NRY
             A(3,N3) = A(3,N3) + FF*NRZ
         ENDDO
      ELSEIF(JFORM == 2) THEN
         WF(1)=FAC1*FOURTH
         WF(2)=FAC1*THIRD
         DO I=1,NEL
             I1 = ELEM(1,I)
             I2 = ELEM(2,I)
             I3 = ELEM(3,I)
             I4 = ELEM(4,I)
             I5 = ELEM(5,I)
             N1 = IBUF(I1)
             N2 = IBUF(I2)
             N3 = IBUF(I3)
             N4 = IBUF(I4)
             NRX= NORMAL(1,I)
             NRY= NORMAL(2,I)
             NRZ= NORMAL(3,I)
             FF = WF(I5)*FEL(I)
             WFEXT   = WFEXT-FEL(I)*VEL(I)*DT1
             A(1,N1) = A(1,N1) + FF*NRX
             A(2,N1) = A(2,N1) + FF*NRY
             A(3,N1) = A(3,N1) + FF*NRZ
             A(1,N2) = A(1,N2) + FF*NRX
             A(2,N2) = A(2,N2) + FF*NRY
             A(3,N2) = A(3,N2) + FF*NRZ
             A(1,N3) = A(1,N3) + FF*NRX
             A(2,N3) = A(2,N3) + FF*NRY
             A(3,N3) = A(3,N3) + FF*NRZ
             IF(I5 == 2) CYCLE
             A(1,N4) = A(1,N4) + FF*NRX
             A(2,N4) = A(2,N4) + FF*NRY
             A(3,N4) = A(3,N4) + FF*NRZ
         ENDDO
      ENDIF
C-----------------------------------------------------------
C     5. Gauge
C-----------------------------------------------------------
      IF(JFORM == 2) THEN
        DO I=1,NBGAUGE
           GAUGE(30,I)=ZERO
           IF(LGAUGE(1,I) == 0 .AND. LGAUGE(3,I) < 0 ) THEN
              I1=SHELL_GA(I)
              IF(I1 > 0) THEN
                 PTOT=PP(I1)+PS(I1)+PR(I1)+RHO*HH(I1)*ABS(GRAVITY)
                 GAUGE(30,I)=MAX(PTOT,PMIN)
              ENDIF
           ENDIF
        ENDDO
      ENDIF
#else
       CALL ARRET(5)
#endif

      RETURN
      END
      
