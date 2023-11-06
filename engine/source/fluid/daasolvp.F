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
Chd|  DAASOLVP                      source/fluid/daasolvp.F
Chd|-- called by -----------
Chd|        FLOW0                         source/fluid/flow0.F
Chd|-- calls ---------------
Chd|        SPMD_FL_SUM                   source/mpi/generic/spmd_fl_sum.F
Chd|        FINTER                        source/tools/curve/finter.F
Chd|        FINTER_SMOOTH                 source/tools/curve/finter_smooth.F
Chd|        PYTHON_FUNCT_MOD              ../common_source/modules/python_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F
Chd|====================================================================
      SUBROUTINE DAASOLVP(NDIM,   NNO,    NEL,   NEL_LOC,
     .                    IFLOW,  IBUF,   ELEM,  IBUFL,   SHELL_GA,  CNP,
     .                    RFLOW,  NORMAL, TA,    AREAF,   COSG,      DIST,
     .                    MFLE,   ACCF,   PM,    PTI, X,  V, A, NPC, TF,
     .                    NBGAUGE,LGAUGE, GAUGE, NSENSOR, SENSOR_TAB, IGRV, AGRV,
     .                    CBEM   ,IPIV_L, MFLE_L,IBUFELR, IBUFELC,NFUNCT,
     .                    PYTHON)
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
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDIM, NNO, NEL, NBGAUGE, NSENSOR, NEL_LOC,NFUNCT
        INTEGER IFLOW(*), IBUF(*), ELEM(NDIM,*), SHELL_GA(*), NPC(*), LGAUGE(3,*)
        INTEGER IBUFL(*), CNP(*), IGRV(NIGRV,*)
        INTEGER IPIV_L(*), IBUFELR(NEL), IBUFELC(NEL)
        my_real X(3,*), V(3,*), A(3,*), TF(*), RFLOW(*)
        my_real NORMAL(3,*), TA(*), AREAF(*), COSG(*), DIST(*), PM(*), ACCF(*), PTI(*)
        my_real MFLE(NEL,*), GAUGE(LLGAUGE,*), AGRV(LFACGRV,*)
        my_real CBEM(NEL,*), MFLE_L(NEL_LOC,*)
        TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
        TYPE (PYTHON_), INTENT(INOUT) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  I, J, K, L, I1, I2, I3, I4, I5, N1, N2, N3, N4, II, JJ, KK
        INTEGER  IFPRES, JFORM, KFORM, IPRES, IWAVE, INTEGR, FREESURF, AFTERFLOW
        INTEGER  GRAV_ID, IFUNC, ISENS, LENBUF, NNO_L, NEL_L, NELMAX, INFO,ISMOOTH
        INTEGER  NEBUF(NEL)
        INTEGER  NBLOC, NPROW, NPCOL, ICTXT, DESCA(9), DESCB(9), MYROW, MYCOL, MXLLDM, NUMROC
        EXTERNAL NUMROC
        my_real  X1,  Y1,  Z1,  X2,  Y2,  Z2,  X3,  Y3,  Z3,  X4,  Y4,  Z4,
     .           X0,  Y0,  Z0,  X12, Y12, Z12, X13, Y13, Z13, X24, Y24, Z24,
     .           NRX, NRY, NRZ, AREA2, WF(2), WI(4,2),
     .           PM1, DSC, VX0, VY0, VZ0
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
        my_real, ALLOCATABLE :: SBUF(:), RBUF(:)

#if defined(MPI) && defined(MYREAL8) && !defined(WITHOUT_LINALG)
C Coordonnees et vitesses locales SPMD
        NNO_L=IFLOW(16)
        LENBUF=6*NNO
        ALLOCATE(SBUF(LENBUF), RBUF(LENBUF))
        SBUF(1:LENBUF)=ZERO
        RBUF(1:LENBUF)=ZERO
        DO I=1,NNO_L
          II=IBUFL(I)
          JJ=IBUF(II)
          KK=6*(II-1)
          SBUF(KK+1)=X(1,JJ)/CNP(II)
          SBUF(KK+2)=X(2,JJ)/CNP(II)
          SBUF(KK+3)=X(3,JJ)/CNP(II)
          SBUF(KK+4)=V(1,JJ)/CNP(II)
          SBUF(KK+5)=V(2,JJ)/CNP(II)
          SBUF(KK+6)=V(3,JJ)/CNP(II)
        ENDDO

        CALL SPMD_FL_SUM(SBUF, LENBUF, RBUF)

        DO I=1,NNO
          K=6*(I-1)
          XL(1,I)=RBUF(K+1)
          XL(2,I)=RBUF(K+2)
          XL(3,I)=RBUF(K+3)
          VL(1,I)=RBUF(K+4)
          VL(2,I)=RBUF(K+5)
          VL(3,I)=RBUF(K+6)
        ENDDO
        DEALLOCATE(SBUF, RBUF)

        JFORM  = IFLOW(4)
        NBLOC  = IFLOW(12)
        NPROW  = IFLOW(18)
        NPCOL  = IFLOW(19)
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
C  Initialisation de la process grid
            CALL SL_INIT(ICTXT, NPROW, NPCOL)
            CALL BLACS_GRIDINFO(ICTXT, NPROW, NPCOL, MYROW, MYCOL)
C          MXLLDM = NUMROC(NEL, NBLOC, MYROW, 1, NPROW)
            MXLLDM = NEL_LOC
            CALL DESCINIT(DESCA, NEL, NEL, NBLOC, NBLOC, 0, 0, ICTXT, MXLLDM, INFO)
            CALL DESCINIT(DESCB, NEL,   1, NBLOC, NBLOC, 0, 0, ICTXT, MXLLDM, INFO)

            IF(DT1 == ZERO) THEN
C  Compute local sub-matrix
              K=0
              DO I=1,NEL
                IF(IBUFELR(I) == 0) CYCLE
                K=K+1
                L=0
                DO J=1,NEL
                  IF(IBUFELC(J) == 0) CYCLE
                  L=L+1
                  MFLE_L(K,L) = MFLE(I,J)
                ENDDO
              ENDDO
C  L U decomposition
              CALL PDGETRF(NEL, NEL, MFLE_L, 1, 1, DESCA, IPIV_L, INFO)
            ENDIF

            DO I=1,NEL
              VECT(I) = TWO*SSP*(RHOC*(VI(I)+VR(I))-PM(I))
            ENDDO
            CALL DGEMV('T',NEL, NEL, ONE, CBEM, NEL, VECT, 1, ZERO, VECT1, 1)

            VECT(1:NEL)=ZERO
            IF(IBUFELC(1) > 0) THEN
              K=0
              DO I=1,NEL
                IF(IBUFELR(I) == 0) CYCLE
                K=K+1
                VECT(K) = VECT1(I)
              ENDDO
            ENDIF

C         Resolution
            CALL PDGETRS('N', NEL, 1, MFLE_L, 1, 1, DESCA, IPIV_L, VECT, 1, 1, DESCB, INFO)

C Echange
            ALLOCATE(SBUF(NEL), RBUF(NEL))
            SBUF(1:NEL)=ZERO
            RBUF(1:NEL)=ZERO
            IF(IBUFELC(1) > 0) THEN
              K=0
              DO I=1,NEL
                IF(IBUFELR(I) == 0) CYCLE
                K=K+1
                SBUF(I) = VECT(K)
              ENDDO
            ENDIF
            CALL SPMD_FL_SUM(SBUF, NEL, RBUF)
            DO I=1,NEL
              VECT(I)=RBUF(I)
            ENDDO

            VECT1(1:NEL) = MATMUL(CBEM(1:NEL,1:NEL), VECT(1:NEL))
            DO I=1,NEL
              IF(PP(I) /= ZERO) THEN
                DPMDT(I) = RHOC*ACCF(I) + VECT(I)
              ELSE
                DPMDT(I)=ZERO
              ENDIF
            ENDDO

            IF(INTEGR == 1) THEN
              DEALLOCATE(SBUF, RBUF)
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
              VECT(1:NEL)=ZERO
              IF(IBUFELC(1) > 0) THEN
                K=0
                DO I=1,NEL
                  IF(IBUFELR(I) == 0) CYCLE
                  K=K+1
                  VECT(K) = VECT1(I)
                ENDDO
              ENDIF

C Resolution
              CALL PDGETRS('N', NEL, 1, MFLE_L, 1, 1, DESCA, IPIV_L, VECT, 1, 1, DESCB, INFO)
C Echange
              SBUF(1:NEL)=ZERO
              RBUF(1:NEL)=ZERO
              IF(IBUFELC(1) > 0) THEN
                K=0
                DO I=1,NEL
                  IF(IBUFELR(I) == 0) CYCLE
                  K=K+1
                  SBUF(I) = VECT(K)
                ENDDO
              ENDIF
              CALL SPMD_FL_SUM(SBUF, NEL, RBUF)
              DO I=1,NEL
                VECT(I)=RBUF(I)
              ENDDO
              DEALLOCATE(SBUF, RBUF)

              VECT1(1:NEL) = MATMUL(CBEM(1:NEL,1:NEL), VECT(1:NEL))
              DO I=1,NEL
                DPMDT(I) = RHOC*ACCF(I) + VECT1(I)
              ENDDO
              DO I=1,NEL
                PM(I) = PM(I) + DPMDT(I)*DT1
                PS(I) = PM(I) - RHOC*(VI(I)+VR(I))
              ENDDO
            ENDIF

            CALL BLACS_GRIDEXIT(ICTXT)
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
            IF(IGRV(6,GRAV_ID) == SENSOR_TAB(K)%SENS_ID) ISENS=K ! do it in starter !!!
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
              CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,TTA,GRAVITY)
              GRAVITY = GRAVITY*FCY
            ENDIF ! IF (ISMOOTH == 0)
          ELSE
            GRAVITY   = FCY
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
          NEL_L=0
          DO I=1,NEL
            I1 = ELEM(1,I)
            I2 = ELEM(2,I)
            I3 = ELEM(3,I)
            N1 = IBUF(I1)
            N2 = IBUF(I2)
            N3 = IBUF(I3)
            IF(N1/=0.OR.N2/=0.OR.N3/=0) THEN
              NEL_L = NEL_L+1
              NEBUF(NEL_L) = I
            ENDIF
          ENDDO
          DO K=1,NEL_L
            I  = NEBUF(K)
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
            IF(N1/=0) THEN
              A(1,N1) = A(1,N1) + FF*NRX
              A(2,N1) = A(2,N1) + FF*NRY
              A(3,N1) = A(3,N1) + FF*NRZ
              TFEXT   = TFEXT+FF*DT1*(NRX*V(1,N1)+NRY*V(2,N1)+NRZ*V(3,N1))/CNP(I1)
            ENDIF
            IF(N2/=0) THEN
              A(1,N2) = A(1,N2) + FF*NRX
              A(2,N2) = A(2,N2) + FF*NRY
              A(3,N2) = A(3,N2) + FF*NRZ
              TFEXT   = TFEXT+FF*DT1*(NRX*V(1,N2)+NRY*V(2,N2)+NRZ*V(3,N2))/CNP(I2)
            ENDIF
            IF(N3/=0) THEN
              A(1,N3) = A(1,N3) + FF*NRX
              A(2,N3) = A(2,N3) + FF*NRY
              A(3,N3) = A(3,N3) + FF*NRZ
              TFEXT   = TFEXT+FF*DT1*(NRX*V(1,N3)+NRY*V(2,N3)+NRZ*V(3,N3))/CNP(I3)
            ENDIF
          ENDDO
        ELSEIF(JFORM == 2) THEN
          NEL_L=0
          DO I=1,NEL
            I1 = ELEM(1,I)
            I2 = ELEM(2,I)
            I3 = ELEM(3,I)
            I4 = ELEM(4,I)
            N1 = IBUF(I1)
            N2 = IBUF(I2)
            N3 = IBUF(I3)
            N4 = IBUF(I4)
            IF(N1/=0.OR.N2/=0.OR.N3/=0.OR.N4/=0) THEN
              NEL_L = NEL_L+1
              NEBUF(NEL_L) = I
            ENDIF
          ENDDO
          WF(1)=FAC1*FOURTH
          WF(2)=FAC1*THIRD
          DO K=1,NEL_L
            I  = NEBUF(K)
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
            IF(N1/=0) THEN
              A(1,N1) = A(1,N1) + FF*NRX
              A(2,N1) = A(2,N1) + FF*NRY
              A(3,N1) = A(3,N1) + FF*NRZ
              TFEXT   = TFEXT+FF*DT1*(NRX*V(1,N1)+NRY*V(2,N1)+NRZ*V(3,N1))/CNP(I1)
            ENDIF
            IF(N2/=0) THEN
              A(1,N2) = A(1,N2) + FF*NRX
              A(2,N2) = A(2,N2) + FF*NRY
              A(3,N2) = A(3,N2) + FF*NRZ
              TFEXT   = TFEXT+FF*DT1*(NRX*V(1,N2)+NRY*V(2,N2)+NRZ*V(3,N2))/CNP(I2)
            ENDIF
            IF(N3/=0) THEN
              A(1,N3) = A(1,N3) + FF*NRX
              A(2,N3) = A(2,N3) + FF*NRY
              A(3,N3) = A(3,N3) + FF*NRZ
              TFEXT   = TFEXT+FF*DT1*(NRX*V(1,N3)+NRY*V(2,N3)+NRZ*V(3,N3))/CNP(I3)
            ENDIF
            IF(N4/=0.AND.I5==1) THEN
              A(1,N4) = A(1,N4) + FF*NRX
              A(2,N4) = A(2,N4) + FF*NRY
              A(3,N4) = A(3,N4) + FF*NRZ
              TFEXT   = TFEXT+FF*DT1*(NRX*V(1,N4)+NRY*V(2,N4)+NRZ*V(3,N4))/CNP(I4)
            ENDIF
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

#endif
        RETURN
      END

