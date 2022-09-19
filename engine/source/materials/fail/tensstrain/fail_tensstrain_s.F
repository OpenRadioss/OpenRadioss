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
Chd|  FAIL_TENSSTRAIN_S             source/materials/fail/tensstrain/fail_tensstrain_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_TENSSTRAIN_S(
     1           NEL    ,NUPARAM,NUVAR  ,NFUNC   ,IFUNC   ,
     2           NPF    ,TF     ,TIME   ,TIMESTEP,UPARAM  ,
     3           NGL    ,IPM    ,MAT    ,ALDT    ,TSTAR  ,ISMSTR ,
     4           EPSXX  ,EPSYY  ,EPSZZ  ,EPSXY   ,EPSYZ  ,EPSZX  ,
     5           SIGNXX ,SIGNYY ,SIGNZZ ,SIGNXY  ,SIGNYZ ,SIGNZX ,
     6           EPSP   ,UVAR   ,OFF    ,IP      ,DFMAX  ,TDELE  ,
     7           MFXX   ,MFXY   ,MFXZ   ,MFYX    ,MFYY   ,MFYZ   ,
     8           MFZX   ,MFZY   ,MFZZ   )
C-----------------------------------------------
C   Tensile Strain failure criterion
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "param_c.inc"
#include "impl1_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL,NUPARAM,NUVAR,ISMSTR,IP
      INTEGER ,DIMENSION(NEL)       :: NGL,MAT
      INTEGER ,DIMENSION(NPROPMI,*) :: IPM
      my_real  :: TIME,TIMESTEP
      my_real ,DIMENSION(NUPARAM)  :: UPARAM
      my_real ,DIMENSION(NEL) :: SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX,
     .   EPSXX,EPSYY,EPSZZ,EPSXY,EPSYZ,EPSZX,EPSP,ALDT,TSTAR,
     .   MFXX,MFXY,MFXZ,MFYX,MFYY,MFYZ,MFZX,MFZY,MFZZ
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL), DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), IFUNC(NFUNC),NFUNC
      my_real FINTER ,TF(*),DF
      EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DF)
C        Y       : y = f(x)
C        X       : x
C        DF    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDXD,NINDXF,S_FLAG,STRDEF,STRFLAG
      INTEGER ,DIMENSION(NEL) :: IFLAG,INDXD,INDXF
      my_real :: E1,E2,E3,E4,E5,E6,RFAC,RFAC2,E42,E52,E62,C,D,EPST,EPST2,
     .       R1,R2,Y,YP,DAV,DYDX,IE_SP,P,SCALE,CC,Y1,I1,I2,I3,Q,R,PHI,
     .       R_INTER,EL_REF,SC_EL,EPSP1,EPSP2,FAC,SCALE_TEMP,
     .       E11,E22,E33,EPSF1,EPSF2,LAMBDA,UNIT_T,EPSP_UNIT
      my_real,DIMENSION(NEL) :: EPS11,EPS22,EPS33,EPS12,EPS23,EPS31,
     .   EPS_MAX,DAMAGE,RIEF1,RIEF2
C=======================================================================
      EPSF1   = UPARAM(1)
      EPSF2   = UPARAM(2)
      EPSP1   = UPARAM(3)
      EPSP2   = UPARAM(4)
      SC_EL      = UPARAM(5)
      EL_REF     = UPARAM(6)
      SCALE_TEMP = UPARAM(7)
      S_FLAG     = INT(UPARAM(8))
      UNIT_T     = UPARAM(9)
      STRDEF     = INT(UPARAM(10))
c
      DAMAGE(:NEL)  = ZERO
      EPS_MAX(:NEL) = ZERO
      NINDXD  = 0
      NINDXF  = 0
      STRFLAG = 0
C-----------------------------------------------
c     Initialization
C-----------------------------------------------
      IF (TIME == ZERO) THEN
        DO I=1,NEL

          SELECT CASE (S_FLAG)
            CASE (1)  ! default - old formulation
              UVAR(I,1) = EPSP1
            CASE (2)
              IF (IFUNC(2) > 0)THEN
                LAMBDA    = ALDT(I)  / EL_REF
                FAC       = SC_EL * FINTER(IFUNC(2),LAMBDA,NPF,TF,DF)
                UVAR(I,1) = FAC
              ELSE
                UVAR(I,1) = ONE
              ENDIF
            CASE (3)
              IF (IFUNC(2) > 0)THEN
                LAMBDA    = ALDT(I)  / EL_REF
                FAC       = SC_EL * FINTER(IFUNC(2),LAMBDA,NPF,TF,DF)
                UVAR(I,1) = FAC
              ELSE
                UVAR(I,1) = ONE
              ENDIF
          END SELECT
        ENDDO
      ENDIF
c-----------------------------------------------
      DO I=1,NEL
        IF (OFF(I) < EM01) OFF(I)=ZERO
        IF (OFF(I) < ONE)   OFF(I)=OFF(I)*FOUR_OVER_5
      END DO
c----------------------------------------------
c     Max strain transformation following input definition
c-------------------
      IF (STRDEF == 2) THEN        ! failure defined as engineering strain
        IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 1
        ELSE IF (ISMSTR == 0 .or. ISMSTR == 2 .or. ISMSTR == 4) THEN
c         transform true strain to engineering
          STRFLAG = 2
        END IF
      ELSE IF (STRDEF == 3) THEN   ! failure defined as true strain
        IF (ISMSTR == 1 .or. ISMSTR == 3 .or. ISMSTR == 11) THEN
c         transform engineering to true strain
          STRFLAG = 3
        ELSE IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 4
        END IF
      END IF
c
c-------------------------
      SELECT CASE (S_FLAG)
c-------------------------
        CASE (1)  ! Equivalent strain criterion (old). Backward compatibility only
c-------------------
          IF (STRFLAG == 1) THEN      ! Radioss strain is Cauchy-Green
c           transform total grad def strain to engineering
            DO I=1,NEL
              EPS11(I)  = MFXX(I)
              EPS22(I)  = MFYY(I)
              EPS33(I)  = MFZZ(I)
              EPS12(I)  = MFXY(I) + MFYX(I)
              EPS23(I)  = MFZY(I) + MFYZ(I)
              EPS31(I)  = MFXZ(I) + MFZX(I)
            END DO
          ELSE IF (STRFLAG == 2) THEN
c           transform engineering strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(EPSXX(I) + ONE)
              EPS22(I)  = LOG(EPSYY(I) + ONE)
              EPS33(I)  = LOG(EPSZZ(I) + ONE)
              EPS12(I)  = LOG(EPSXY(I) + ONE)
              EPS23(I)  = LOG(EPSYZ(I) + ONE)
              EPS31(I)  = LOG(EPSZX(I) + ONE)
            END DO
          ELSE IF (STRFLAG == 3) THEN
c           transform engineering strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(EPSXX(I) + ONE)
              EPS22(I)  = LOG(EPSYY(I) + ONE)
              EPS33(I)  = LOG(EPSZZ(I) + ONE)
              EPS12(I)  = LOG(EPSXY(I) + ONE)
              EPS23(I)  = LOG(EPSYZ(I) + ONE)
              EPS31(I)  = LOG(EPSZX(I) + ONE)
            END DO
          ELSE IF (STRFLAG == 4) THEN      ! Radioss strain is Cauchy-Green
c           transform total grad def strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(MFXX(I) + ONE)
              EPS22(I)  = LOG(MFYY(I) + ONE)
              EPS33(I)  = LOG(MFZZ(I) + ONE)
              EPS12(I)  = LOG(MFXY(I) + MFYX(I) + ONE)
              EPS23(I)  = LOG(MFXZ(I) + MFZX(I) + ONE)
              EPS31(I)  = LOG(MFZY(I) + MFYZ(I) + ONE)
            END DO
          ELSE
            EPS11(1:NEL)  = EPSXX(1:NEL)
            EPS22(1:NEL)  = EPSYY(1:NEL)
            EPS33(1:NEL)  = EPSZZ(1:NEL)
            EPS12(1:NEL)  = EPSXY(1:NEL)
            EPS23(1:NEL)  = EPSYZ(1:NEL)
            EPS31(1:NEL)  = EPSZX(1:NEL)
          END IF
c         calculate max strain
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              DAV = (EPS11(I)+EPS22(I)+EPS33(I))*THIRD
              E1 = EPS11(I) - DAV
              E2 = EPS22(I) - DAV
              E3 = EPS33(I) - DAV
              E4 = HALF*EPS12(I)
              E5 = HALF*EPS23(I)
              E6 = HALF*EPS31(I)
              E42 = E4*E4
              E52 = E5*E5
              E62 = E6*E6
              C =  (E1*E2 + E1*E3 + E2*E3) - E42 - E52 - E62
              D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42 - TWO*E4*E5*E6
              CC =  C*THIRD
              EPST = SQRT(-CC)
              EPST2 = EPST * EPST
              Y = (EPST2 + C)* EPST + D
              Y1 = -(EPST2 + C)* EPST + D
              IF(ABS(Y) > EM8 .OR.(ABS(Y) < EM8 .AND. ABS(Y1) < ABS(Y)))THEN
                EPST = 1.75 * EPST
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP

                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
C                          
              ENDIF
              EPST       = EPST + DAV
              EPS_MAX(I) = EPST
            END IF
          ENDDO
c
c         test failure crit
c
          DO I=1,NEL
            IF (OFF(I) == ONE)THEN
              RFAC = ONE
              IF (IFUNC(1) > 0)THEN
                EPSP_UNIT = EPSP(I) * UNIT_T
                RFAC = FINTER(IFUNC(1),EPSP_UNIT,NPF,TF,DYDX)
                RFAC = MAX(RFAC,EM20)
              ENDIF
              R1 = EPSF1*RFAC
              R2 = EPSF2*RFAC
c
              IF(EPS_MAX(I) > R1.AND.R1 < R2) THEN
                IF (DFMAX(I) == ZERO) THEN
                  NINDXD = NINDXD + 1
                  INDXD(NINDXD) = I
#include "lockon.inc"
                  WRITE(IOUT, 2002) NGL(I),EPS_MAX(I)
                  WRITE(ISTDO,2002) NGL(I),EPS_MAX(I)
#include "lockoff.inc"
                ENDIF
                DAMAGE(I)= (EPS_MAX(I)-R1)/(R2-R1)
                DAMAGE(I)= MIN(ONE,DAMAGE(I))
              ENDIF
              IF (EPS_MAX(I) >= R2) THEN
                DAMAGE(I)= ONE
                OFF(I)=FOUR_OVER_5
                NINDXF=NINDXF+1
                INDXF(NINDXF)=I
                TDELE(I) = TIME
#include "lockon.inc"
                WRITE(IOUT, 3000) NGL(I),EPS_MAX(I)
                WRITE(ISTDO,3100) NGL(I),EPS_MAX(I),TIME
#include "lockoff.inc"
              ENDIF

              IF (EPSP1 > ZERO .OR. EPSP2 > ZERO)THEN
c!              from: http://www.continuummechanics.org/principalstrain.html
                E1  = EPS11(I)
                E2  = EPS22(I)
                E3  = EPS33(I)
                E4  = HALF*EPS12(I)
                E5  = HALF*EPS23(I)
                E6  = HALF*EPS31(I)
                E42 = E4*E4
                E52 = E5*E5
                E62 = E6*E6

                I1 = E1 + E2 + E3
                I2 = E1*E2 + E2*E3 + E3*E1 - E4*E4 - E5*E5 - E6*E6
                I3 = E1*E2*E3 - E1*E52 - E2*E62 - E3*E42 + TWO*E4*E5*E6

                Q  = (THREE*I2 - I1*I1)/NINE
                R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4     ! (2*I3^3-9*I1*I2+27*I3)/54

                R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
                PHI = ACOS(MAX(R_INTER,-ONE))

                E11 = TWO*SQRT(-Q)*COS(PHI/THREE)+THIRD*I1
                E22 = TWO*SQRT(-Q)*COS((PHI+TWO*PI)/THREE)+THIRD*I1
                E33 = TWO*SQRT(-Q)*COS((PHI+FOUR*PI)/THREE)+THIRD*I1
c
                IF (E11 < E22) THEN
                   R_INTER = E11
                   E11     = E22
                   E22     = R_INTER
                ENDIF
                IF (E22 < E33)THEN
                   R_INTER = E22
                   E22     = E33
                   E33     = R_INTER
                ENDIF
                IF (E11 < E22)THEN
                   R_INTER = E11
                   E11     = E22
                   E22     = R_INTER
                ENDIF
                DFMAX(I) = MIN(ONE,(E11 / UVAR(I,1)))
c
                IF(E11 >= UVAR(I,1)) THEN
                  DAMAGE(I)= ONE
                  OFF(I)=FOUR_OVER_5
                  NINDXF=NINDXF+1
                  INDXF(NINDXF)=I
                  TDELE(I) = TIME
#include "lockon.inc"
                    WRITE(IOUT, 6000) NGL(I),E11
                    WRITE(ISTDO,6100) NGL(I),E11,TIME
#include "lockoff.inc"
                ENDIF
c
                IF (UVAR(I,1) < ZERO .AND. ABS(E11) >= ABS(UVAR(I,1))) THEN
                  DAMAGE(I)= ONE
                  OFF(I)=FOUR_OVER_5
                  NINDXF=NINDXF+1
                  INDXF(NINDXF)=I
                  TDELE(I) = TIME
#include "lockon.inc"
                    WRITE(IOUT, 6000) NGL(I),E11
                    WRITE(ISTDO,6100) NGL(I),E11,TIME
#include "lockoff.inc"
                ENDIF

                IF(EPSP2 > ZERO)THEN
                  IF(ABS(E22) >= EPSP2) THEN
                    DAMAGE(I)= ONE
                    OFF(I)=FOUR_OVER_5
                    NINDXF=NINDXF+1
                    INDXF(NINDXF)=I
                    TDELE(I) = TIME
#include "lockon.inc"
                      WRITE(IOUT, 7000) NGL(I),E22
                      WRITE(ISTDO,7100) NGL(I),E22,TIME
#include "lockoff.inc"
                  ENDIF
                ENDIF
              ENDIF     ! EPSP1 > ZERO .OR. EPSP2 > ZERO
c
            ENDIF       ! OFF(I) == ONE
          ENDDO
c
c----------------------------------------------------
        CASE (2)  ! Equivalent strain criterion (new)
c----------------------------------------------------
          IF (STRFLAG == 1) THEN      ! Radioss strain is Cauchy-Green
c           transform total grad def strain to engineering
            DO I=1,NEL
              EPS11(I)  = MFXX(I)
              EPS22(I)  = MFYY(I)
              EPS33(I)  = MFZZ(I)
              EPS12(I)  = MFXY(I) + MFYX(I)
              EPS23(I)  = MFZY(I) + MFYZ(I)
              EPS31(I)  = MFXZ(I) + MFZX(I)
            END DO
          ELSE IF (STRFLAG == 2) THEN
c           transform engineering strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(EPSXX(I) + ONE)
              EPS22(I)  = LOG(EPSYY(I) + ONE)
              EPS33(I)  = LOG(EPSZZ(I) + ONE)
              EPS12(I)  = LOG(EPSXY(I) + ONE)
              EPS23(I)  = LOG(EPSYZ(I) + ONE)
              EPS31(I)  = LOG(EPSZX(I) + ONE)
            END DO
          ELSE IF (STRFLAG == 3) THEN
c           transform engineering strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(EPSXX(I) + ONE)
              EPS22(I)  = LOG(EPSYY(I) + ONE)
              EPS33(I)  = LOG(EPSZZ(I) + ONE)
              EPS12(I)  = LOG(EPSXY(I) + ONE)
              EPS23(I)  = LOG(EPSYZ(I) + ONE)
              EPS31(I)  = LOG(EPSZX(I) + ONE)
            END DO
          ELSE IF (STRFLAG == 4) THEN      ! Radioss strain is Cauchy-Green
c           transform total grad def strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(MFXX(I) + ONE)
              EPS22(I)  = LOG(MFYY(I) + ONE)
              EPS33(I)  = LOG(MFZZ(I) + ONE)
              EPS12(I)  = LOG(MFXY(I) + MFYX(I) + ONE)
              EPS23(I)  = LOG(MFXZ(I) + MFZX(I) + ONE)
              EPS31(I)  = LOG(MFZY(I) + MFYZ(I) + ONE)
            END DO
          ELSE
            EPS11(1:NEL)  = EPSXX(1:NEL)
            EPS22(1:NEL)  = EPSYY(1:NEL)
            EPS33(1:NEL)  = EPSZZ(1:NEL)
            EPS12(1:NEL)  = EPSXY(1:NEL)
            EPS23(1:NEL)  = EPSYZ(1:NEL)
            EPS31(1:NEL)  = EPSZX(1:NEL)
          END IF
c         calculate eps max
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              DAV = (EPS11(I)+EPS22(I)+EPS33(I))*THIRD
              E1 = EPS11(I) - DAV
              E2 = EPS22(I) - DAV
              E3 = EPS33(I) - DAV
              E4 = HALF*EPS12(I)
              E5 = HALF*EPS23(I)
              E6 = HALF*EPS31(I)
              E42 = E4*E4
              E52 = E5*E5
              E62 = E6*E6
              C =  (E1*E2 + E1*E3 + E2*E3) - E42 - E52 - E62
              D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42 - TWO*E4*E5*E6
              CC =  C*THIRD
              EPST = SQRT(-CC)
              EPST2 = EPST * EPST
              Y = (EPST2 + C)* EPST + D
              Y1 = -(EPST2 + C)* EPST + D
              IF(ABS(Y) > EM8 .OR.(ABS(Y) < EM8 .AND. ABS(Y1) < ABS(Y)))THEN
                EPST = 1.75 * EPST
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP

                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
              ENDIF
              EPST       = EPST + DAV
              EPS_MAX(I) = EPST
            END IF
          ENDDO
c
c         test failure crit
c
          DO I=1,NEL
            IF (OFF(I) == ONE)THEN
              RFAC = ONE
              IF(IFUNC(1) > 0)THEN
                EPSP_UNIT = EPSP(I) * UNIT_T
                RFAC = FINTER(IFUNC(1),EPSP_UNIT,NPF,TF,DYDX)
                RFAC = MAX(RFAC,EM20)
              ENDIF
              IF (IFUNC(3) > 0) THEN   ! temperature
                RFAC2 = FINTER(IFUNC(3),TSTAR(I),NPF,TF,DYDX)
                RFAC2 = MAX(RFAC2,EM20)
              ELSE
                RFAC2 = ONE
              ENDIF
              R1 = EPSF1*RFAC*RFAC2*UVAR(I,1)
              R2 = EPSF2*RFAC*RFAC2*UVAR(I,1)
c
              IF (EPS_MAX(I) > R1 .AND. R1 < R2) THEN
                DAMAGE(I)= (EPS_MAX(I)-R1)/(R2-R1)
                DAMAGE(I)= MIN(ONE,DAMAGE(I))
              ENDIF

              IF (EPS_MAX(I) >= R2) THEN
                DAMAGE(I)= ONE
                OFF(I)=FOUR_OVER_5
                NINDXF=NINDXF+1
                INDXF(NINDXF)=I
                TDELE(I) = TIME
#include "lockon.inc"
                WRITE(IOUT, 3000) NGL(I),EPS_MAX(I)
                WRITE(ISTDO,3100) NGL(I),EPS_MAX(I),TIME
#include "lockoff.inc"
              ENDIF
            ENDIF
          ENDDO
c-------------------
        CASE (3)    !  Max principal tensile strain. No failure in compression
c-------------------
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN

              E1  = EPSXX(I)
              E2  = EPSYY(I)
              E3  = EPSZZ(I)
              E4  = HALF*EPSXY(I)
              E5  = HALF*EPSYZ(I)
              E6  = HALF*EPSZX(I)
              E42 = E4*E4
              E52 = E5*E5
              E62 = E6*E6

              I1 = E1 + E2 + E3
              I2 = E1*E2 + E2*E3 + E3*E1 - E4*E4 - E5*E5 - E6*E6
              I3 = E1*E2*E3 - E1*E52 - E2*E62 - E3*E42 + TWO*E4*E5*E6
              Q  = (THREE*I2 - I1*I1)/NINE
              R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4     ! (2*I3^3-9*I1*I2+27*I3)/54

              R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
              PHI = ACOS(MAX(R_INTER,-ONE))

              E11 = TWO*SQRT(-Q)*COS(PHI/THREE)+THIRD*I1
              E22 = TWO*SQRT(-Q)*COS((PHI+TWO*PI)/THREE)+THIRD*I1
              E33 = TWO*SQRT(-Q)*COS((PHI+FOUR*PI)/THREE)+THIRD*I1
c
              IF (STRFLAG == 1) THEN
                E11 = SQRT(E11 + ONE) - ONE
                E22 = SQRT(E22 + ONE) - ONE
                E33 = SQRT(E33 + ONE) - ONE
              ELSE IF (STRFLAG == 2) THEN
                E11 = EXP(E11) - ONE
                E22 = EXP(E22) - ONE
                E33 = EXP(E33) - ONE
              ELSE IF (STRFLAG == 3) THEN
                E11 = LOG(E11 + ONE)
                E22 = LOG(E22 + ONE)
                E33 = LOG(E33 + ONE)
              ELSE IF (STRFLAG == 4) THEN
                E11 = LOG(SQRT(E11+ONE))
                E22 = LOG(SQRT(E22+ONE))
                E33 = LOG(SQRT(E33+ONE))
              END IF
c
              IF (E11 < E22) THEN
                 R_INTER = E11
                 E11     = E22
                 E22     = R_INTER
              ENDIF
              IF (E22 < E33)THEN
                 R_INTER = E22
                 E22     = E33
                 E33     = R_INTER
              ENDIF
              IF (E11 < E22)THEN
                 R_INTER = E11
                 E11     = E22
                 E22     = R_INTER
              ENDIF
              EPS_MAX(I) = E11
            END IF
          ENDDO
c         test failure crit
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              R1 = EPSF1*UVAR(I,1)
              R2 = EPSF2*UVAR(I,1)
              IF (IFUNC(1) > 0) THEN    ! strain rate factor
                EPSP_UNIT = EPSP(I) * UNIT_T
                RFAC = FINTER(IFUNC(1),EPSP_UNIT,NPF,TF,DYDX)
                RFAC = MAX(RFAC,EM20)
                R1 = R1*RFAC
                R2 = R2*RFAC
              ENDIF
              IF (IFUNC(3) > 0) THEN    ! temperature factor
                RFAC2 = FINTER(IFUNC(3),TSTAR(I),NPF,TF,DYDX)
                RFAC2 = MAX(RFAC2,EM20)
                R1 = R1*RFAC2
                R2 = R2*RFAC2
              ENDIF
c
              IF (EPS_MAX(I) > R1 .AND. R1 < R2 .AND. EPS_MAX(I) > ZERO) THEN
                IF (DFMAX(I) == ZERO) THEN
                  NINDXD = NINDXD + 1
                  INDXD(NINDXD) = I
#include "lockon.inc"
                 WRITE(IOUT, 2001) NGL(I),EPS_MAX(I)
                 WRITE(ISTDO,2001) NGL(I),EPS_MAX(I)
#include "lockoff.inc"
                ENDIF
                DAMAGE(I) = (EPS_MAX(I)-R1)/(R2-R1)
                DAMAGE(I) = MIN(ONE,DAMAGE(I))
              ENDIF
              IF (EPS_MAX(I) >= R2 .AND. EPS_MAX(I) > ZERO) THEN
               DAMAGE(I)= ONE
               OFF(I) = FOUR_OVER_5
               NINDXF=NINDXF+1
               INDXF(NINDXF)=I
               TDELE(I) = TIME
#include "lockon.inc"
               WRITE(IOUT, 6000) NGL(I),EPS_MAX(I)
               WRITE(ISTDO,6100) NGL(I),EPS_MAX(I),TIME
#include "lockoff.inc"
              ENDIF
          ENDIF
c
         ENDDO
c---------------
      END SELECT
c--------------------------
c
      IF (NINDXF > 0 .AND. IMCONV == 1) THEN
        IDEL7NOK = 1
        DO J=1,NINDXF
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDXF(J))
          WRITE(ISTDO,1100) NGL(INDXF(J)),TIME
#include "lockoff.inc"
        END DO
      END IF
C ---
      DO I=1,NEL
        R1 = EPSF1
        R2 = EPSF2
        IF (R1 < R2) THEN
          SIGNXX(I) = (ONE - DAMAGE(I))*SIGNXX(I)
          SIGNYY(I) = (ONE - DAMAGE(I))*SIGNYY(I)
          SIGNZZ(I) = (ONE - DAMAGE(I))*SIGNZZ(I)
          SIGNXY(I) = (ONE - DAMAGE(I))*SIGNXY(I)
          SIGNYZ(I) = (ONE - DAMAGE(I))*SIGNYZ(I)
          SIGNZX(I) = (ONE - DAMAGE(I))*SIGNZX(I)
        END IF
        DFMAX(I) = MAX(DFMAX(I),DAMAGE(I))  ! Maximum Damage storing for output : 0 < DFMAX < 1
       ENDDO
c-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,' AT TIME :',1PE20.13)
 2001 FORMAT(1X,'START DAMAGE (TENS) OF ELEMENT ',I10,', 1st PRINCIPAL STRAIN = ',G11.4)
 2002 FORMAT(1X,'START DAMAGE (TENS) OF ELEMENT ',I10,', EQUIVALENT STRAIN = ',G11.4)
 3000 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   MAX EQUIVALENT STRAIN =',G11.4)
 3100 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   MAX EQUIVALENT STRAIN =',G11.4,
     .          ' AT TIME :',1PE12.4)
 6000 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   1st PRINCIPAL STRAIN = ',G11.4)
 6100 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   1st PRINCIPAL STRAIN = ',G11.4,
     .          ' AT TIME :',1PE12.4)
 7000 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   2nd PRINCIPAL STRAIN = ',G11.4)
 7100 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   2nd PRINCIPAL STRAIN = ',G11.4,
     .          ' AT TIME :',1PE12.4)
c-----------------------------------------------
      RETURN
      END
