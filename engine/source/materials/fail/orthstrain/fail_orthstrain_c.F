Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  FAIL_ORTHSTRAIN_C             source/materials/fail/orthstrain/fail_orthstrain_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_ORTHSTRAIN_C(
     1     NEL      ,NUPARAM  ,NUVAR    ,UPARAM   ,UVAR     ,
     2     NFUNC    ,IFUNC    ,NPF      ,TF       ,NGL      ,
     3     TIME     ,TIMESTEP ,IPG      ,ILAY     ,IPT      ,   
     4     EPSXX    ,EPSYY    ,EPSXY    ,DMG_FLAG ,DMG_SCALE,
     5     EPSPXX   ,EPSPYY   ,EPSPXY   ,ALDT     ,ISMSTR   ,
     6     SIGNXX   ,SIGNYY   ,SIGNXY   ,
     7     OFF      ,OFFLY    ,FOFF     ,DFMAX    ,TDEL     )
C-----------------------------------------------
C    Orthotropic strain failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT,ISMSTR
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME,TIMESTEP
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: OFF,EPSXX,EPSYY,EPSXY,
     .   EPSPXX,EPSPYY,EPSPXY,ALDT
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
      TARGET :: UVAR
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) :: DMG_FLAG
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: OFFLY,FOFF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX,SIGNXX,SIGNYY,SIGNXY
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,FAILI,MODE,XX,XY,YY,IFUNC_SIZ,STRDEF,STRFLAG
      INTEGER ,DIMENSION(NEL)   :: INDX,IPOSP,IADP,ILENP
      INTEGER ,DIMENSION(NEL,6) :: FMODE
      my_real DAM(NEL,6),ODAM(NEL,6),EPSP(NEL,6),
     .  SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),ESCAL(NEL),DYDXE(NEL)
      my_real DYDX,FRATE,ET1,ET2,ET12,EC1,EC2,EC12,ET3,EC3,EC23,ET23,EC31,
     .  ET31,ET1M,ET2M,ET12M,EC1M,EC2M,EC12M,ET3M,EC3M,EC23M,ET23M,EC31M,
     .  ET31M,DI,ALPHA,DAMXX,DAMYY,DAMXY,ODAMXX,ODAMYY,ODAMXY,THETA,C,S,
     .  EPSPREF,EPDOT,DEPS,DEPSM,FACT,FSCALE_SIZ,REF_SIZ,EPSD1,EPSD2,
     .  EPST1,EPST2,EPST_A,EPST_B
      my_real ,DIMENSION(NEL) :: EPS11,EPS22,EPS12,EPSP11,EPSP22,EPSP12,
     .                           EPSM1,EPSM2,EPSM3,EPSM4,EPSM5,EPSM6
      my_real ,DIMENSION(:), POINTER :: FSIZE
C=======================================================================
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      DMG_FLAG   = INT(UPARAM(1))
      ALPHA      = MIN(TWO*PI*UPARAM(16)*TIMESTEP,ONE)
      EPSPREF    = UPARAM(17)
      FSCALE_SIZ = UPARAM(26)
      REF_SIZ    = UPARAM(27)
      STRDEF     = NINT(UPARAM(28))
      IFUNC_SIZ  = IFUNC(13)
c
c     et1  = mode1 , et2  = mode 2, ec1 = mode 3, ec2 = mode 4, 
c     et12 = mode 5, ec12 = mode 6
c     xx = dir 1, yy = dir 2, xy = dir 3
c     xx => et1,  ec1
c     yy => et2,  ec2
c     xy => et12, ec12
c
c----------------------------------------------
c     strain transformation following input definition
c-------------------
      STRFLAG = 0
      IF (STRDEF == 2) THEN        ! failure defined as engineering strain
        IF (ISMSTR /= 1 .AND. ISMSTR /= 3 .AND. ISMSTR /= 11) THEN
          STRFLAG = 2
        END IF
      ELSE IF (STRDEF == 3) THEN   ! failure defined as true strain
        IF (ISMSTR == 1 .OR. ISMSTR == 3 .OR. ISMSTR == 11) THEN
          STRFLAG = 3
        END IF
      END IF     
c--------------------------
      SELECT CASE (STRFLAG)
c
        CASE (2)  !  transform true strain to engineering
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              THETA  = HALF*ATAN(EPSXY(I) / MAX((EPSXX(I)-EPSYY(I)),EM20))
              C = COS(THETA)
              S = SIN(THETA)
c             strain principal + transformation              
              EPST_A = HALF*(EPSXX(I)+EPSYY(I))
              EPST_B = SQRT( (HALF*(EPSXX(I)-EPSYY(I)))**2 + (HALF*EPSXY(I))**2)
              EPST1  = EPST_A + EPST_B
              EPST2  = EPST_A - EPST_B
              EPST1  = EXP(EPST1) - ONE
              EPST2  = EXP(EPST2) - ONE
              EPS11(I) = C*C*EPST1 + S*S*EPST2
              EPS22(I) = S*S*EPST1 + C*C*EPST2
              EPS12(I) = S*C*(EPST2 - EPST1)
c             strain rate
              EPST_A = HALF*(EPSPXX(I)+EPSPYY(I))
              EPST_B = SQRT( (HALF*(EPSPXX(I)-EPSPYY(I)))**2 + (HALF*EPSPXY(I))**2)
              EPSD1  = EPST_A + EPST_B
              EPSD2  = EPST_A - EPST_B
              EPSD1  = (ONE+EPST1)*EPSD1
              EPSD2  = (ONE+EPST2)*EPSD2
              EPSP11(I) = C*C*EPSD1 + S*S*EPSD2
              EPSP22(I) = S*S*EPSD1 + C*C*EPSD2
              EPSP12(I) = S*C*(EPSD2 - EPSD1) 
            END IF
          ENDDO
c
        CASE (3)  !  transform engineering to true strain
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              THETA  = HALF*ATAN(EPSXY(I) / MAX((EPSXX(I)-EPSYY(I)),EM20))
              C = COS(THETA)
              S = SIN(THETA)
c             strain principal + transformation              
              EPST_A = HALF*(EPSXX(I)+EPSYY(I))
              EPST_B = SQRT( (HALF*(EPSXX(I)-EPSYY(I)))**2 + (HALF*EPSXY(I))**2)
              EPST1  = EPST_A + EPST_B
              EPST2  = EPST_A - EPST_B
              EPST1  = LOG(ONE + EPST1)
              EPST2  = LOG(ONE + EPST2)
              EPS11(I) = C*C*EPST1 + S*S*EPST2
              EPS22(I) = S*S*EPST1 + C*C*EPST2
              EPS12(I) = S*C*(EPST2 - EPST1)
c             strain rate
              EPST_A = HALF*(EPSPXX(I)+EPSPYY(I))
              EPST_B = SQRT( (HALF*(EPSPXX(I)-EPSPYY(I)))**2 + (HALF*EPSPXY(I))**2)
              EPSD1  = EPST_A + EPST_B
              EPSD2  = EPST_A - EPST_B
              EPSD1  = (ONE/EXP(EPST1))*EPSD1
              EPSD2  = (ONE/EXP(EPST2))*EPSD2
              EPSP11(I) = C*C*EPSD1 + S*S*EPSD2
              EPSP22(I) = S*S*EPSD1 + C*C*EPSD2
              EPSP12(I) = S*C*(EPSD2 - EPSD1)
            END IF
          ENDDO
c
        CASE DEFAULT
           ! no transformation : failure strain measure is defined by Ismstr
           EPS11(1:NEL)  = EPSXX(1:NEL)
           EPS22(1:NEL)  = EPSYY(1:NEL)
           EPS12(1:NEL)  = HALF*EPSXY(1:NEL)
           EPSP11(1:NEL) = EPSPXX(1:NEL)
           EPSP22(1:NEL) = EPSPYY(1:NEL)
           EPSP12(1:NEL) = HALF*EPSPXY(1:NEL)
      END SELECT 
c----------------------------------------------
c     Element size scale factor
      FSIZE => UVAR(1:NEL,32)
c
      IF (FSIZE(1)==ZERO) THEN
        IF (IFUNC_SIZ > 0) THEN
                  ESCAL(1:NEL) = ALDT(1:NEL) / REF_SIZ
          IPOSP(1:NEL) = 0
          IADP (1:NEL) = NPF(IFUNC_SIZ) / 2 + 1
          ILENP(1:NEL) = NPF(IFUNC_SIZ  + 1) / 2 - IADP(1:NEL)
          CALL VINTER2(TF,IADP,IPOSP,ILENP,NEL,ESCAL,DYDXE,FSIZE)
          FSIZE(1:NEL) = FSCALE_SIZ * FSIZE(1:NEL)
        ELSE
                  FSIZE(1:NEL) = ONE
        ENDIF
      ENDIF
c
      XX=1
      YY=2
      XY=3
c     variable initialisation, current time
      DO  J=1 ,6
        DO I=1, NEL
          DAM(I,J)  = UVAR(I,J)
          ODAM(I,J) = MIN(DAM(I,J),0.999)
          EPSP(I,J) = UVAR(I,J+6)
        ENDDO
      ENDDO  
      DO I =1, NEL 
       SIGOXX(I) = UVAR(I,12+XX)
       SIGOYY(I) = UVAR(I,12+YY)
       SIGOXY(I) = UVAR(I,12+XY)
      ENDDO
c      
c     strain rate filtering
      DO I=1,NEL
        EPSP(I,XX) = ALPHA * ABS(EPSP11(I)) + (ONE-ALPHA)*EPSP(I,XX) 
        EPSP(I,YY) = ALPHA * ABS(EPSP22(I)) + (ONE-ALPHA)*EPSP(I,YY) 
        EPSP(I,XY) = ALPHA * ABS(EPSP12(I)) + (ONE-ALPHA)*EPSP(I,XY)
      ENDDO 
c
c     failure and damage calculation in each mode (dir)
c
      NINDX = 0  
      DO I=1, NEL
        IF (OFF(I) == ONE .and. OFFLY(I) == 1 .and. FOFF(I) == 1) THEN
          ET1    = UPARAM(4)
          ET1M   = UPARAM(5)
          ET2    = UPARAM(6)
          ET2M   = UPARAM(7) 
          EC1    = UPARAM(8)
          EC1M   = UPARAM(9)
          EC2    = UPARAM(10)
          EC2M   = UPARAM(11)
          ET12   = UPARAM(12)
          ET12M  = UPARAM(13)
          EC12   = UPARAM(14)
          EC12M  = UPARAM(15)
          ET3    = UPARAM(18)
          ET3M   = UPARAM(19)
          EC3    = UPARAM(20)
          EC3M   = UPARAM(21)
          ET23   = UPARAM(22)
          ET23M  = UPARAM(23)
          EC23   = UPARAM(24)
          EC23M  = UPARAM(25)
          ET31   = UPARAM(22)
          ET31M  = UPARAM(23)
          EC31   = UPARAM(24)
          EC31M  = UPARAM(25)
c
          FMODE(I,1:6) = 0
          FAILI = 0
c
          MODE  = 1   ! traction xx
          EPDOT = ABS(EPSP(I,XX))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
                FRATE = FRATE*FSIZE(I)
          ET1M = FRATE * ET1M
          ET1  = FRATE * ET1
          DEPS = EPS11(I) - ET1 
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = ET1M - ET1
            FACT  = ET1M / EPS11(I)
            DI    = FACT * DEPS / DEPSM 
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
              EPSM1(I) = ET1M
            ENDIF
          ENDIF
c
          MODE  = 2   ! traction yy
          EPDOT = ABS(EPSP(I,YY))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
                  FRATE = FRATE*FSIZE(I)
          ET2M = FRATE * ET2M
          ET2  = FRATE * ET2
          DEPS = MAX(EPS22(I) - ET2, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (ET2M - ET2)
            FACT  = ET2M / EPS22(I)
            DI    = FACT * DEPS / DEPSM 
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
              EPSM2(I) = ET2M
            ENDIF
          ENDIF
c
          MODE  = 3   ! traction xy
          EPDOT = ABS(EPSP(I,XY))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
                  FRATE = FRATE*FSIZE(I)
          ET12M = FRATE * ET12M
          ET12  = FRATE * ET12
          DEPS  = MAX(EPS12(I) - ET12, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (ET12M - ET12)
            FACT  = ET12M / EPS12(I)
            DI    = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
              EPSM3(I) = ET12M
            ENDIF
          ENDIF
c
          MODE  = 4   ! compression xx
          EPDOT = ABS(EPSP(I,XX))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
                  FRATE = FRATE*FSIZE(I)
          EC1M = FRATE * EC1M
          EC1  = FRATE * EC1
          DEPS = -EPS11(I) - EC1
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (EC1M - EC1)
            FACT = EC1M / ABS(EPS11(I))
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
              EPSM4(I) = -EC1M
            ENDIF
          ENDIF
c
          MODE = 5   ! compression yy
          EPDOT = ABS(EPSP(I,YY))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
                  FRATE = FRATE*FSIZE(I)
          EC2M = FRATE * EC2M
          EC2  = FRATE * EC2
          DEPS = MAX(-EPS22(I) - EC2, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (EC2M - EC2)
            FACT  = EC2M / ABS(EPS22(I))
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
              EPSM5(I) = -EC2M
            ENDIF
          ENDIF
c
          MODE = 6   ! compression xy
          EPDOT = ABS(EPSP(I,XY))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
                  FRATE = FRATE*FSIZE(I)
          EC12M = FRATE * EC12M
          EC12  = FRATE * EC12
          DEPS  = MAX(-EPS12(I) - EC12, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (EC12M - EC12)
            FACT  = EC12M / ABS(EPS12(I))
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
              EPSM6(I) = -EC12M
            ENDIF
          ENDIF
c-----
          IF (FAILI == 1) THEN
            FOFF(I) = 0
            TDEL(I) = TIME 
            NINDX   = NINDX + 1  
            INDX(NINDX) = I
          ENDIF
        ENDIF
      ENDDO  
c------------------------
      IF (NINDX > 0) THEN 
        DO J=1,NINDX    
          I = INDX(J)
#include  "lockon.inc"
          
          WRITE(IOUT, 1000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,1000) NGL(I),IPG,ILAY,IPT
          IF (FMODE(I,1)==1) WRITE(IOUT, 2000) '1 - TRACTION XX',EPS11(I),EPSM1(I),EPSP(I,XX)
          IF (FMODE(I,2)==1) WRITE(IOUT, 2000) '2 - TRACTION YY',EPS22(I),EPSM2(I),EPSP(I,YY)
          IF (FMODE(I,3)==1) WRITE(IOUT, 2000) '3 - POSITIVE SHEAR XY',EPS12(I),EPSM3(I),EPSP(I,XY)
          IF (FMODE(I,4)==1) WRITE(IOUT, 2000) '4 - COMPRESSION XX',EPS11(I),EPSM4(I),EPSP(I,XX)
          IF (FMODE(I,5)==1) WRITE(IOUT, 2000) '5 - COMPRESSION YY',EPS22(I),EPSM5(I),EPSP(I,YY)
          IF (FMODE(I,6)==1) WRITE(IOUT, 2000) '6 - NEGATIVE SHEAR XY',EPS12(I),EPSM6(I),EPSP(I,XY)
#include  "lockoff.inc"
        END DO
      END IF              
c-----------------------------------------------------------------------
c     Damage stress reduction
c     damage / diretion is equal to the max of damages par mode in the same direction
c-----------------------------------------------------------------------
      IF (DMG_FLAG == 1) THEN   ! scalar damage applied to nodal force contribution
        DO I=1, NEL
          DAMXX  = MAX(DAM(I,1),DAM(I,4))
          DAMYY  = MAX(DAM(I,2),DAM(I,5))
          DAMXY  = MAX(DAM(I,3),DAM(I,6))
          DFMAX(I) = MAX(DFMAX(I), DAMXX)
          DFMAX(I) = MAX(DFMAX(I), DAMYY)
          DFMAX(I) = MAX(DFMAX(I), DAMXY)
          DFMAX(I) = MIN(ONE,DFMAX(I))
          DMG_SCALE(I) = ONE - DFMAX(I)
        ENDDO
      ELSE                      ! directional damage applied to stress
        DO I=1, NEL
          DAMXX  = MAX(DAM(I,1),DAM(I,4))
          DAMYY  = MAX(DAM(I,2),DAM(I,5))
          DAMXY  = MAX(DAM(I,3),DAM(I,6))
          DFMAX(I) = MAX(DFMAX(I), DAMXX)
          DFMAX(I) = MAX(DFMAX(I), DAMYY)
          DFMAX(I) = MAX(DFMAX(I), DAMXY)
          DFMAX(I) = MIN(ONE,DFMAX(I))
          ODAMXX = MAX(ODAM(I,1),ODAM(I,4))
          ODAMYY = MAX(ODAM(I,2),ODAM(I,5))
          ODAMXY = MAX(ODAM(I,3),ODAM(I,6))
c         Undamaged stress estimate
          SIGNXX(I) = SIGOXX(I)/(ONE-ODAMXX) + (SIGNXX(I) - SIGOXX(I))
          SIGNYY(I) = SIGOYY(I)/(ONE-ODAMYY) + (SIGNYY(I) - SIGOYY(I))
          SIGNXY(I) = SIGOXY(I)/(ONE-ODAMXY) + (SIGNXY(I) - SIGOXY(I))
c         Damaged stress
          SIGNXX(I) = SIGNXX(I)* (ONE-DAMXX)
          SIGNYY(I) = SIGNYY(I)* (ONE-DAMYY)
          SIGNXY(I) = SIGNXY(I)* (ONE-DAMXY)
          UVAR(I,13) = SIGNXX(I)
          UVAR(I,14) = SIGNYY(I)
          UVAR(I,15) = SIGNXY(I)
        ENDDO
      ENDIF
c     Update state variables
      DO  J=1 ,6
        DO I=1, NEL
          UVAR(I,J)   = DAM(I,J)
          UVAR(I,J+6) = EPSP(I,J)
        ENDDO
      ENDDO  
c-----------------------------------------------------------------------
 1000 FORMAT(1X,'FAILURE (ORTHSTR) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2000 FORMAT(1X,'MODE',1X,A,', STRAIN',1PE12.4,', CRITICAL VALUE',1PE12.4,
     .       ', STRAIN RATE',1PE12.4)
c-----------------------------------------------------------------------
      RETURN          
      END
