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
Chd|  FAIL_ORTHSTRAIN               source/materials/fail/orthstrain/fail_orthstrain_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
              SUBROUTINE FAIL_ORTHSTRAIN ( 
     1        NEL      ,NUPARAM,NUVAR  ,NFUNC   ,IFUNC , 
     2        NPF      ,TF     ,TIME   ,TIMESTEP,UPARAM ,ISMSTR   , 
     3        EPSPXX   ,EPSPYY ,EPSPZZ ,EPSPXY  ,EPSPYZ ,EPSPZX   ,
     4        EPSXX    ,EPSYY  ,EPSZZ  ,EPSXY   ,EPSYZ  ,EPSZX    ,
     5        SIGNXX   ,SIGNYY ,SIGNZZ ,SIGNXY  ,SIGNYZ ,SIGNZX   ,
     6        UVAR     ,OFF    ,IPT    ,NGL     ,DFMAX  ,TDEL     ,
     7        UELR     ,NPT    ,DELTAX )
C-----------------------------------------------
c    Orthotropic strain failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C--------------------------------------------
      INTEGER :: NEL,NUPARAM,NUVAR,IPT,NPT,ISMSTR
      INTEGER ,DIMENSION(NEL) :: NGL
      my_real :: TIME,TIMESTEP
      my_real ,DIMENSION(NUPARAM) :: UPARAM
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: EPSXX,EPSYY,EPSZZ,
     .  EPSXY,EPSYZ,EPSZX,EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX
      my_real ,DIMENSION(NEL)     :: UELR,DELTAX,
     .  SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR),OFF(NEL),DFMAX(NEL),TDEL(NEL)
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
      INTEGER I,J,NINDX,FAILI,MODE,XX,XY,YY,ZZ,YZ,ZX,IFUNC_SIZ,STRDEF,STRFLAG,
     .    M11T, M11C,M22T,M22C,M33T,M33C,M12T,M12C,M23T,M23C,M31T,M31C
      INTEGER ,DIMENSION(NEL)    :: INDX,IPOSP,IADP,ILENP
      INTEGER ,DIMENSION(NEL,12) :: FMODE
      my_real  DAM(NEL,12),ODAM(NEL,12),EPSP(NEL,6),SIGO(NEL,6),FSIZE(NEL),
     .   ESCAL(NEL),DYDX(NEL)
      my_real ,DIMENSION(NEL)  :: EPS11,EPS22,EPS33,EPS12,EPS23,EPS31,
     .     EPSP11,EPSP22,EPSP33,EPSP12,EPSP23,EPSP31 
      my_real  FRATE,EPSPREF,EPDOT,ALPHA,DEPSM,DEPS,FACT,P,I1,I2,I3,Q,R,R_INTER,
     .    PHI,ET1,ET2,ET12,EC1,EC2,EC12,ET3,EC3,EC23,ET23,EC31,ET31,
     .    ET1M,ET2M,ET12M,EC1M,EC2M,EC12M,ET3M,EC3M,EC23M,ET23M,EC31M,ET31M,
     .    DI,DAMXX,DAMYY,DAMXY,DAMZZ,DAMYZ,DAMZX, 
     .    ODAMXX,ODAMYY,ODAMXY,ODAMZZ,ODAMYZ,ODAMZX,FSCALE_SIZ,REF_SIZ
      my_real  EPSTENS(3,3),EPSTENS2(3,3),EPS_PR(3),VECT_PR(3,3),
     .         EPSPTENS(3,3),EPSPTENS2(3,3),EPSP_PR(3)
      INTEGER  NROT,K,L,M
C=======================================================================
      ALPHA  = UPARAM(16)
      EPSPREF= UPARAM(17)
      FSCALE_SIZ = UPARAM(26)
      REF_SIZ    = UPARAM(27)
      STRDEF     = NINT(UPARAM(28))
      IFUNC_SIZ  = IFUNC(13)
c----------------------------------------------
c     strain transformation following input definition
c-------------------
      STRFLAG = 0
      IF (STRDEF == 2) THEN        ! failure defined as engineering strain
        IF (ISMSTR /=1 .AND. ISMSTR /=3 .AND. ISMSTR /= 11) THEN
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
              ! -> Fill the strain tensor 3x3
              EPSTENS(1,1) = EPSXX(I)
              EPSTENS(2,2) = EPSYY(I)
              EPSTENS(3,3) = EPSZZ(I)
              EPSTENS(1,2) = HALF*EPSXY(I)
              EPSTENS(2,3) = HALF*EPSYZ(I)
              EPSTENS(1,3) = HALF*EPSZX(I)
              EPSTENS(2,1) = EPSTENS(1,2)
              EPSTENS(3,1) = EPSTENS(1,3)
              EPSTENS(3,2) = EPSTENS(2,3)
              ! -> Compute principal strains and vectors
              CALL JACOBIEW(EPSTENS,3,EPS_PR,VECT_PR,NROT)
              ! -> Transform true principal strains to engineering principal strains
              EPS_PR(1) = EXP(EPS_PR(1)) - ONE
              EPS_PR(2) = EXP(EPS_PR(2)) - ONE
              EPS_PR(3) = EXP(EPS_PR(3)) - ONE 
              ! -> Recover engineering strain tensor 
              EPSTENS(1:3,1:3) = ZERO
              EPSTENS(1,1) = EPS_PR(1)
              EPSTENS(2,2) = EPS_PR(2)
              EPSTENS(3,3) = EPS_PR(3)
              !  --> Principal vector*Principal strains*Transpose(Principal vector)
              DO K = 1,3
                DO L = 1,3
                  EPSTENS2(K,L) = ZERO
                  DO M = 1,3
                    EPSTENS2(K,L) = EPSTENS2(K,L) + EPSTENS(K,M)*VECT_PR(L,M)
                  ENDDO
                ENDDO
              ENDDO
              DO K = 1,3
                DO L = 1,3
                  EPSTENS(K,L) = ZERO
                  DO M = 1,3
                    EPSTENS(K,L) = EPSTENS(K,L) + VECT_PR(K,M)*EPSTENS2(M,L)
                  ENDDO
                ENDDO
              ENDDO
              ! -> Save engineering strains
              EPS11(I)  = EPSTENS(1,1)
              EPS22(I)  = EPSTENS(2,2)
              EPS33(I)  = EPSTENS(3,3)
              EPS12(I)  = EPSTENS(1,2)
              EPS23(I)  = EPSTENS(2,3)
              EPS31(I)  = EPSTENS(3,1)
              ! -> Fill the strain rate tensor
              EPSPTENS(1,1) = EPSPXX(I)
              EPSPTENS(2,2) = EPSPYY(I)
              EPSPTENS(3,3) = EPSPZZ(I)
              EPSPTENS(1,2) = HALF*EPSPXY(I)
              EPSPTENS(2,3) = HALF*EPSPYZ(I)
              EPSPTENS(1,3) = HALF*EPSPZX(I)
              EPSPTENS(2,1) = EPSPTENS(1,2)
              EPSPTENS(3,1) = EPSPTENS(1,3)
              EPSPTENS(3,2) = EPSPTENS(2,3)
              ! -> Compute principal strain rates and vectors
              CALL JACOBIEW(EPSPTENS,3,EPSP_PR,VECT_PR,NROT)
              ! -> Transform true principal strain rates to engineering principal strain rates
              EPSP_PR(1) = (EPS_PR(1)+ONE)*EPSP_PR(1)
              EPSP_PR(2) = (EPS_PR(2)+ONE)*EPSP_PR(2)
              EPSP_PR(3) = (EPS_PR(3)+ONE)*EPSP_PR(3)
              ! -> Recover engineering strain rate tensor 
              EPSPTENS(1:3,1:3) = ZERO
              EPSPTENS(1,1) = EPSP_PR(1)
              EPSPTENS(2,2) = EPSP_PR(2)
              EPSPTENS(3,3) = EPSP_PR(3)
              !  --> Principal vector*Principal strains*Transpose(Principal vector)
              DO K = 1,3
                DO L = 1,3
                  EPSPTENS2(K,L) = ZERO
                  DO M = 1,3
                    EPSPTENS2(K,L) = EPSPTENS2(K,L) + EPSPTENS(K,M)*VECT_PR(L,M)
                  ENDDO
                ENDDO
              ENDDO
              DO K = 1,3
                DO L = 1,3
                  EPSPTENS(K,L) = ZERO
                  DO M = 1,3
                    EPSPTENS(K,L) = EPSPTENS(K,L) + VECT_PR(K,M)*EPSPTENS2(M,L)
                  ENDDO
                ENDDO
              ENDDO
              ! -> Save engineering strain rates
              EPSP11(I) = EPSPTENS(1,1)
              EPSP22(I) = EPSPTENS(2,2)
              EPSP33(I) = EPSPTENS(3,3)
              EPSP12(I) = EPSPTENS(1,2)
              EPSP23(I) = EPSPTENS(2,3)
              EPSP31(I) = EPSPTENS(3,1)
            END IF
          ENDDO
c
        CASE (3)  !  transform engineering to true strain
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              ! -> Fill the strain tensor 3x3
              EPSTENS(1,1) = EPSXX(I)
              EPSTENS(2,2) = EPSYY(I)
              EPSTENS(3,3) = EPSZZ(I)
              EPSTENS(1,2) = HALF*EPSXY(I)
              EPSTENS(2,3) = HALF*EPSYZ(I)
              EPSTENS(1,3) = HALF*EPSZX(I)
              EPSTENS(2,1) = EPSTENS(1,2)
              EPSTENS(3,1) = EPSTENS(1,3)
              EPSTENS(3,2) = EPSTENS(2,3)
              ! -> Compute principal strains and vectors
              CALL JACOBIEW(EPSTENS,3,EPS_PR,VECT_PR,NROT)
              ! -> Transform engineering principal strains to true principal strains
              EPS_PR(1) = LOG(ONE + EPS_PR(1))
              EPS_PR(2) = LOG(ONE + EPS_PR(2))
              EPS_PR(3) = LOG(ONE + EPS_PR(3)) 
              ! -> Recover true strain tensor 
              EPSTENS(1:3,1:3) = ZERO
              EPSTENS(1,1) = EPS_PR(1)
              EPSTENS(2,2) = EPS_PR(2)
              EPSTENS(3,3) = EPS_PR(3)
              !  --> Principal vector*Principal strains*Transpose(Principal vector)
              DO K = 1,3
                DO L = 1,3
                  EPSTENS2(K,L) = ZERO
                  DO M = 1,3
                    EPSTENS2(K,L) = EPSTENS2(K,L) + EPSTENS(K,M)*VECT_PR(L,M)
                  ENDDO
                ENDDO
              ENDDO
              DO K = 1,3
                DO L = 1,3
                  EPSTENS(K,L) = ZERO
                  DO M = 1,3
                    EPSTENS(K,L) = EPSTENS(K,L) + VECT_PR(K,M)*EPSTENS2(M,L)
                  ENDDO
                ENDDO
              ENDDO
              ! -> Save true strains
              EPS11(I)  = EPSTENS(1,1)
              EPS22(I)  = EPSTENS(2,2)
              EPS33(I)  = EPSTENS(3,3)
              EPS12(I)  = EPSTENS(1,2)
              EPS23(I)  = EPSTENS(2,3)
              EPS31(I)  = EPSTENS(3,1)
              ! -> Fill the strain rate tensor
              EPSPTENS(1,1) = EPSPXX(I)
              EPSPTENS(2,2) = EPSPYY(I)
              EPSPTENS(3,3) = EPSPZZ(I)
              EPSPTENS(1,2) = HALF*EPSPXY(I)
              EPSPTENS(2,3) = HALF*EPSPYZ(I)
              EPSPTENS(1,3) = HALF*EPSPZX(I)
              EPSPTENS(2,1) = EPSPTENS(1,2)
              EPSPTENS(3,1) = EPSPTENS(1,3)
              EPSPTENS(3,2) = EPSPTENS(2,3)
              ! -> Compute principal strain rates and vectors
              CALL JACOBIEW(EPSPTENS,3,EPSP_PR,VECT_PR,NROT)
              ! -> Transform engineering principal strain rates to true principal strain rates
              EPSP_PR(1) = (ONE/(EXP(EPS_PR(1))))*EPSP_PR(1)
              EPSP_PR(2) = (ONE/(EXP(EPS_PR(2))))*EPSP_PR(2)
              EPSP_PR(3) = (ONE/(EXP(EPS_PR(3))))*EPSP_PR(3)
              ! -> Recover true strain rates tensor 
              EPSPTENS(1:3,1:3) = ZERO
              EPSPTENS(1,1) = EPSP_PR(1)
              EPSPTENS(2,2) = EPSP_PR(2)
              EPSPTENS(3,3) = EPSP_PR(3)
              !  --> Principal vector*Principal strains*Transpose(Principal vector)
              DO K = 1,3
                DO L = 1,3
                  EPSPTENS2(K,L) = ZERO
                  DO M = 1,3
                    EPSPTENS2(K,L) = EPSPTENS2(K,L) + EPSPTENS(K,M)*VECT_PR(L,M)
                  ENDDO
                ENDDO
              ENDDO
              DO K = 1,3
                DO L = 1,3
                  EPSPTENS(K,L) = ZERO
                  DO M = 1,3
                    EPSPTENS(K,L) = EPSPTENS(K,L) + VECT_PR(K,M)*EPSPTENS2(M,L)
                  ENDDO
                ENDDO
              ENDDO
              ! -> Save true strain rates
              EPSP11(I) = EPSPTENS(1,1)
              EPSP22(I) = EPSPTENS(2,2)
              EPSP33(I) = EPSPTENS(3,3)
              EPSP12(I) = EPSPTENS(1,2)
              EPSP23(I) = EPSPTENS(2,3)
              EPSP31(I) = EPSPTENS(3,1)
            END IF
          ENDDO
c
        CASE DEFAULT
          ! no transformation : failure strain measure is defined by Ismstr
          EPS11(1:NEL)  = EPSXX(1:NEL)
          EPS22(1:NEL)  = EPSYY(1:NEL)
          EPS33(1:NEL)  = EPSZZ(1:NEL)
          EPS12(1:NEL)  = HALF*EPSXY(1:NEL)
          EPS23(1:NEL)  = HALF*EPSYZ(1:NEL)
          EPS31(1:NEL)  = HALF*EPSZX(1:NEL)
          EPSP11(1:NEL) = EPSPXX(1:NEL)
          EPSP22(1:NEL) = EPSPYY(1:NEL)
          EPSP33(1:NEL) = EPSPZZ(1:NEL)
          EPSP12(1:NEL) = HALF*EPSPXY(1:NEL)
          EPSP23(1:NEL) = HALF*EPSPYZ(1:NEL)
          EPSP31(1:NEL) = HALF*EPSPZX(1:NEL)
      END SELECT
c------------------------------
c     Element size scale factor
      IF (IFUNC_SIZ > 0) THEN
	       ESCAL(1:NEL) = FSCALE_SIZ * DELTAX(1:NEL) / REF_SIZ
        IPOSP(1:NEL) = NINT(UVAR(1:NEL,32))
        IADP (1:NEL) = NPF(IFUNC_SIZ) / 2 + 1
        ILENP(1:NEL) = NPF(IFUNC_SIZ  + 1) / 2 - IADP(1:NEL) - IPOSP(1:NEL)
c
        CALL VINTER2(TF,IADP,IPOSP,ILENP,NEL,ESCAL,DYDX,FSIZE)
      ELSE
	       FSIZE(1:NEL) = ONE
      ENDIF
c------------------------------
c
      XX=1
      YY=2
      ZZ=3
      XY=4
      YZ=5
      ZX=6
      M11T = 1
      M11C = 4
      M22T = 2 
      M22C = 5
      M33T = 7
      M33C = 8
      M12T = 3
      M12C = 6
      M23T = 9
      M23C = 10
      M31T = 11
      M31C = 12
c     current variable initialisation
      DO I=1, NEL
        DO J=1 ,12
          DAM(I,J)  = UVAR(I,J+12)
          ODAM(I,J) = MIN(DAM(I,J),0.999)
        ENDDO
        DO J=1 ,6
          SIGO(I,J) = UVAR(I,J)
          EPSP(I,J) = UVAR(I,J+6)
        ENDDO
      ENDDO  
c     
c strain rate filtering
      DO I=1,NEL
        EPSP(I,XX) = ALPHA * ABS(EPSP11(I)) + (ONE-ALPHA)*EPSP(I,XX) 
        EPSP(I,YY) = ALPHA * ABS(EPSP22(I)) + (ONE-ALPHA)*EPSP(I,YY) 
        EPSP(I,ZZ) = ALPHA * ABS(EPSP33(I)) + (ONE-ALPHA)*EPSP(I,ZZ) 
        EPSP(I,XY) = ALPHA * ABS(EPSP12(I)) + (ONE-ALPHA)*EPSP(I,XY)
        EPSP(I,YZ) = ALPHA * ABS(EPSP23(I)) + (ONE-ALPHA)*EPSP(I,YZ)
        EPSP(I,ZX) = ALPHA * ABS(EPSP31(I)) + (ONE-ALPHA)*EPSP(I,ZX)
      ENDDO 
c
c damage and failure calculation in each mode (dir)
c
      NINDX = 0  
      DO I=1, NEL
        IF (OFF(I) == ONE) THEN
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
          FMODE(I,1:12) = 0
          FAILI = 0
c
c         MODE XX TRACTION
c
          MODE  = 1
          EPDOT = ABS(EPSP(I,XX))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
		        FRATE = FRATE*FSIZE(I)
          ET1M = FRATE * ET1M
          ET1  = FRATE * ET1
          DEPS = MAX(EPS11(I) - ET1, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (ET1M - ET1)
            FACT  = ET1M / EPS11(I)
            DI = FACT*DEPS/DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
            ENDIF
          ENDIF
c
c         MODE YY TRACTION				
c
          MODE  = 2
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
            ENDIF
          ENDIF
c		
c         MODE XY TRACTION
c
          MODE  = 3
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
            ENDIF
          ENDIF
c
c		       MODE XX COMPRESSION
c         		
          MODE  = 4
          EPDOT = ABS(EPSP(I,XX))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
          FRATE = FRATE*FSIZE(I)
          EC1M = FRATE * EC1M
          EC1  = FRATE * EC1
          DEPS = MAX(-EPS11(I) - EC1, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (EC1M - EC1)
            FACT = EC1M / ABS(EPS11(I))
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
            ENDIF
          ENDIF
c
c		       MODE YY COMPRESSION	 
c         	
          MODE = 5
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
            ENDIF
          ENDIF
c		
c         MODE XY COMPRESSION		
c
          MODE  = 6
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
            ENDIF
          ENDIF
c		
c         MODE ZZ Traction		
c
          MODE  = M33T
          EPDOT = ABS(EPSP(I,ZZ))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
          FRATE = FRATE*FSIZE(I)
          ET3M = FRATE * ET3M
          ET3  = FRATE * ET3
          DEPS = MAX(EPS33(I) - ET3, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (ET3M - ET3)
            FACT = ET3M / EPS33(I)
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
            ENDIF
          ENDIF
c		
c         MODE ZZ Compresion		
c
          MODE  = M33C
          EPDOT = ABS(EPSP(I,ZZ))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
          FRATE = FRATE*FSIZE(I)
          EC3M = FRATE * EC3M
          EC3  = FRATE * EC3
          DEPS = MAX(-EPS33(I) - EC3, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (EC3M - EC3)
            FACT = EC3M / ABS(EPS33(I))
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
            ENDIF
          ENDIF
c		
c         MODE YZ TRACT
c
          MODE  = M23T
          EPDOT = ABS(EPSP(I,YZ))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
          FRATE = FRATE*FSIZE(I)
          ET23M = FRATE * ET23M
          ET23  = FRATE * ET23
          DEPS  = MAX(EPS23(I) - ET23, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (ET23M - ET23)
            FACT = ET23M / EPS23(I)
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
            ENDIF
          ENDIF
c		
c         MODE YZ COMP		
c
          MODE  = M23C
          EPDOT = ABS(EPSP(I,YZ))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
          FRATE = FRATE*FSIZE(I)
          EC23M = FRATE * EC23M
          EC23  = FRATE * EC23
          DEPS  = MAX(-EPS23(I) - EC23, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (EC23M - EC23)
            FACT  = EC23M / ABS(EPS23(I))
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
            ENDIF
          ENDIF
c		
c         MODE ZX Traction
c
          MODE  = M31T
          EPDOT = ABS(EPSP(I,ZX))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
          FRATE = FRATE*FSIZE(I)
          ET31M = FRATE * ET31M
          ET31  = FRATE * ET31
          DEPS  = MAX(EPS31(I) - ET31, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (ET31M - ET31)
            FACT  = ET31M / EPS31(I)
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
            ENDIF
          ENDIF
c		
c         mode ZX comp		
c
          MODE  = M31C  ! Mode 12
          EPDOT = ABS(EPSP(I,ZX))/EPSPREF
          IF (IFUNC(MODE) > 0) THEN
            FRATE = FINTER(IFUNC(MODE),EPDOT,NPF,TF,DYDX)
          ELSE
            FRATE = ONE
          ENDIF
          FRATE = FRATE*FSIZE(I)
          EC31M = FRATE * EC31M
          EC31  = FRATE * EC31
          DEPS  = MAX(-EPS31(I) - EC31, ZERO)
          IF (DEPS > ZERO .and. DAM(I,MODE) < ONE) THEN
            DEPSM = (EC31M - EC31)
            FACT  = EC31M / ABS(EPS31(I))
            DI = FACT * DEPS / DEPSM
            DAM(I,MODE) = MAX(DAM(I,MODE), DI)
            IF (DAM(I,MODE) >= ONE) THEN
              FAILI = 1
              FMODE(I,MODE) = 1
              DAM(I,MODE) = ONE
            ENDIF
          ENDIF
c
c
          IF (FAILI == 1) THEN
            FAILI   = 0
            UELR(I) = UELR(I)+1
            NINDX   = NINDX + 1  
            INDX(NINDX) = I
            IF (UELR(I) >= NPT) THEN
		            UVAR(I,31) = ONE
              TDEL(I) = TIME 
              OFF(I)  = FOUR_OVER_5 
              P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
              SIGNXX(I) = P
              SIGNYY(I) = P
              SIGNZZ(I) = P
              SIGNXY(I) = ZERO
              SIGNYZ(I) = ZERO
              SIGNZX(I) = ZERO
            ENDIF
          ENDIF
        ENDIF
      ENDDO  
c------------------------
c     Apply damage to stress
c------------------------
      DO I=1, NEL
c damage / direction is equal to the max of damages par mode in the same direction
         DAMXX  = MAX(DAM(I,1),DAM(I,4))
         DAMYY  = MAX(DAM(I,2),DAM(I,5))
         DAMZZ  = MAX(DAM(I,7),DAM(I,8))
         DAMXY  = MAX(DAM(I,3),DAM(I,6))
         DAMYZ  = MAX(DAM(I,9),DAM(I,10))
         DAMZX  = MAX(DAM(I,11),DAM(I,12))
c
         ODAMXX = MAX(ODAM(I,1),ODAM(I,4))
         ODAMYY = MAX(ODAM(I,2),ODAM(I,5))
         ODAMXY = MAX(ODAM(I,3),ODAM(I,6))
         ODAMZZ = MAX(ODAM(I,7),ODAM(I,8))
         ODAMYZ = MAX(ODAM(I,9),ODAM(I,10))
         ODAMZX = MAX(ODAM(I,11),ODAM(I,12))
c
         DFMAX(I) = MAX(DAMXX,DAMYY)
         DFMAX(I) = MAX(DAMZZ,DFMAX(I))
         DFMAX(I) = MAX(DAMXY,DFMAX(I))
         DFMAX(I) = MAX(DAMYZ,DFMAX(I))
         DFMAX(I) = MAX(DAMZX,DFMAX(I))
c Undamaged sign estimate
         SIGNXX(I) = SIGO(I,XX)/(ONE-ODAMXX) + (SIGNXX(I) - SIGO(I,XX))
         SIGNYY(I) = SIGO(I,YY)/(ONE-ODAMYY) + (SIGNYY(I) - SIGO(I,YY))
         SIGNZZ(I) = SIGO(I,ZZ)/(ONE-ODAMZZ) + (SIGNZZ(I) - SIGO(I,ZZ))
         SIGNXY(I) = SIGO(I,XY)/(ONE-ODAMXY) + (SIGNXY(I) - SIGO(I,XY))
         SIGNYZ(I) = SIGO(I,YZ)/(ONE-ODAMYZ) + (SIGNYZ(I) - SIGO(I,YZ))
         SIGNZX(I) = SIGO(I,ZX)/(ONE-ODAMZX) + (SIGNZX(I) - SIGO(I,ZX))
c Damaged sign
         SIGNXX(I) = SIGNXX(I) * (ONE-DAMXX)*OFF(I)
         SIGNYY(I) = SIGNYY(I) * (ONE-DAMYY)*OFF(I)
         SIGNZZ(I) = SIGNZZ(I) * (ONE-DAMZZ)*OFF(I)
         SIGNXY(I) = SIGNXY(I) * (ONE-DAMXY)*(ONE-DAMXX)*(ONE-DAMYY)*OFF(I)
c transverse stress are also affected by OFF(I)
         SIGNYZ(I) = SIGNYZ(I) * (ONE-DAMYZ)*(ONE-DAMXX)*(ONE-DAMYY)*OFF(I)
         SIGNZX(I) = SIGNZX(I) * (ONE-DAMZX)*(ONE-DAMXX)*(ONE-DAMYY)*OFF(I)
      ENDDO
c------------------------
c     Update UVAR
c------------------------
      DO  J=1 ,12
        DO I=1, NEL
          UVAR(I,J+12) = DAM(I,J)
        ENDDO
      ENDDO  
      DO  J=1 ,6
        DO I=1, NEL
          UVAR(I,J+6) = EPSP(I,J)
        ENDDO
      ENDDO  
      DO I =1, NEL 
        UVAR(I,XX) = SIGNXX(I)
        UVAR(I,YY) = SIGNYY(I)
        UVAR(I,ZZ) = SIGNZZ(I)
        UVAR(I,XY) = SIGNXY(I)
        UVAR(I,YZ) = SIGNYZ(I)
        UVAR(I,ZX) = SIGNZX(I)
      ENDDO
c------------------------
c     Output
c------------------------
      IF (NINDX > 0) THEN 
        DO J=1,NINDX    
          I = INDX(J)
          DO MODE = 1,12
            IF (FMODE(I,MODE) == 1) THEN
#include  "lockon.inc"
              WRITE(ISTDO,1000) NGL(I),IPT,MODE
              WRITE(IOUT, 2000) NGL(I),IPT,MODE,EPSP(I,XX)
#include  "lockoff.inc"
            ENDIF
          ENDDO
#include  "lockon.inc"
          IF (OFF(I) == FOUR_OVER_5) WRITE(IOUT, 3000) NGL(I),TIME
#include  "lockoff.inc"
        END DO
      END IF              
c------------------------
      IF (NINDX > 0) THEN 
        DO J=1,NINDX    
          I = INDX(J)
          IF (OFF(I) == FOUR_OVER_5) OFF(I) = ZERO
        END DO
      END IF              
c-----------------------------------------------------------------------
 1000 FORMAT(1X,'FAILURE (ORTHSTR) OF SOLID ELEMENT ',I10,1X,',GAUSS PT ',I5,
     .       1X,'MODE',1X,I2)
 2000 FORMAT(1X,'FAILURE (ORTHSTR) OF SOLID ELEMENT ',I10,1X,',GAUSS PT ',I5,
     .       1X,'MODE',1X,I2,1X,'STRAIN RATE',1PE12.4)
 3000 FORMAT(1X,'RUPTURE OF ELEMENT #',I10,1X,'AT TIME # ',1PE12.4)    
c-----------------------------------------------------------------------
      RETURN          
      END
