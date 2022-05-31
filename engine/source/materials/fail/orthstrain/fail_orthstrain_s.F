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
     7        UELR     ,NPT    ,DELTAX ,MFXX   ,MFXY    ,MFXZ     ,               
     8        MFYX     ,MFYY   ,MFYZ   ,MFZX   ,MFZY    ,MFZZ     )
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
     .  EPSXY,EPSYZ,EPSZX,EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX,
     .  MFXX,MFXY,MFXZ,MFYX,MFYY,MFYZ,MFZX,MFZY,MFZZ
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
c=======================================================================
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
        IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 1
        ELSE IF (ISMSTR == 0 .or. ISMSTR == 2 .or. ISMSTR == 4) THEN
          STRFLAG = 2
        END IF
      ELSE IF (STRDEF == 3) THEN   ! failure defined as true strain
        IF (ISMSTR == 1 .or. ISMSTR == 3 .or. ISMSTR == 11) THEN
          STRFLAG = 3
        ELSE IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 4
        END IF
      END IF           
c--------------------------
      SELECT CASE (STRFLAG)
        CASE (1)  !  transform Grad def to engineering
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS11(I)  = MFXX(I)
              EPS22(I)  = MFYY(I)
              EPS33(I)  = MFZZ(I)
              EPS12(I)  = MFXY(I) + MFYX(I)
              EPS23(I)  = MFXZ(I) + MFZX(I)
              EPS31(I)  = MFZY(I) + MFYZ(I)
            END IF
          ENDDO
c
        CASE (2)  !  transform true strain to engineering
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS11(I)  = EXP(EPSXX(I)) - ONE
              EPS22(I)  = EXP(EPSYY(I)) - ONE
              EPS33(I)  = EXP(EPSZZ(I)) - ONE
              EPS12(I)  = EXP(EPSXY(I)) - ONE
              EPS23(I)  = EXP(EPSYZ(I)) - ONE
              EPS31(I)  = EXP(EPSZX(I)) - ONE
              EPSP11(I) = EXP(EPSPXX(I)) - ONE
              EPSP22(I) = EXP(EPSPYY(I)) - ONE
              EPSP33(I) = EXP(EPSPZZ(I)) - ONE
              EPSP12(I) = EXP(EPSPXY(I)) - ONE
              EPSP23(I) = EXP(EPSPYZ(I)) - ONE
              EPSP31(I) = EXP(EPSPZX(I)) - ONE
            END IF
          ENDDO
c
        CASE (3)  !  transform engineering to true strain
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS11(I)  = LOG(EPSXX(I) + ONE)
              EPS22(I)  = LOG(EPSYY(I) + ONE)
              EPS33(I)  = LOG(EPSZZ(I) + ONE)
              EPS12(I)  = LOG(EPSXY(I) + ONE)
              EPS23(I)  = LOG(EPSYZ(I) + ONE)
              EPS31(I)  = LOG(EPSZX(I) + ONE)
              EPSP11(I) = LOG(EPSPXX(I) + ONE)
              EPSP22(I) = LOG(EPSPYY(I) + ONE)
              EPSP33(I) = LOG(EPSPZZ(I) + ONE)
              EPSP12(I) = LOG(EPSPXY(I) + ONE)
              EPSP23(I) = LOG(EPSPYZ(I) + ONE)
              EPSP31(I) = LOG(EPSPZX(I) + ONE)
            END IF
          ENDDO
c
        CASE (4)  !  transform Cauchy-Green to true strain
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS11(I)  = LOG(MFXX(I) + ONE)
              EPS22(I)  = LOG(MFYY(I) + ONE)
              EPS33(I)  = LOG(MFZZ(I) + ONE)
              EPS12(I)  = LOG(MFXY(I) + MFYX(I) + ONE)
              EPS23(I)  = LOG(MFXZ(I) + MFZX(I) + ONE)
              EPS31(I)  = LOG(MFZY(I) + MFYZ(I) + ONE)
              EPSP11(I) = LOG(SQRT(EPSPXX(I)+ONE))
              EPSP22(I) = LOG(SQRT(EPSPYY(I)+ONE))
              EPSP33(I) = LOG(SQRT(EPSPZZ(I)+ONE))
              EPSP12(I) = LOG(SQRT(EPSPXY(I)+ONE))
              EPSP23(I) = LOG(SQRT(EPSPYZ(I)+ONE))
              EPSP31(I) = LOG(SQRT(EPSPZX(I)+ONE))
            END IF
          ENDDO
c
        CASE DEFAULT
          ! no transformation : failure strain measure is defined by Ismstr
          EPS11(1:NEL)  = EPSXX(1:NEL)
          EPS22(1:NEL)  = EPSYY(1:NEL)
          EPS33(1:NEL)  = EPSZZ(1:NEL)
          EPS12(1:NEL)  = EPSXY(1:NEL)
          EPS23(1:NEL)  = EPSYZ(1:NEL)
          EPS31(1:NEL)  = EPSZX(1:NEL)
          EPSP11(1:NEL) = EPSPXX(1:NEL)
          EPSP22(1:NEL) = EPSPYY(1:NEL)
          EPSP33(1:NEL) = EPSPZZ(1:NEL)
          EPSP12(1:NEL) = EPSPXY(1:NEL)
          EPSP23(1:NEL) = EPSPYZ(1:NEL)
          EPSP31(1:NEL) = EPSPZX(1:NEL)
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
 1000 FORMAT(1X,'FAILURE (ORTHSTR) OF SOLID ELEMENT ',I10,1X,',GAUSS PT',I2,
     .       1X,'MODE',1X,I2)
 2000 FORMAT(1X,'FAILURE (ORTHSTR) OF SOLID ELEMENT ',I10,1X,',GAUSS PT',I2,
     .       1X,'MODE',1X,I2,1X,'STRAIN RATE',1PE12.4)
 3000 FORMAT(1X,'RUPTURE OF ELEMENT #',I10,1X,'AT TIME # ',1PE12.4)    
c-----------------------------------------------------------------------
      RETURN          
      END
