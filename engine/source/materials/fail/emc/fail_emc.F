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
Chd|  FAIL_EMC                      source/materials/fail/emc/fail_emc.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_EMC(
     1     NEL     ,NUVAR   ,NPF     ,TF      ,TIME    ,TIMESTEP,
     2     UPARAM  ,NGL     ,IPM     ,MAT     ,
     4     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     5     PLAS    ,DPLA    ,EPSP    ,UVAR    ,
     6     OFF     ,IP      ,DFMAX   ,TDELE   )
C---------+---------+---+---+--------------------------------------------
c---------+---------+---+---+--------------------------------------------
C   /FAIL/EMC - tabulated rupture criteria for solids
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
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
#include "units_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUVAR,IP
      INTEGER NGL(NEL),IPM(NPROPMI,*),MAT(NEL)
c
      my_real TIME,TIMESTEP,UPARAM(*),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   PLAS(NEL),DPLA(NEL),EPSP(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .   UVAR(NEL,NUVAR), OFF(NEL),DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,K,IR,IADBUF,NINDX,IFAIL
      INTEGER, DIMENSION(NEL) :: 
     .   INDX
C
      my_real 
     .    SEQ,INV2,ETA,SIGM,XI,EPSF,
     .    DEVXX,DEVYY,DEVZZ,BCOEF,LODE,DAM,
     .    BFRAC,GAMA,CFRAC,DEPS0,
     .    PLAP,F1,F2,F3,AFRAC,NFRAC
      my_real :: DF12, DF23, DF13
C=======================================================================
C     INITIALIZATIONS
C-----------------------------------------------
      AFRAC  = UPARAM(1)
      BFRAC  = UPARAM(2)
      GAMA   = UPARAM(3)
      CFRAC  = UPARAM(4)
      NFRAC  = UPARAM(5)
      DEPS0  = UPARAM(6)
      PLAP   = 0
C-----------------------------------------------
      INDX = 0                                  
      NINDX  = 0  
C-------------------------------------------------------------------
c     Failure strain value - function interpolation
C-------------------------------------------------------------------
       DO I=1,NEL
        
        DAM = UVAR(I,2)
        IF (OFF(I) < 0.1) OFF(I)=ZERO
        IF (OFF(I) < ONE)  OFF(I)=OFF(I)*FOUR_OVER_5

        IF (PLAS(I) > ZERO .and. OFF(I) == ONE) THEN
C---      failure strain interpolation

          PLAP = DPLA(I)/TIMESTEP
          
          SIGM  = (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
          DEVXX = SIGNXX(I) - SIGM
          DEVYY = SIGNYY(I) - SIGM
          DEVZZ = SIGNZZ(I) - SIGM
          SEQ   = SQRT(THREE_HALF*(DEVXX*DEVXX 
     .                       +DEVYY*DEVYY
     .                       +DEVZZ*DEVZZ
     .                + TWO*(SIGNXY(I)*SIGNXY(I)
     .                       +SIGNYZ(I)*SIGNYZ(I)
     .                       +SIGNZX(I)*SIGNZX(I))))

          	  ! determinant du deviateur des contraintes
          INV2 = DEVXX * DEVYY * DEVZZ 
     .         + TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
     .         - DEVXX*SIGNYZ(I)**2 - DEVYY*SIGNZX(I)**2
     .         - DEVZZ*SIGNXY(I)**2

          IF (SEQ == ZERO) THEN
            ETA = THIRD
            XI  = ZERO
          ELSE    
            ETA = SIGM/SEQ
            XI  = ONE/SEQ**3
            XI  = HALF*TWENTY7*INV2*XI
          ENDIF
          
          IF (XI < -ONE) XI =-ONE
          IF (XI >  ONE) XI = ONE

          LODE = ONE - TWO*ACOS(XI)/PI
          
          F1 = TWO_THIRD*COS((ONE   -LODE)*PI/SIX)
          F2 = TWO_THIRD*COS((THREE+LODE)*PI/SIX)
          F3 =-TWO_THIRD*COS((ONE   +LODE)*PI/SIX)
          PLAP  = MAX (PLAP,DEPS0)
          BCOEF = BFRAC*(ONE+GAMA*LOG(PLAP/DEPS0))

          IF (ETA < -THIRD) THEN
            EPSF = EP02
          ELSE
            DF12 = MAX(EM20,F1-F2)
            DF23 = MAX(EM20,F2-F3)
            DF13 = MAX(EM20,F1-F3)
            EPSF = BCOEF*(ONE+CFRAC)**(ONE/NFRAC)
     .           * ((HALF*(DF12**AFRAC  
     .                      +DF23**AFRAC
     .                      +DF13**AFRAC))**(ONE/AFRAC)
     .           +   CFRAC*(TWO*ETA+F1+F3))**(-ONE/NFRAC)

          ENDIF

          DAM = UVAR(I,2) + (PLAS(I) - UVAR(I,1))/EPSF

          UVAR(I,1) = PLAS(I)
          UVAR(I,2) = DAM  
          DFMAX(I)  = MIN(ONE,MAX(DFMAX(I),DAM ))        
c
          IF (DAM >= ONE) THEN
            OFF(I) = FOUR_OVER_5
            NINDX  = NINDX+1
            INDX(NINDX)=I
            IDEL7NOK  = 1  
            TDELE(I) = TIME                
          ENDIF         
        ENDIF
        ! EPSF(I) = YY
c
       ENDDO
c-----------------------------
       IF (NINDX > 0 )THEN
         DO J=1,NINDX
#include "lockon.inc"
           WRITE(IOUT, 1000) NGL(INDX(J))
           WRITE(ISTDO,1100) NGL(INDX(J)),TIME
#include "lockoff.inc"
         ENDDO
      
      ENDIF
c-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',1PE12.4)
c-----------           
      RETURN
      END
