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
C new (energy  failure model)
Chd|====================================================================
Chd|  FAIL_ENERGY_S                 source/materials/fail/energy/fail_energy_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_ENERGY_S(
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2     NPF    ,TF     ,TIME   ,TIMESTEP ,UPARAM  ,
     3     NGL    , IPM    ,MAT,
     4     SIGNXX ,SIGNYY  ,SIGNZZ ,SIGNXY  ,SIGNYZ ,SIGNZX,
     5     VOLUME ,EINT    ,EPSP  , UVAR    ,OFF    ,IP    ,
     6     DFMAX  ,TDELE   )
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
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
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
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL),IPM(NPROPMI,*),
     .        MAT(NEL)
      my_real TIME,TIMESTEP,UPARAM(*),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   EPSP(NEL),VOLUME(NEL),EINT(NEL)     
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
 
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL), DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC),IP
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
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
      INTEGER I,J,IDEL,IDEV,IFLAG(MVSIZ),INDX(MVSIZ),IADBUF,NINDX,
     .        NINDEX,INDEX(MVSIZ),IR,IFAIL,JJ,
     .        ISOLID
      my_real 
     .       E1,E2,E3,E4,E5,E6,RFAC,E42,E52,E62,C,D, EPST,EPST2,
     .       R1,R2,Y,YP,DAV,DYDX,IE_SP,P,SCALE,CC
      my_real
     .      EPS_MAX(MVSIZ),DAMAGE(MVSIZ),EPSF1(MVSIZ),EPSF2(MVSIZ),
     .      RIEF1,RIEF2,XFAC
C--------------------------------------------------------------
      RIEF1 = UPARAM(1)
      RIEF2 = UPARAM(2)
      XFAC  = UPARAM(4)
      !ISOLID = INT(UPARAM(4)) 
      DO I=1,NEL
        DAMAGE(I) = ZERO
      ENDDO
C-----------------------------------------------
      DO I=1,NEL
        IF(OFF(I).LT.EM01) OFF(I)=ZERO
        IF(OFF(I).LT.ONE) OFF(I)=OFF(I)*FOUR_OVER_5
      END DO
CC           
       NINDX=0  
       DO I=1,NEL
        RFAC = ONE
        IF(IFUNC(1).NE.0)THEN
          RFAC=FINTER(IFUNC(1),EPSP(I)*XFAC,NPF,TF,DYDX)
          RFAC = MAX(RFAC,EM20)
        ENDIF  
        IF(OFF(I).EQ.ONE)THEN
          IE_SP = EINT(I)/VOLUME(I)
          
          R1 = RIEF1*RFAC
          R2 = RIEF2*RFAC
         IF(IE_SP.GT.R1.AND.R1.LT.R2) THEN
          DAMAGE(I)= (IE_SP-R1)/(R2-R1)
          DAMAGE(I)= MIN(ONE,DAMAGE(I))
         ENDIF
         IF(IE_SP.GT.R2) THEN
          DAMAGE(I)= ONE
          OFF(I)=FOUR_OVER_5
          NINDX=NINDX+1
          INDX(NINDX)=I
          IDEL7NOK = 1
          TDELE(I) = TIME  
         ENDIF                  
       ENDIF 
      ENDDO
c
      IF(NINDX.GT.0.AND.IMCONV.EQ.1)THEN
       DO J=1,NINDX
#include "lockon.inc"
        WRITE(IOUT, 1000) NGL(INDX(J))
        WRITE(ISTDO,1100) NGL(INDX(J)),TIME
#include "lockoff.inc"
        END DO
      END IF         
      DO I=1,NEL
        R1 = RIEF1
        R2 = RIEF2
        IF(R1 < R2)THEN         
          SIGNXX(I) = (ONE - DAMAGE(I))*SIGNXX(I)
          SIGNYY(I) = (ONE - DAMAGE(I))*SIGNYY(I)
          SIGNZZ(I) = (ONE - DAMAGE(I))*SIGNZZ(I)
          SIGNXY(I) = (ONE - DAMAGE(I))*SIGNXY(I)
          SIGNYZ(I) = (ONE - DAMAGE(I))*SIGNYZ(I)
          SIGNZX(I) = (ONE - DAMAGE(I))*SIGNZX(I)
        END IF
C-------------Maximum Damage storing for output : 0 < DFMAX < 1--------------
        DFMAX(I) = MAX(DFMAX(I),DAMAGE(I))
       ENDDO            
C-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',1PE20.13)
CC     
      RETURN
      END
