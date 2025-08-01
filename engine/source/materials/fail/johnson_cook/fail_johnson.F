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
!||    fail_johnson    ../engine/source/materials/fail/johnson_cook/fail_johnson.F
!||--- called by ------------------------------------------------------
!||    mmain           ../engine/source/materials/mat_share/mmain.F90
!||    mmain8          ../engine/source/materials/mat_share/mmain8.F
!||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8          ../engine/source/materials/mat_share/mulaw8.F90
!||    usermat_solid   ../engine/source/materials/mat_share/usermat_solid.F
!||====================================================================
      SUBROUTINE FAIL_JOHNSON(
     1     NEL    ,NUPARAM,NUVAR   ,
     2     TIME   ,TIMESTEP ,UPARAM  ,NGL    ,
     4     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     5     DPLA   ,EPSP   ,TSTAR   ,UVAR    ,OFF     ,
     6     DFMAX  ,TDELE  )
c-----------------------------------------------
c    Johnson cook failure model
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
#include  "scr17_c.inc"
#include  "units_c.inc"
#include  "comlock.inc"
#include  "param_c.inc"
#include  "impl1_c.inc"
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL)
      my_real TIME,TIMESTEP,UPARAM(NUPARAM),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),UVAR(NEL,NUVAR),
     .   DPLA(NEL),EPSP(NEL),TSTAR(NEL),OFF(NEL),DFMAX(NEL),TDELE(NEL)    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,IDEL,IDEV,IFLAG,NINDX,IR,IFAIL,ISOLID
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real :: D1,D2,D3,D4,D5,EPSP0,P,EPSF,SVM,SCALE,SXX,SYY,SZZ,EPSF_MIN
C--------------------------------------------------------------
      D1 = UPARAM(1)
      D2 = UPARAM(2)
      D3 = UPARAM(3)
      D4 = UPARAM(4)
      D5 = UPARAM(5)
      EPSP0 = UPARAM(6)
      ISOLID = INT(UPARAM(8)) 
      EPSF_MIN = UPARAM(12)

C-----------------------------------------------
      IDEL=0
      IDEV=0
      SCALE = ZERO
      IF ((ISOLID==1).OR.(ISOLID == 4)) THEN
        IDEL=1
      ELSEIF ((ISOLID==2).OR.(ISOLID == 3)) THEN
        IDEV =1
      ENDIF
C...
      IF(IDEL==1)THEN
        DO I=1,NEL
          IF(OFF(I)<0.1) OFF(I)=0.0
          IF(OFF(I)<1.0) OFF(I)=OFF(I)*0.8
        END DO
      END IF
C      
      IF(IDEL==1)THEN
       NINDX=0  
       DO I=1,NEL
        IF ((ISOLID==1.AND.OFF(I)==ONE).OR.(ISOLID==4))THEN
         IF(DPLA(I)/=ZERO)THEN
           P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
           SXX = SIGNXX(I) - P
           SYY = SIGNYY(I) - P
           SZZ = SIGNZZ(I) - P
           SVM =HALF*(SXX**2 + SYY**2 + SZZ**2)
     .            +SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
           SVM=SQRT(THREE*SVM)
           EPSF = D3*P/MAX(EM20,SVM)
           EPSF = D1 + D2*EXP(EPSF)
           IF(D4/=ZERO) EPSF = EPSF * (ONE + D4*LOG(MAX(ONE,EPSP(I)/EPSP0))) ! if d4=0, epsp is not correctly defined
           IF(D5/=ZERO) EPSF = EPSF * (ONE + D5*TSTAR(I)) ! if d5=0, tsart is not correctly defined
           EPSF = MAX(EPSF,EPSF_MIN)
           IF(EPSF>ZERO) DFMAX(I) = DFMAX(I) + DPLA(I)/EPSF
           DFMAX(I) = MIN(ONE,DFMAX(I))
         ENDIF
         IF (DFMAX(I)>=ONE.AND.OFF(I)==ONE) THEN
          IF (ISOLID == 1) THEN 
            OFF(I)=FOUR_OVER_5
            NINDX=NINDX+1
            INDX(NINDX)=I
            TDELE(I) = TIME 
          ELSEIF (ISOLID == 4) THEN
            DFMAX(I)  = ONE
            OFF(I)    = ONE 
            SIGNXX(I) = ZERO
            SIGNYY(I) = ZERO
            SIGNZZ(I) = ZERO
            SIGNXY(I) = ZERO
            SIGNYZ(I) = ZERO
            SIGNZX(I) = ZERO 
          ENDIF
         ENDIF
        ENDIF 
       ENDDO
       IF(NINDX>0.AND.IMCONV==1)THEN
        DO J=1,NINDX
#include "lockon.inc"
         WRITE(IOUT, 1000) NGL(INDX(J))
         WRITE(ISTDO,1100) NGL(INDX(J)),TIME
#include "lockoff.inc"
        END DO
       END IF         
      ENDIF
Cc deviatoric will be vanished      
      IF(IDEV==1)THEN
       NINDX=0 
       DO I=1,NEL 
        IF((ISOLID==2.OR.ISOLID==3).AND.OFF(I)==ONE)THEN 
         IF(DFMAX(I)<ONE.AND.DPLA(I)/=ZERO)THEN
          P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
          SXX = SIGNXX(I) - P
          SYY = SIGNYY(I) - P
          SZZ = SIGNZZ(I) - P
          SVM =HALF*(SXX**2+ SYY**2 + SZZ**2)
     .          +SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
          SVM=SQRT(THREE*SVM)
          EPSF = D3*P/MAX(EM20,SVM)
          EPSF = (D1 + 
     .           D2*EXP(EPSF))*(ONE 
     .                  + D4*LOG(MAX(ONE,EPSP(I)/EPSP0)))
     .                    *(ONE + D5*TSTAR(I))
          EPSF = MAX(EPSF,EPSF_MIN)
          IF(EPSF>ZERO) DFMAX(I) = DFMAX(I) + DPLA(I)/EPSF
          DFMAX(I) = MIN(ONE,DFMAX(I))
          IF(DFMAX(I)>=ONE.AND.OFF(I)==ONE) THEN
           NINDX=NINDX+1
           INDX(NINDX)=I
           DFMAX(I) = ONE
           IF (ISOLID == 2) THEN 
            SIGNXX(I) =   P
            SIGNYY(I) =   P
            SIGNZZ(I) =   P
            SIGNXY(I) = ZERO
            SIGNYZ(I) = ZERO
            SIGNZX(I) = ZERO       
           ELSEIF (ISOLID == 3) THEN
            SIGNXX(I) = MIN(P,ZERO)
            SIGNYY(I) = MIN(P,ZERO)
            SIGNZZ(I) = MIN(P,ZERO)
            SIGNXY(I) = ZERO
            SIGNYZ(I) = ZERO
            SIGNZX(I) = ZERO             
           ENDIF
          ENDIF
c       
         ELSEIF(DFMAX(I)>=ONE)THEN
          P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
          DFMAX(I) = ONE
          IF (ISOLID == 2) THEN
            SIGNXX(I) =   P
            SIGNYY(I) =   P
            SIGNZZ(I) =   P
            SIGNXY(I) = ZERO
            SIGNYZ(I) = ZERO
            SIGNZX(I) = ZERO
          ELSEIF (ISOLID == 3) THEN
            SIGNXX(I) = MIN(P,ZERO)
            SIGNYY(I) = MIN(P,ZERO)
            SIGNZZ(I) = MIN(P,ZERO)
            SIGNXY(I) = ZERO
            SIGNYZ(I) = ZERO
            SIGNZX(I) = ZERO
          ENDIF
         ENDIF
        ENDIF  
       ENDDO
       IF(NINDX>0.AND.IMCONV==1)THEN
        DO J=1,NINDX
         I = INDX(J)
#include "lockon.inc"
         WRITE(IOUT, 2000) NGL(I)
         WRITE(ISTDO,2100) NGL(I),TIME
#include "lockoff.inc"
        END DO
       END IF           
      ENDIF   
C---------Damage for output  0 < DFMAX < 1 --------------------
c       DO J=1,IR
c          I=JST(J)
c          DFMAX(I)= MIN(ONE,DFMAX(I))
c       ENDDO
C------------------
C-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',1PE12.4)
C     
 2000 FORMAT(1X,' DEVIATORIC STRESS WILL BE VANISHED',I10)
 2100 FORMAT(1X,' DEVIATORIC STRESS WILL BE VANISHED',I10,
     .          ' AT TIME :',1PE12.4)
      RETURN
      END
