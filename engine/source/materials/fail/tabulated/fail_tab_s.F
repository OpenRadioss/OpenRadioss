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
Chd|  FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_TAB_S (
     1     NEL     ,NUVAR   ,NPF     ,TF      ,TIME    ,
     2     UPARAM  ,NGL     ,ALDT    ,
     4     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     5     DPLA    ,EPSP    ,TSTAR   ,UVAR    ,
     6     OFF     ,TABLE   ,DFMAX   ,TDELE   ,
     7     NFUNC   ,IFUNC )
C---------+---------+---+---+--------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
C---------+---------+---+---+--------------------------------------------
C   /FAIL/TAB - tabulated rupture criteria for solids
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
#include "mvsiz_p.inc"
#include "units_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
#include "impl1_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUVAR
      INTEGER NGL(NEL)
      INTEGER, INTENT(IN) :: NFUNC
      INTEGER, DIMENSION(NFUNC), INTENT(IN) ::  IFUNC
c
      my_real TIME,UPARAM(*),ALDT(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DPLA(NEL),EPSP(NEL),TSTAR(NEL)     
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      TYPE(TTABLE) TABLE(*)
      my_real, DIMENSION(NEL,3) :: XX0
      my_real 
     .   UVAR(NEL,NUVAR), OFF(NEL), DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
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
      INTEGER I,J,IDEL,IDEV,NINDX,
     .  IFUN_EL,IFUN_TEMP,ID_DD ,ITABLE
      INTEGER :: SFLAG
      INTEGER, DIMENSION(MVSIZ) :: INDX
      INTEGER :: NINDX_2
      INTEGER, DIMENSION(MVSIZ) :: INDX_2
      INTEGER, DIMENSION(NEL,3) :: IPOS
      my_real, DIMENSION(MVSIZ) :: EPSF,YY,DYDX,DP
      my_real :: DCRIT,DD,DN,SC_TEMP,SC_EL,EL_REF
      my_real 
     .        P,SIGM,SVM,SXX,SYY,SZZ,DF,FAC,LAMBDA,
     .        XI,THETA, DET,Y1SCALE,
     .        X1SCALE
C=======================================================================
C     INITIALIZATIONS
C-----------------------------------------------
        SFLAG   = INT(UPARAM(1))    
        DCRIT   = UPARAM(4)
        DD      = UPARAM(5)
        DN      = UPARAM(6)
        SC_TEMP = UPARAM(7)
        SC_EL   = UPARAM(8)
        EL_REF  = UPARAM(9)
        Y1SCALE = UPARAM(12) 
        X1SCALE = UPARAM(13)
        ITABLE  = UPARAM(23)
c---
        IDEL = 0
        IDEV = 0
        INDX = 0                                  
        IF (SFLAG == 1) THEN
            IDEL=1
        ELSEIF (SFLAG == 2) THEN
            IDEV =1
        ELSEIF (SFLAG == 3) THEN
            IDEV = 2
        ENDIF
C-------------------------------------------------------------------
c     Failure strain value - function interpolation
C-------------------------------------------------------------------

        IPOS(1:NEL,1:3) = 1
        DO I=1,NEL
C---    failure strain interpolation
            P   = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))     
            SXX = SIGNXX(I) - P                                 
            SYY = SIGNYY(I) - P                                 
            SZZ = SIGNZZ(I) - P                                 
            SVM = HALF*(SXX**2 + SYY**2 + SZZ**2)             
     .          + SIGNXY(I)**2+ SIGNZX(I)**2 + SIGNYZ(I)**2    
            SVM = SQRT(THREE*SVM)                               
            SIGM = P / MAX(EM20,SVM)                            
C----
            DET = SXX*SYY*SZZ + TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)- 
     .            SXX*SIGNYZ(I)**2-SZZ*SIGNXY(I)**2-SYY*SIGNZX(I)**2

            XI = ONE/MAX(EM20,SVM**3)
            XI = HALF*TWENTY7*DET*XI         
            IF(XI < -ONE) XI = -ONE
            IF(XI > ONE) XI = ONE
            THETA = ONE - TWO*ACOS(XI)/PI
            XX0(I,1)=SIGM 
            XX0(I,2)=EPSP(I) *X1SCALE
            XX0(I,3)=THETA
        ENDDO

        CALL TABLE_VINTERP(TABLE(ITABLE),NEL,NEL,IPOS,XX0,YY,DYDX)
        EPSF(1:NEL) = YY(1:NEL)*Y1SCALE


c---- Scale functions       
        DO I=1,NEL
            IFUN_EL   = IFUNC(1)
            IFUN_TEMP = IFUNC(2)
c----   element length scale function
            IF (IFUN_EL > 0) THEN
                LAMBDA = ALDT(I) / EL_REF                     
                FAC = SC_EL*FINTER(IFUN_EL,LAMBDA,NPF,TF,DF)  
                EPSF(I) = EPSF(I)* FAC                           
            ENDIF                                                              
c----   temperature scale function
            IF (IFUN_TEMP > 0) THEN
                FAC = SC_TEMP*FINTER(IFUN_TEMP,TSTAR(I),NPF,TF,DF) 
                EPSF(I) = EPSF(I)* FAC 
            ENDIF                                                              
        ENDDO
c-------------------------------------------------------------------
c---- element will be deleted 
        IF (IDEL == 1) THEN
            DO I=1,NEL
                IF (OFF(I) < 0.1) OFF(I)=ZERO
                IF (OFF(I) < ONE)  OFF(I)=OFF(I)*FOUR_OVER_5
            ENDDO
        ENDIF
C      
        IF (IDEL == 1)THEN
            NINDX = 0  
            NINDX_2 = 0  
            ID_DD = IFUNC(3)
            ! --------------
            ! need to split the loop to vectorize the ID_DD==0 loop
            IF(ID_DD /= 0 )THEN
                DO I=1,NEL
                    IF (SFLAG==1 .AND. OFF(I)==ONE) THEN
                        NINDX_2 = NINDX_2 + 1
                        INDX_2(NINDX_2) = I
                        DP(I) = FINTER(ID_DD,UVAR(I,1),NPF,TF,DF)
                    ENDIF
                ENDDO
            ELSE
                DO I=1,NEL
                    IF (SFLAG==1 .AND. OFF(I)==ONE) THEN
                        NINDX_2 = NINDX_2 + 1
                        INDX_2(NINDX_2) = I
                        DP(I) = DN*DD**(ONE-ONE/DN)
                    ENDIF
                ENDDO
            ENDIF
            ! --------------
#include "vectorize.inc"
            DO J=1,NINDX_2
                I = INDX_2(J)
                IF (EPSF(I) > ZERO) UVAR(I,1)=UVAR(I,1)+DP(I)*DPLA(I)/EPSF(I)      
                IF (UVAR(I,1) >= DCRIT) THEN
                    OFF(I)=FOUR_OVER_5
                    NINDX=NINDX+1
                    INDX(NINDX)=I
                    IDEL7NOK = 1 
                    TDELE(I) = TIME                                  
                ENDIF 
            ENDDO
            IF (NINDX > 0 .AND. IMCONV == 1)THEN
                DO J=1,NINDX
#include "lockon.inc"
                    WRITE(IOUT, 1000) NGL(INDX(J))
                    WRITE(ISTDO,1100) NGL(INDX(J)),TIME
#include "lockoff.inc"
                ENDDO
            ENDIF         
        ENDIF
C---- 
        IF (IDEV > 0) THEN ! element deleted when rupture in all integration points

            ! --------------
            NINDX  = 0 
            NINDX_2 = 0
            DO I=1,NEL      
                IF (OFF(I) == ONE .AND. (SFLAG==2 .OR. SFLAG==3)) THEN
                    NINDX_2 = NINDX_2 + 1
                    INDX_2(NINDX_2) = I
                    DP(I) = ZERO
                    IF (UVAR(I,1) < DCRIT)THEN
                        ID_DD = IFUNC(3)
                        IF(ID_DD /= 0 )THEN
                            DP(I) = FINTER(ID_DD,UVAR(I,1),NPF,TF,DF)
                        ELSE
                            DP(I) = DN*DD**(ONE-ONE/DN)
                        ENDIF
                    ENDIF
                ENDIF            
            ENDDO
            ! --------------
#include "vectorize.inc"
            DO J=1,NINDX_2
                I = INDX_2(J)    
                IF (UVAR(I,1) < DCRIT)THEN
                    IF (EPSF(I) > ZERO) UVAR(I,1)=UVAR(I,1)+DP(I)*DPLA(I)/EPSF(I)      
                    IF (UVAR(I,1) >= DCRIT) THEN
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
                        IF (SFLAG == 2) THEN
                            SIGNXX(I) = P
                            SIGNYY(I) = P
                            SIGNZZ(I) = P
                        ENDIF
                    ENDIF
                ELSEIF (SFLAG == 2) THEN   !  UVAR > DCRIT     
                    P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
                    SIGNXX(I) = P
                    SIGNYY(I) = P
                    SIGNZZ(I) = P
                ENDIF
            ENDDO
            ! --------------
            IF (NINDX > 0.AND.IMCONV == 1) THEN
                DO J=1,NINDX
                    I = INDX(J)
#include "lockon.inc"
                    WRITE(IOUT, 2000) NGL(I)
                    WRITE(ISTDO,2100) NGL(I),TIME
#include "lockoff.inc"
                END DO
            ENDIF           
            ! --------------
        ENDIF   
C-------------Maximum Damage storing for output : 0 < DFMAX < 1--------------    
        DO I=1,NEL
            DFMAX(I)= MIN(ONE,MAX(DFMAX(I),UVAR(I,1)/DCRIT))
        ENDDO
C-----------------------------------------------
 1000   FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100   FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',1PE12.4)
C     
 2000   FORMAT(1X,' DEVIATORIC STRESS SET TO ZERO',I10)
 2100   FORMAT(1X,' DEVIATORIC STRESS SET TO ZERO',I10,
     .          ' AT TIME :',1PE12.4)
c-----------           
        RETURN
        END
