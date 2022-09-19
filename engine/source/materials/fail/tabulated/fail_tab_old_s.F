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
Chd|  FAIl_TAB_OLD_S                source/materials/fail/tabulated/fail_tab_old_s.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIl_TAB_OLD_S (
     1     NEL     ,NUVAR   ,NPF     ,TF      ,TIME    ,
     2     UPARAM  ,NGL     ,IPM     ,MAT     ,ALDT    ,
     4     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     5     PLAS    ,DPLA    ,EPSP    ,TSTAR   ,UVAR    ,
     6     OFF     ,IP      ,DFMAX   ,TDELE   ,
     7     NFUNC   ,IFUNC )
C---------+---------+---+---+--------------------------------------------
C   /FAIL/TAB - tabulated rupture criteria for solids (old, obsolete version)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
#include "scr19_c.inc"
#include "impl1_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUVAR,IP
      INTEGER NGL(NEL),IPM(NPROPMI,*),MAT(NEL)
      INTEGER, INTENT(IN) :: NFUNC
      INTEGER, DIMENSION(NFUNC), INTENT(IN) ::  IFUNC
c
      my_real TIME,TIMESTEP,UPARAM(*),ALDT(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   PLAS(NEL),DPLA(NEL),EPSP(NEL),TSTAR(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real :: UVAR(NEL,NUVAR), OFF(NEL), DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,J1,J2,IDEL,IDEV,IADBUF,NINDX,NRATE,IFUN_EL,IFUN_TEMP,SFLAG
      INTEGER, DIMENSION(NEL) ::  INDX
C
      my_real :: DCRIT,DD,DN,SC_TEMP,SC_EL,EL_REF,DP,P,SIGM,SVM,
     .   SXX,SYY,SZZ,EF1,EF2,DF,FAC,LAMBDA,ETA,YY
      my_real, DIMENSION(NEL)   :: EPSF
      my_real, DIMENSION(NFUNC) :: YFAC,RATE
C=======================================================================
C     INITIALIZATIONS
C-----------------------------------------------
      SFLAG  = INT(UPARAM(1))
      DCRIT  = UPARAM(4)
      DD     = UPARAM(5)
      DN     = UPARAM(6)
      SC_TEMP= UPARAM(7)
      SC_EL  = UPARAM(8)
      EL_REF = UPARAM(9)
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
      NRATE = NFUNC - 2
      YFAC(1:NRATE) = UPARAM(11+1    :11+NRATE)
      RATE(1:NRATE) = UPARAM(11+NRATE:11+NRATE*2)
C-------------------------------------------------------------------
c     Failure strain value - function interpolation
C-------------------------------------------------------------------

      DO I=1,NEL
C---    failure strain interpolation
        P   = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
        SXX = SIGNXX(I) - P
        SYY = SIGNYY(I) - P
        SZZ = SIGNZZ(I) - P
        SVM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .      + SIGNXY(I)**2+ SIGNZX(I)**2 + SIGNYZ(I)**2
        SVM = SQRT(THREE*SVM)
        SIGM = P / MAX(EM20,SVM)
c----
        J1 = 1
        IF (NRATE > 1) THEN
          J2  = J1+1
          EF1 = YFAC(J1)*FINTER(IFUNC(J1),SIGM,NPF,TF,DF)
          EF2 = YFAC(J2)*FINTER(IFUNC(J2),SIGM,NPF,TF,DF)
          FAC = (EPSP(I) - RATE(J1)) / (RATE(J2) - RATE(J1))
          EPSF(I) = MAX(EF1 + FAC*(EF2 - EF1), EM20)
        ELSE
          EPSF(I) = YFAC(J1)*FINTER(IFUNC(J1),SIGM,NPF,TF,DF)
        ENDIF
      ENDDO
c
c---- Scale functions
      DO I=1,NEL
        IFUN_EL   = IFUNC(2)
        IFUN_TEMP = IFUNC(3)
c----   element lenght scale function
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
        NINDX  = 0
        DO I=1,NEL
          IF (SFLAG==1 .AND. OFF(I)==ONE) THEN
            DP = DN*DD**(ONE-ONE/DN)
            IF (EPSF(I) > ZERO) UVAR(I,1)=UVAR(I,1) + DP*DPLA(I)/EPSF(I)
            IF (UVAR(I,1) >= DCRIT) THEN
              OFF(I)=FOUR_OVER_5
              NINDX=NINDX+1
              INDX(NINDX)=I
              IDEL7NOK = 1
              TDELE(I) = TIME
            ENDIF
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
        NINDX  = 0
        DO I=1,NEL
c---
          IF (OFF(I) == ONE .AND. (SFLAG==2 .OR. SFLAG==3)) THEN
            IF (UVAR(I,1) < DCRIT)THEN
              DP = DN*DD**(ONE-ONE/DN)
              IF (EPSF(I) > ZERO) UVAR(I,1)=UVAR(I,1)+DP*DPLA(I)/EPSF(I)
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
          ENDIF
c---
        ENDDO
        IF (NINDX > 0.AND.IMCONV == 1) THEN
          DO J=1,NINDX
            I = INDX(J)
#include "lockon.inc"
            WRITE(IOUT, 2000) NGL(I)
            WRITE(ISTDO,2100) NGL(I),TIME
#include "lockoff.inc"
          END DO
        ENDIF
      ENDIF
C-------------Maximum Damage storing for output : 0 < DFMAX < 1--------------    
       DO I=1,NEL
          DFMAX(I)= MIN(ONE,MAX(DFMAX(I),UVAR(I,1)/DCRIT))
       ENDDO
C-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',1PE12.4)
C     
 2000 FORMAT(1X,' DEVIATORIC STRESS SET TO ZERO',I10)
 2100 FORMAT(1X,' DEVIATORIC STRESS SET TO ZERO',I10,
     .          ' AT TIME :',1PE12.4)
c-----------
      RETURN
      END
