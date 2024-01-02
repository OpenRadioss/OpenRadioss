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
Chd|  FAIL_TENSSTRAIN_C             source/materials/fail/tensstrain/fail_tensstrain_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_TENSSTRAIN_C(
     1     NEL      ,NFUNC    ,NUPARAM  ,NUVAR    ,IFUNC    ,
     2     UPARAM   ,UVAR     ,NPF      ,TF       ,TIME     ,
     3     NGL      ,IPG      ,ILAY     ,IPT      ,EPSP     ,
     4     EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     5     OFF      ,FOFF     ,PTHKF    ,DFMAX    ,TDEL     ,
     6     DMG_FLAG ,DMG_SCALE,ALDT     ,TSTAR    ,ISMSTR   )
C-----------------------------------------------
C    tension strain failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C EPSP    | NEL     | F | R | EQUIVALENT STRAIN RATE
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C PTHKF   |  1      | F | W | PERCENTAGE OF LAYER THICKNESS TO FAIL
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C DMG_FLAG|  1      | I | W | STRESS REDUCTION FLAG DUE TO DAMAGE
C DMG_SCALE| NEL    | F | W | STRESS REDUCTION FACTOR
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT,ISMSTR
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: OFF,EPSP,ALDT,
     .   EPSXX,EPSYY,EPSXY,EPSYZ,EPSZX,TSTAR
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) :: DMG_FLAG
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,INTENT(OUT) :: PTHKF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX
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
      INTEGER I,J,NINDXD,NINDXF,NINDX1,NINDX2,S_FLAG,SCALE_FLAG,STRDEF,STRFLAG
      INTEGER ,DIMENSION(NEL) :: INDXD,INDXF,INDX1,INDX2
      my_real R1,R2,DYDX,EPSP1,EPSP2,EPSF1,EPSF2,E22,LAMBDA,FAC,DF,
     .        EL_REF,SC_EL,SCALE_TEMP,RFAC,RFAC2,EPST_A,EPST_B,
     .        UNIT_T,EPSP_UNIT
      my_real ,DIMENSION(NEL) :: EPS_MAX,EPST1,EPST2,DAMAGE,EPSRATE
C=======================================================================
      DMG_FLAG     = 1
      S_FLAG       = 1
      SCALE_FLAG   = 1
      PTHKF        = EM06      ! % of thickness to fail <=> 1 integration point
      EPSF1        = UPARAM(1)
      EPSF2        = UPARAM(2)
      EPSP1        = UPARAM(3)
      EPSP2        = UPARAM(4)
      SCALE_TEMP   = ONE

      DAMAGE(1:NEL)=ZERO
C-----------------------------------------------
C!  Initialization of initial el.length
C-----------------------------------------------
      SC_EL      = UPARAM(5)
      EL_REF     = UPARAM(6)
      SCALE_TEMP = UPARAM(7)
      S_FLAG     = INT(UPARAM(8))
      UNIT_T     = UPARAM(9)
      STRDEF= INT(UPARAM(10))
      STRFLAG    = 0
c
      IF (UVAR(1,1) == ZERO) THEN
        DO I=1,NEL
          UVAR(I,1) = ALDT(I) 
          SELECT CASE (S_FLAG)
            CASE (2)
              IF (IFUNC(2) /= 0)THEN
                LAMBDA = UVAR(I,1) / EL_REF
                FAC = SC_EL * FINTER(IFUNC(2),LAMBDA,NPF,TF,DF) 
                UVAR(I,1) = FAC
              ELSE
                UVAR(I,1) = ONE
              ENDIF
            CASE (3)
              IF (IFUNC(2) /= 0)THEN
                LAMBDA = UVAR(I,1) / EL_REF
                FAC = SC_EL * FINTER(IFUNC(2),LAMBDA,NPF,TF,DF) 
                UVAR(I,1) = FAC
              ELSE
                UVAR(I,1) = ONE
              ENDIF
            CASE DEFAULT    ! old formulation, backward compatibility only
              UVAR(I,1) = EPSP1
          END SELECT
        ENDDO
      ENDIF
c
      IF (STRDEF == 2) THEN        ! failure defined as engineering strain
        IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 1              ! Cauchy-Green to engineering
        ELSE IF (ISMSTR == 0 .or. ISMSTR == 2 .or. ISMSTR == 4) THEN
          STRFLAG = 2              ! true strain to engineering
        END IF
      ELSE IF (STRDEF == 3) THEN   ! failure defined as true strain
        IF (ISMSTR == 1 .or. ISMSTR == 3 .or. ISMSTR == 11) THEN
          STRFLAG = 3              ! engineering to true strain
        ELSE IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 4              ! Cauchy-Green to true strain
        END IF
      END IF           
c------------------------------------
c     Equivalent or principal strain
c------------------------------------
      SELECT CASE (S_FLAG)
c
        CASE (1,2)      ! equivalent strain criterion
          IF (STRFLAG == 1 .or. STRFLAG == 4) THEN
            DO I=1,NEL          !COMPUTE THE 2 PRINCIPAL VALUES
              EPST_A = HALF*(EPSXX(I)+EPSYY(I))
              EPST_B = SQRT( (HALF*(EPSXX(I)-EPSYY(I)))**2 + (HALF*EPSXY(I))**2)
              EPST1(I)  = EPST_A + EPST_B
              EPST2(I)  = EPST_A - EPST_B
            ENDDO
          ELSE
            DO I=1,NEL
              EPS_MAX(I) = HALF*(EPSXX(I)+EPSYY(I)
     .                + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                + EPSXY(I)*EPSXY(I) ) )
            ENDDO
          END IF
c
        CASE (3)      ! max tensile principal strain criterion
          DO I=1,NEL          
            EPST_A = HALF*(EPSXX(I)+EPSYY(I))
            EPST_B = SQRT((HALF*(EPSXX(I)-EPSYY(I)))**2 + (HALF*EPSXY(I))**2)
            EPST1(I)   = EPST_A + EPST_B
            EPST2(I)   = EPST_A - EPST_B
            EPS_MAX(I) = EPST1(I)
          ENDDO
      END SELECT
c----------------------------------------------
c     Max strain transformation following input definition
c--------------------------
      SELECT CASE (STRFLAG)
        CASE (1)  !  transform Cauchy-Green to engineering
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPST1(I) = SQRT(EPST1(I) + ONE) - ONE
              EPST2(I) = SQRT(EPST2(I) + ONE) - ONE
              EPS_MAX(I) = SQRT(EPST1(I)**2 + EPST2(I)**2)
              EPSRATE(I) = SQRT(EPSP(I) + ONE) - ONE
            END IF
          ENDDO
c
        CASE (2)  !  transform true strain to engineering
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS_MAX(I) = EXP(EPS_MAX(I)) - ONE
              EPSRATE(I) = EXP(EPSP(I)) - ONE
            END IF
          ENDDO
c
        CASE (3)  !  transform engineering to true strain
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS_MAX(I) = LOG(EPS_MAX(I) + ONE)
              EPSRATE(I) = LOG(EPSP(I) + ONE)
            END IF
          ENDDO
c
        CASE (4)  !  transform Cauchy-Green to true strain
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPST1(I) = LOG(SQRT(EPST1(I)+ONE))
              EPST2(I) = LOG(SQRT(EPST2(I)+ONE))
              EPS_MAX(I) = SQRT(EPST1(I)**2 + EPST2(I)**2)
              EPSRATE(I) = LOG(SQRT(EPSP(I)+ONE))
            END IF
          ENDDO
c
        CASE DEFAULT
           ! no transformation : failure strain measure is defined by Ismstr
           EPSRATE(1:NEL) = EPSP(1:NEL)         
      END SELECT
c----------------------------------------------
c     Calculate failure criterion
c-------------------
c
      NINDXD = 0  
      NINDXF = 0  
      NINDX1 = 0  
      NINDX2 = 0  
c
      SELECT CASE (S_FLAG)
        CASE (2)
C-------------------
c     Equivalent strain. New format: Element length and temperature
C-------------------
          DO I=1,NEL
            IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
              IF (IFUNC(1) > 0) THEN   ! strain rate
                EPSP_UNIT = EPSRATE(I) * UNIT_T
                RFAC = FINTER(IFUNC(1),EPSP_UNIT,NPF,TF,DYDX)
                RFAC = MAX(RFAC,EM20)
              ELSE
                RFAC = ONE
              ENDIF  
              IF (IFUNC(3) > 0) THEN   ! temperatuer
                RFAC2 = FINTER(IFUNC(3),TSTAR(I),NPF,TF,DYDX)
                RFAC2 = MAX(RFAC2,EM20)
              ELSE
                RFAC2 = ONE
              ENDIF  
              R1 = EPSF1*RFAC*RFAC2*UVAR(I,1)
              R2 = EPSF2*RFAC*RFAC2*UVAR(I,1)  
              IF (EPS_MAX(I) > R1) THEN
                IF (DFMAX(I) == ZERO) THEN
                  NINDXD = NINDXD + 1  
                  INDXD(NINDXD) = I
                ENDIF   
                DAMAGE(I) = MIN(ONE, (EPS_MAX(I)-R1)/(R2-R1))
              ENDIF
              IF (EPS_MAX(I) >= R2) THEN
                FOFF(I) = 0
                TDEL(I) = TIME 
                NINDXF  = NINDXF + 1  
                INDXF(NINDXF) = I
              ENDIF 
            ENDIF          
          ENDDO

        CASE (3)
C-------------------
c     Principal strain.  New format: No failure in compression
C-------------------
          DO I=1,NEL
            IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
              IF (IFUNC(1) > 0) THEN   ! strain rate
                EPSP_UNIT = EPSRATE(I) * UNIT_T
                RFAC = FINTER(IFUNC(1),EPSP_UNIT,NPF,TF,DYDX)
                RFAC = MAX(RFAC,EM20)
              ELSE
                RFAC = ONE
              ENDIF  
              IF (IFUNC(3) > 0) THEN   ! temperatuer
                RFAC2 = FINTER(IFUNC(3),TSTAR(I),NPF,TF,DYDX)
                RFAC2 = MAX(RFAC2,EM20)
              ELSE
                RFAC2 = ONE
              ENDIF  
              R1 = EPSF1*RFAC*RFAC2*UVAR(I,1)
              R2 = EPSF2*RFAC*RFAC2*UVAR(I,1)  
              IF ( EPS_MAX(I) > R1) THEN
                IF (DFMAX(I) == ZERO) THEN
                  NINDXD = NINDXD + 1  
                  INDXD(NINDXD) = I
                ENDIF   
                DAMAGE(I) = MIN(ONE, (EPS_MAX(I)-R1)/(R2-R1))
              ENDIF
              IF ( EPS_MAX(I) >= R2) THEN
                FOFF(I) = 0
                TDEL(I) = TIME 
                NINDXF  = NINDXF + 1  
                INDXF(NINDXF) = I
              ENDIF 
            ENDIF          
          ENDDO
          
C-------------------
c     Equivalent strain.  No change for downward compatibility.
C-------------------
        CASE DEFAULT  

          DO I=1,NEL
            IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
              IF (IFUNC(1) > 0) THEN
                EPSP_UNIT = EPSRATE(I) * UNIT_T
                RFAC = FINTER(IFUNC(1),EPSP_UNIT,NPF,TF,DYDX)
                RFAC = MAX(RFAC,EM20)
              ELSE
                RFAC = ONE
              ENDIF  
              R1 = EPSF1*RFAC
              R2 = EPSF2*RFAC      
              IF (EPS_MAX(I) > R1) THEN
                IF (DFMAX(I) == ZERO) THEN
                  NINDXD = NINDXD + 1  
                  INDXD(NINDXD) = I
                ENDIF   
                DAMAGE(I) = MIN(ONE, (EPS_MAX(I)-R1)/(R2-R1))
              ENDIF
              IF (EPS_MAX(I) >= R2) THEN
                FOFF(I) = 0
                TDEL(I) = TIME 
                NINDXF  = NINDXF + 1  
                INDXF(NINDXF) = I
              ENDIF 
c         
              E22 = ZERO
              IF (EPSP1 > ZERO .OR. EPSP2 > ZERO) THEN
                IF (EPSP2 > ZERO) THEN
                  E22 = HALF*( EPSXX(I)+EPSYY(I)
     .                - SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                + EPSXY(I)*EPSXY(I) ) ) !PRINCIPAL STRAIN MIN
                  IF (STRFLAG == 1) THEN
                    E22 = SQRT(E22 + ONE) - ONE
                  ELSE IF (STRFLAG == 2) THEN
                    E22 = EXP(E22) - ONE
                  ELSE IF (STRFLAG == 3) THEN
                    E22 = LOG(E22 + ONE)
                  ELSE IF (STRFLAG == 4) THEN
                    E22 = LOG(SQRT(E22+ONE))
                  END IF
                ENDIF
                IF (EPS_MAX(I) > UVAR(I,1) .AND. UVAR(I,1) /= ZERO) THEN
                  FOFF(I) = 0
                  TDEL(I) = TIME 
                  NINDX1  = NINDX1 + 1  
                  INDX1(NINDX1) = I
                ELSEIF (E22 > EPSP2 .AND. EPSP2 /= ZERO) THEN
                  FOFF(I) = 0
                  TDEL(I) = TIME 
                  NINDX2  = NINDX2 + 1  
                  INDX2(NINDX2) = I
                  EPS_MAX(I) = E22
                ENDIF
                DFMAX(I)  = MAX(DFMAX(I),MIN( ONE, (EPS_MAX(I) / UVAR(I,1))))
                SCALE_FLAG = 0
              ENDIF
c              
            ENDIF          
          ENDDO
      END SELECT
c----------------------------------------------------------------------- 
c---  Maximum Damage for output : 0 < DFMAX < 1
      DO I=1,NEL
        DFMAX(I)     = MAX(DFMAX(I), DAMAGE(I))
        IF (SCALE_FLAG == 1) DMG_SCALE(I) = ONE - DFMAX(I)
      ENDDO           
c------------------------
c     print
c------------------------
      IF (NINDXD > 0) THEN    ! start damage
        DO J=1,NINDXD    
          I = INDXD(J)
          IF (S_FLAG == 3) THEN
#include  "lockon.inc"
            WRITE(IOUT, 2001) NGL(I),IPG,ILAY,IPT,EPS_MAX(I)
            WRITE(ISTDO,2001) NGL(I),IPG,ILAY,IPT,EPS_MAX(I)
#include  "lockoff.inc"
          ELSE
#include  "lockon.inc"
            WRITE(IOUT, 2002) NGL(I),IPG,ILAY,IPT,EPS_MAX(I)
            WRITE(ISTDO,2002) NGL(I),IPG,ILAY,IPT,EPS_MAX(I)
#include  "lockoff.inc"
          ENDIF
        END DO
      END IF              
c
      IF (NINDXF > 0) THEN    ! failure
        DO J=1,NINDXF    
          I = INDXF(J)
          IF (S_FLAG == 3) THEN
#include  "lockon.inc"
            WRITE(IOUT, 3001) NGL(I),IPG,ILAY,IPT,EPS_MAX(I)
            WRITE(ISTDO,3101) NGL(I),IPG,ILAY,IPT,TIME,EPS_MAX(I)
#include  "lockoff.inc"
          ELSE
            WRITE(IOUT, 3002) NGL(I),IPG,ILAY,IPT,EPS_MAX(I)
            WRITE(ISTDO,3102) NGL(I),IPG,ILAY,IPT,TIME,EPS_MAX(I)
          ENDIF
        END DO
      END IF              
c
      IF (NINDX1 > 0) THEN 
        DO J=1,NINDX1    
          I = INDX1(J)
#include  "lockon.inc"
          WRITE(IOUT, 4000) NGL(I),IPG,ILAY,IPT,EPS_MAX(I)
          WRITE(ISTDO,4100) NGL(I),IPG,ILAY,IPT,TIME,EPS_MAX(I)
#include  "lockoff.inc"
        END DO
      END IF              
c
      IF (NINDX2 > 0) THEN 
        DO J=1,NINDX2    
          I = INDX2(J)
#include  "lockon.inc"
          WRITE(IOUT, 5000) NGL(I),IPG,ILAY,IPT,EPS_MAX(I)
          WRITE(ISTDO,5100) NGL(I),IPG,ILAY,IPT,TIME,EPS_MAX(I)
#include  "lockoff.inc"
        END DO
      END IF              
c-----------------------------------------------
 2001 FORMAT(1X,'START DAMAGE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,/
     .       1X,'PRINCIPAL STRAIN=',G11.4)
 2002 FORMAT(1X,'START DAMAGE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,/
     .       1X,'EQUIVALENT STRAIN=',G11.4)
c---
 3001 FORMAT(1X,'FAILURE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,/
     .       1X,'PRINCIPAL STRAIN=',G11.4)
 3101 FORMAT(1X,'FAILURE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,
     .       1X,'AT TIME :',1PE12.4,/1X,'PRINCIPAL STRAIN=',G11.4)
 3002 FORMAT(1X,'FAILURE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,/
     .       1X,'EQUIVALENT STRAIN=',G11.4)
 3102 FORMAT(1X,'FAILURE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,
     .       1X,'AT TIME :',1PE12.4,/1X,'EQUIVALENT STRAIN=',G11.4)
 4000 FORMAT(1X,'FAILURE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,/
     .       1X,'1st PRINCIPAL STRAIN=',G11.4)
 4100 FORMAT(1X,'FAILURE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,
     .       1X,'AT TIME :',1PE12.4,/1X,'1st PRINCIPAL STRAIN=',G11.4)
 5000 FORMAT(1X,'FAILURE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,/
     .       1X,'2nd PRINCIPAL STRAIN=',G11.4)
 5100 FORMAT(1X,'FAILURE (TENS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,
     .       1X,'AT TIME :',1PE12.4,/1X, '2ndPRINCIPAL STRAIN=',G11.4)
c-----------------------------------------------
      RETURN
      END
