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
Chd|  FAIL_FABRIC_C                 source/materials/fail/fabric/fail_fabric_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_FABRIC_C(
     1     NEL      ,NGL      ,NUPARAM  ,NUVAR    ,NFUNC    ,
     2     UPARAM   ,UVAR     ,IFUNC    ,TIME     ,TIMESTEP ,
     3     NPF      ,TF       ,DEPS1    ,DEPS2    ,EPS1     ,
     4     EPS2     ,SIG1     ,SIG2     ,DFMAX    ,TDEL     ,
     5     IPG      ,ILAY     ,IPT      ,OFF      ,FOFF     )
C-----------------------------------------------
c    Anisotropic fabric tension strain failure
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C---------+---------+---+---+--------------------------------------------
C EPS1    | NEL     | F | R | STRAIN IN 1st FIBER DIRECTION
C EPS2    | NEL     | F | R | STRAIN IN 2nd FIBER DIRECTION
C DEPS1   | NEL     | F | R | STRAIN INCREMENT IN 1st FIBER DIRECTION
C DEPS2   | NEL     | F | R | STRAIN INCREMENT IN 2nd FIBER DIRECTION
C SIG1    | NEL     | F | W | STRESS IN 1st FIBER DIRECTION
C SIG2    | NEL     | F | W | STRESS IN 2nd FIBER DIRECTION
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
C---------+---------+---+---+--------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT
      INTEGER ,DIMENSION(NEL), INTENT(IN) :: NGL
      my_real, INTENT(IN) :: TIME,TIMESTEP
      my_real ,DIMENSION(NUPARAM), INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL), INTENT(IN)  :: DEPS1,DEPS2,EPS1,EPS2,OFF
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL), INTENT(INOUT) :: FOFF
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: DFMAX,SIG1,SIG2
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: TDEL
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX1,NINDX2,NDIR
      my_real :: XFAC,RF1,RR1,RF2,RR2,DYDX,EPSR1,EPSR2,EPSF1,EPSF2,DMG1,DMG2
      INTEGER ,DIMENSION(NEL) :: INDX1,INDX2
      my_real ,DIMENSION(NEL) :: RFAC1,RFAC2,EPSP1,EPSP2
c-----------------------------------------------------------------------
c     UVAR(1) = stress reduction factor in 1st fiber direction
c     UVAR(2) = stress reduction factor in 2nd fiber direction
C=======================================================================
      NINDX1 = 0
      NINDX2 = 0
      EPSF1 = UPARAM(1)
      EPSR1 = UPARAM(2)
      EPSF2 = UPARAM(3)
      EPSR2 = UPARAM(4)
      XFAC  = UPARAM(5)
      NDIR  = NINT(UPARAM(6))
C-------------------
C     STRAIN 
C-------------------
      IF (IFUNC(1) > 0) THEN   ! strain rate dependency
        DO I=1,NEL
          EPSP1(I) = XFAC * DEPS1(I) / MAX(TIMESTEP,EM20)
          EPSP2(I) = XFAC * DEPS2(I) / MAX(TIMESTEP,EM20)
          RFAC1(I) = FINTER(IFUNC(1),EPSP1(I),NPF,TF,DYDX)
          RFAC1(I) = MAX(RFAC1(I),EM20)
          RFAC2(I) = FINTER(IFUNC(1),EPSP2(I),NPF,TF,DYDX)
          RFAC2(I) = MAX(RFAC2(I),EM20)
        ENDDO
      ELSE
        RFAC1(1:NEL) = ONE
        RFAC2(1:NEL) = ONE
      ENDIF
c
      DO I=1,NEL
        DMG1 = ZERO
        DMG2 = ZERO
        RF1  = EPSF1*RFAC1(I)
        RR1  = EPSR1*RFAC1(I)
        RF2  = EPSF2*RFAC2(I)
        RR2  = EPSR2*RFAC2(I)
        IF (EPS1(I) > RF1) DMG1 = MIN(ONE, (EPS1(I)-RF1)/(RR1-RF1))
        IF (EPS2(I) > RF2) DMG2 = MIN(ONE, (EPS2(I)-RF2)/(RR2-RF2))
c
        IF (UVAR(I,1) == ZERO .and. DMG1 > ZERO) THEN
          NINDX1 = NINDX1 + 1
          INDX1(NINDX1) = I
        ENDIF
        IF (UVAR(I,2) == ZERO .and. DMG2 > ZERO) THEN
          NINDX2 = NINDX2 + 1
          INDX2(NINDX2) = I
        ENDIF
        UVAR(I,1) = MAX(UVAR(I,1), DMG1)
        UVAR(I,2) = MAX(UVAR(I,2), DMG2)
        IF (UVAR(I,1)>ZERO .and. SIG1(I)>ZERO) SIG1(I) = SIG1(I)*(ONE-UVAR(I,1))
        IF (UVAR(I,2)>ZERO .and. SIG2(I)>ZERO) SIG2(I) = SIG2(I)*(ONE-UVAR(I,2))
        IF (NDIR == 2) THEN 
          IF (UVAR(I,1) == ONE .AND. UVAR(I,2) == ONE) THEN
            FOFF(I) = 0
            TDEL(I) = TIME                      
          ENDIF
        ELSE
          IF (UVAR(I,1) == ONE .OR. UVAR(I,2) == ONE) THEN
            FOFF(I) = 0
            TDEL(I) = TIME                      
          ENDIF
        ENDIF
      ENDDO
c
c---  Output of Maximum Damage : 0 < DFMAX < 1
      DO I=1,NEL
        DFMAX(I) = MAX(DFMAX(I), UVAR(I,1))
        DFMAX(I) = MAX(DFMAX(I), UVAR(I,2))
      ENDDO           
c----------------------------------------------
c     Output
c----------------------------------------------
      DO J=1,NINDX1
        I = INDX1(J)
        IF (UVAR(I,1) > ZERO) THEN
#include  "lockon.inc"                       
          WRITE(IOUT, 1100) NGL(I),IPG,ILAY,IPT,TIME
          WRITE(ISTDO,1100) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc"                      
        ENDIF
      ENDDO
      DO J=1,NINDX2
        I = INDX2(J)
        IF (UVAR(I,2) > ZERO) THEN
#include  "lockon.inc"                       
          WRITE(IOUT, 2100) NGL(I),IPG,ILAY,IPT,TIME
          WRITE(ISTDO,2200) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc"                      
        ENDIF
      ENDDO
C-----------------------------------------------
 1100 FORMAT(1X,'START DAMAGE (FABRIC) OF FIBER 1, ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
 1200 FORMAT(1X,'START DAMAGE (FABRIC) OF FIBER 2, ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
 2100 FORMAT(1X,'START DAMAGE (FABRIC) OF FIBER 1, ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
 2200 FORMAT(1X,'START DAMAGE (FABRIC) OF FIBER 2, ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
 3000 FORMAT(1X,'FAILURE (FABRIC) OF ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
C-----------------------------------------------
      RETURN
      END
