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
Chd|  FAIL_JOHNSON_C                source/materials/fail/johnson_cook/fail_johnson_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE FAIL_JOHNSON_C(
     1           NEL      ,NUPARAM  ,NUVAR    ,UPARAM   ,UVAR     ,
     2           TIME     ,NGL      ,IPG      ,ILAY     ,IPTT     ,
     3           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4           DPLA     ,EPSP     ,TSTAR    ,OFF      ,FOFF     ,
     5           PTHKF    ,DFMAX    ,TDEL     )
C-----------------------------------------------
c    Johnson-Cook failure model
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
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
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | R | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | R | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C DPLA    | NEL     | F | R | PLASTIC STRAIN
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C PTHKF   |  1      | F | W | PERCENTAGE OF LAYER THICKNESS TO FAIL
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPTT
      INTEGER ,DIMENSION(NEL), INTENT(IN) :: NGL
      my_real, INTENT(IN) :: TIME
      my_real ,DIMENSION(NUPARAM), INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL), INTENT(IN)  :: DPLA,EPSP,TSTAR,OFF,
     .         SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL), INTENT(INOUT) :: FOFF
      my_real :: PTHKF
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: TDEL
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IFAIL_SH,NINDX
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real :: D1,D2,D3,D4,D5,EPSP0,P,EPSF,SVM
C=======================================================================
      NINDX   = 0  
      D1      = UPARAM(1)
      D2      = UPARAM(2)
      D3      = UPARAM(3)
      D4      = UPARAM(4)
      D5      = UPARAM(5)
      EPSP0   = UPARAM(6)
      IFAIL_SH  = INT(UPARAM(7))
c-----------------------------
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = ONE
      ENDIF
c-----------------------------
      DO I=1,NEL
        IF (OFF(I) == ONE .and. FOFF(I) == 1 .and. DPLA(I) > ZERO) THEN
          P   = THIRD*(SIGNXX(I) + SIGNYY(I))
          SVM = SQRT (SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)
     .              - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))
          EPSF = D3*P/MAX(EM20,SVM)
          EPSF = (D1 + D2*EXP(EPSF))
          IF(D4/=ZERO) EPSF = EPSF * (ONE + D4*LOG(MAX(ONE,EPSP(I)/EPSP0))) ! if d4=0, epsp is not correctly defined
          IF(D5/=ZERO) EPSF = EPSF * (ONE + D5*TSTAR(I)) ! if d5=0, tstart is not correctly defined
          IF (EPSF > ZERO) DFMAX(I) = DFMAX(I) + DPLA(I)/EPSF  
          IF (DFMAX(I) >= ONE) THEN
            NINDX = NINDX + 1
            INDX(NINDX) = I  
            FOFF(I) = 0
            TDEL(I) = TIME
          ENDIF
        ENDIF 
      ENDDO      
c------------------------
      IF (NINDX > 0) THEN        
        DO J=1,NINDX             
          I = INDX(J)            
#include "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPTT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPTT,TIME
#include "lockoff.inc" 
        END DO                   
      END IF   ! NINDX             
c------------------
c     Damage for output  0 < DFMAX < 1
c------------------
      DO I=1,NEL
        DFMAX(I) = MIN(ONE,DFMAX(I))
      ENDDO
c------------------
 2000 FORMAT(1X,'FAILURE (JC) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',LAYER',I3,
     .       1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (JC) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',LAYER',I3,
     .       1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------
      RETURN
      END
