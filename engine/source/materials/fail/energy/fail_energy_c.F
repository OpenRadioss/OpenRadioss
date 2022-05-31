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
Chd|  FAIL_ENERGY_C                 source/materials/fail/energy/fail_energy_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_ENERGY_C(
     1     NEL      ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,
     2     UPARAM   ,UVAR     ,NPF      ,TF       ,TIME     , 
     3     NGL      ,IPG      ,ILAY     ,IPT      ,EPSP     ,
     4     EINT     ,AREA     ,THK      ,PTHKF    ,DMG_FLAG ,
     5     DMG_SCALE,OFF      ,FOFF     ,DFMAX    ,TDEL     )
C-----------------------------------------------
C    energy failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
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
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C EINT    | NEL     | F | R | INTERNAL ENERGY
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
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME
      my_real ,DIMENSION(NEL) ,INTENT(IN)    :: AREA,THK,EPSP,OFF
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL,2) ,INTENT(IN)  :: EINT
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) ::DMG_FLAG
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
      INTEGER :: I,J,NINDX
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real RFAC,R1,R2,IE_SP,DYDX,RIEF1,RIEF2,XFAC
      my_real DAMAGE(NEL)
c=======================================================================
      RIEF1    = UPARAM(1)             
      RIEF2    = UPARAM(2)           
      PTHKF    = UPARAM(3)
      XFAC     = UPARAM(4)           
      DMG_FLAG = INT(UPARAM(5))
      NINDX    = 0  
C-----------------------------------------------
      DO I = 1,NEL
        IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
          IF (IFUNC(1) /= 0) THEN
            RFAC = FINTER(IFUNC(1),EPSP(I)*XFAC,NPF,TF,DYDX)
            RFAC = MAX(RFAC,EM20)
          ELSE
            RFAC = ONE
          ENDIF  
          R1 = RIEF1*RFAC
          R2 = RIEF2*RFAC

          IE_SP = (EINT(I,1) + EINT(I,2))/AREA(I)/THK(I)

          IF (IE_SP > R1) THEN
            DAMAGE(I) = (IE_SP-R1)/(R2-R1)
            DAMAGE(I) = MIN(ONE,DAMAGE(I))
            DFMAX(I)  = MAX(DFMAX(I) ,DAMAGE(I))
          ENDIF
          IF (IE_SP > R2) THEN
            NINDX = NINDX + 1
            INDX(NINDX) = I
            FOFF(I) = 0
            TDEL(I) = TIME
          ENDIF                    
        ENDIF                    
      ENDDO
c
      IF (DMG_FLAG == 1) THEN
        DO I = 1,NEL
          DMG_SCALE(I) = ONE - DFMAX(I)
        ENDDO          
      ENDIF
c------------------------
      IF (NINDX > 0) THEN  
        DO J=1,NINDX       
          I = INDX(J)      
#include  "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc"
        END DO
      END IF              
c------------------------
 2000 FORMAT(1X,'FAILURE (ENERGY) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (ENERGY) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      RETURN
      END
