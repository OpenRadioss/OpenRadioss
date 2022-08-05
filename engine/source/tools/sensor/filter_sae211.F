Chd|====================================================================
Chd|  FILTER_SAE211                    source/tools/sensor/filter_sae211.F
Chd|-- called by -----------
Chd|        SENSOR_NIC                    source/tools/sensor/sensor_nic.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FILTER_SAE211(NPT    ,VAR   ,CA    ,CB   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPT
      my_real ,DIMENSION(NPT) :: VAR
      my_real ,DIMENSION(3)   :: CA, XX
      my_real ,DIMENSION(2)   :: CB,YY,ZZ
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,NPH,NP
      my_real :: X0,X1,X2,Y0,Y1,Y2,Z0,Z1,Z2
c=======================================================================
c      NPT = 2*NPH + 1
c      NPH = (NPT - 1) / 2
!      NP = NPT
!      NP = 10
c
c     Filter forward
c
      X0 = VAR(1)
      X2 = VAR(2)
      X3 = VAR(3)
      Y1 = VAR(4)
      Y2 = VAR(5)
      Z1 = VAR(6)
      Z2 = VAR(7)

      Y0 = CA(1)*X0 + CA(2)*X1 + CA(3)*X2
     .   + CB(1)*Y1 + CB(2)*Y2
c
c     Filter backward
c
      Z0 = CA(1)*YN + CA(2)*Y1 + CA(3)*Y2
     .   + CB(1)*Z1 + CB(2)*Z2
     
      VAR(1) = ZERO
      VAR(2) = X0
      VAR(3) = X1
      VAR(4) = Y0
      VAR(5) = Y1
      VAR(6) = Z0
      VAR(7) = Z1
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
