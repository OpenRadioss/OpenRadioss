Chd|====================================================================
Chd|  FILTER_SAE211                    source/tools/sensor/filter_sae211.F
Chd|-- called by -----------
Chd|        SENSOR_NIC                    source/tools/sensor/sensor_nic.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FILTER_SAE211(Y   ,DT   ,NPT    ,CA    ,CB   )
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
      my_real :: DT
      my_real ,DIMENSION(NPT) :: Y
      my_real ,DIMENSION(3)   :: CA
      my_real ,DIMENSION(2)   :: CB
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,NPH
      my_real :: X0,X1,X2,Y0,Y1,Y2,YF
c=======================================================================
c      NPT = 2*NPH + 1
      NPH = (NPT - 1) / 2
c
c     Set up pre-start array for forward filter - Inverted mirror
c
      YF = TWO * Y(1)
      X1 = YF - Y(NPT)
      X0 = YF - Y(NPT-1)
      Y1 = ZERO
      DO I = NPH+1, NPT
        Y1 = Y1 + Y(I)
      END DO
      Y1 = Y1 / NPH
      Y0 = Y1
c
      DO I = NPT-2,1,-1
        X2 = X1
        X1 = X0
        X0 = YF - Y(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = CA(1)*X0 + CA(2)*X1 + CA(3)*X2 + CB(1)*Y1 + CB(2)*Y2
      END DO
c
c     Filter forward
c
      DO I = 1,NPT
        X2 = X1
        X1 = X0
        X0 = Y(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = CA(1)*X0 + CA(2)*X1 + CA(3)*X2 + CB(1)*Y1 + CB(2)*Y2
        Y(I) = Y0
      END DO
c
c     Set up pre-start array for backward filter
c
      YF = TWO * Y(NPT-1)
      X1 = YF - Y(NPT)
      X0 = YF - Y(NPT-1)
      Y1 = ZERO
      DO I = 1,NPH
        Y1 = Y1 + Y(I)
      END DO
      Y1 = Y1 / NPH
      Y0 = Y1
c
      DO I = 3,NPT-2
        X2 = X1
        X1 = X0
        X0 = YF - Y(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = CA(1)*X0 + CA(2)*X1 + CA(3)*X2 + CB(1)*Y1 + CB(2)*Y2
      END DO
c
c     Filter backward
c
      DO I = NPT-1,1,-1
        X2 = X1
        X1 = X0
        X0 = Y(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = CA(1)*X0 + CA(2)*X1 + CA(3)*X2 + CB(1)*Y1 + CB(2)*Y2
        Y(I) = Y0
      END DO
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
