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
Chd|  SENSOR_DIST_SURF              source/tools/sensor/sensor_dist_surf.F
Chd|-- called by -----------
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        DIST_NODE_PLANE_3N            source/tools/sensor/dist_node_plane_3n.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_DIST_SURF(SENSOR ,X   ,IGRSURF)
!$COMMENT
!       SENSOR_DIST_SURF description
!       computation of distance to surface for sensor typ16 + plane defined by 3 nodes
!       and modification of the state of sensor
!       For the other surface, the distance computation is done in SENSOR_DIST_SURF0 routine
!       
!       SENSOR_DIST_SURF0 organization :
!       - computation of local distance for sensor 16 + plane defined by 3 nodes
!       - sensor state modification
!$ENDCOMMENT
c-----------------------------------------------
C   M o d u l e s
c-----------------------------------------------
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real ,DIMENSION(3,NUMNOD) :: X
      TYPE (SURF_) ,DIMENSION(NSURF), TARGET :: IGRSURF
      TYPE (SENSOR_STR_) :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
       INTEGER I,ISURF,INOD,NP1,NP2,NP3,NP4,NSEG,ICRIT,IFLAG
       my_real DIST,DMIN,DMAX,ALPHA,TDELAY,TMIN
       my_real XNOD,YNOD,ZNOD,XP1,YP1,ZP1,XP2,YP2,ZP2,XP3,YP3,ZP3,
     .    XP4,YP4,ZP4,NORM,INFINITY
      TYPE (SURF_) ,POINTER :: SURFACE
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
      IF (SENSOR%STATUS == 1) RETURN   ! already activated

C.....................................................
C ==> DIST between node and a plan defined by 3 nodes
C.....................................................
C     ID_N1 : Node identifier
C     ID_PN1 : Plan Node 1 identifier
C     ID_PN2 : Plan Node 2 identifier
C     ID_PN3 : Plan Node 3 identifier
C
      TMIN   = SENSOR%TMIN
      TDELAY = SENSOR%TDELAY      ! time delay before activation
      INOD   = SENSOR%IPARAM(1)
      ISURF  = SENSOR%IPARAM(2)
c
      DMIN   = SENSOR%RPARAM(1)
      DMAX   = SENSOR%RPARAM(2)
      ICRIT  = 0
      IFLAG  = 0
c
c...  Current position of reference node
c
      XNOD  = X(1,INOD)
      YNOD  = X(2,INOD)
      ZNOD  = X(3,INOD)
c    
c
      IF (ISURF == 0) THEN  ! calculate distance to plane defined by 3 nodes
c
        NP1 = SENSOR%IPARAM(3)
        NP2 = SENSOR%IPARAM(4)
        NP3 = SENSOR%IPARAM(5)
        XP1 = X(1,NP1)
        YP1 = X(2,NP1)
        ZP1 = X(3,NP1)
        XP2 = X(1,NP2)
        YP2 = X(2,NP2)
        ZP2 = X(3,NP2)
        XP3 = X(1,NP3)
        YP3 = X(2,NP3)
        ZP3 = X(3,NP3)
        CALL DIST_NODE_PLANE_3N(DIST,
     .       XNOD,YNOD,ZNOD,XP1,YP1,ZP1,XP2,YP2,ZP2,XP3,YP3,ZP3)
c
        SENSOR%VALUE = DIST
      END IF
      IF (SENSOR%VALUE <= DMIN) THEN
        ICRIT = 1
        IFLAG = 1
      ELSE IF (SENSOR%VALUE >= DMAX) THEN
        ICRIT = 1
        IFLAG = 2
      END IF
c----------------------------------------------------------------
c     check activation status
c----------------------------------------------------------------
      IF (SENSOR%TCRIT + TMIN > TT) THEN
        IF (ICRIT == 0) THEN
          SENSOR%TCRIT = INFINITY
        ELSE IF (SENSOR%TCRIT == INFINITY) THEN
          SENSOR%TCRIT = MIN(SENSOR%TCRIT, TT)
        END IF
      END IF
      IF (SENSOR%TSTART == INFINITY .and. SENSOR%TCRIT < INFINITY) THEN
        SENSOR%TSTART = SENSOR%TCRIT + TMIN + TDELAY      
      END IF
      IF (SENSOR%TSTART <= TT) THEN   ! sensor activation
        SENSOR%STATUS = 1
      END IF
c----------------------------------------------------------
      IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1100) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1100) SENSOR%SENS_ID,SENSOR%TSTART
        IF (IFLAG == 1) THEN
          WRITE (IOUT ,1200) DMIN,DIST
        ELSE IF (IFLAG == 2) THEN
          WRITE (IOUT ,1300) DMAX,DIST
        END IF
#include "lockoff.inc"
      END IF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5)
1200  FORMAT('      TARGET MIN DISTANCE TO PLAN = ',1PE12.5,/
     .       '      CURRENT DISTANCE AFTER TMIN and TDELAY = ',1PE12.5)
1300  FORMAT('      TARGET MAX DISTANCE TO PLAN = ',1PE12.5,/
     .       '      CURRENT DISTANCE AFTER TMIN and TDELAY = ',1PE12.5)
c----------------------------------------------------------
      RETURN
      END SUBROUTINE
