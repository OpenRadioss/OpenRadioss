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
!||    sensor_dist_surf0   ../engine/source/tools/sensor/sensor_dist_surf0.F
!||--- called by ------------------------------------------------------
!||    resol               ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    dist_node_seg3n     ../engine/source/tools/sensor/dist_node_seg3n.F
!||    dist_node_seg4n     ../engine/source/tools/sensor/dist_node_seg4n.F
!||--- uses       -----------------------------------------------------
!||    groupdef_mod        ../common_source/modules/groupdef_mod.F
!||    sensor_mod          ../common_source/modules/sensor_mod.F90
!||    spmd_mod            ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      SUBROUTINE SENSOR_DIST_SURF0(NSENSOR,SENSOR_TAB,X,IGRSURF,COMM_SENS16)
!$COMMENT
!       SENSOR_DIST_SURF0 description
!       computation of distance to surface for sensor typ16
!       and reduction with a mpi communication
!       
!       SENSOR_DIST_SURF0 organization :
!       - computation of local distance
!       - reduction with mpi comm
!$ENDCOMMENT
c-----------------------------------------------
C   M o d u l e s
c-----------------------------------------------
      USE SPMD_MOD
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
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      my_real ,DIMENSION(3,NUMNOD) :: X
      TYPE (SURF_) ,DIMENSION(NSURF), TARGET :: IGRSURF
      TYPE (SENSOR_STR_), DIMENSION(NSENSOR),INTENT(INOUT) :: SENSOR_TAB
      TYPE (SENSOR_COMM), INTENT(IN) :: COMM_SENS16
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
       INTEGER I,ISURF,INOD,NP1,NP2,NP3,NP4,NSEG,IJK
       my_real DIST,DMIN,DMAX,ALPHA,TMIN,TDELAY
       my_real XNOD,YNOD,ZNOD,XP1,YP1,ZP1,XP2,YP2,ZP2,XP3,YP3,ZP3,
     .    XP4,YP4,ZP4,NORM,INFINITY
      TYPE (SURF_) ,POINTER :: SURFACE
      PARAMETER (INFINITY = 1.0E20)
      INTEGER :: ISENS
      my_real, DIMENSION(COMM_SENS16%NUM_SENS) :: LOCAL_VALUE,GLOBAL_VALUE
!     -----------------------------------------------------------

      ! -----------------
      ! computation of local distance (one distance per processor)
      DO IJK=1,COMM_SENS16%NUM_SENS
        ISENS = COMM_SENS16%ID_SENS(IJK)
        LOCAL_VALUE(IJK) = ZERO
        GLOBAL_VALUE(IJK) = ZERO
        IF (SENSOR_TAB(ISENS)%STATUS == 1) CYCLE   ! already activated

C.....................................................
C ==> DIST between node and a plan defined by 3 nodes
C.....................................................
C     ID_N1 : Node identifier
C     ID_PN1 : Plan Node 1 identifier
C     ID_PN2 : Plan Node 2 identifier
C     ID_PN3 : Plan Node 3 identifier
C
        TMIN   = SENSOR_TAB(ISENS)%TMIN        ! min criterion time duration
        TDELAY = SENSOR_TAB(ISENS)%TDELAY      ! time delay before activation
        INOD   = SENSOR_TAB(ISENS)%IPARAM(1)
        ISURF  = SENSOR_TAB(ISENS)%IPARAM(2)
c
        DMIN   = SENSOR_TAB(ISENS)%RPARAM(1)
        DMAX   = SENSOR_TAB(ISENS)%RPARAM(2)
c
c...  Current position of reference node
c
        XNOD  = X(1,INOD)
        YNOD  = X(2,INOD)
        ZNOD  = X(3,INOD)
c    
c
      ! calculate distance to Surf_Id composed of segments
c
        SURFACE => IGRSURF(ISURF)
        NSEG = SURFACE%NSEG
c--------------------
        SELECT CASE (SURFACE%TYPE)
c--------------------
          CASE (2)  ! SOLIDS
c
c           to be completed
c
          CASE (3)  ! SH4N
c
            DO I = 1,NSEG
              NP1 = SURFACE%NODES(I,1)
              NP2 = SURFACE%NODES(I,2)
              NP3 = SURFACE%NODES(I,3)
              NP4 = SURFACE%NODES(I,4)
              XP1 = X(1,NP1)
              YP1 = X(2,NP1)
              ZP1 = X(3,NP1)
              XP2 = X(1,NP2)
              YP2 = X(2,NP2)
              ZP2 = X(3,NP2)
              XP3 = X(1,NP3)
              YP3 = X(2,NP3)
              ZP3 = X(3,NP3)
              XP4 = X(1,NP4)
              YP4 = X(2,NP4)
              ZP4 = X(3,NP4)
              CALL DIST_NODE_SEG4N(
     .             DIST,DMIN,DMAX,XNOD,YNOD,ZNOD, 
     .             XP1,YP1,ZP1,XP2,YP2,ZP2,XP3,YP3,ZP3,XP4,YP4,ZP4)
              SENSOR_TAB(ISENS)%VALUE = MIN(SENSOR_TAB(ISENS)%VALUE, DIST)
            END DO            
c
          CASE (7)  ! SH3N          
c
            DO I = 1,NSEG
              NP1 = SURFACE%NODES(I,1)
              NP2 = SURFACE%NODES(I,2)
              NP3 = SURFACE%NODES(I,3)
              XP1 = X(1,NP1)
              YP1 = X(2,NP1)
              ZP1 = X(3,NP1)
              XP2 = X(1,NP2)
              YP2 = X(2,NP2)
              ZP2 = X(3,NP2)
              XP3 = X(1,NP3)
              YP3 = X(2,NP3)
              ZP3 = X(3,NP3)
c
              CALL DIST_NODE_SEG3N(
     .             DIST,DMIN,DMAX,XNOD,YNOD,ZNOD, 
     .             XP1,YP1,ZP1,XP2,YP2,ZP2,XP3,YP3,ZP3)
              SENSOR_TAB(ISENS)%VALUE = MIN(SENSOR_TAB(ISENS)%VALUE, DIST)
            END DO            
c
c--------------------
        END SELECT
c--------------------
        LOCAL_VALUE(IJK) = SENSOR_TAB(ISENS)%VALUE
      ENDDO
      ! -----------------
      ! reduction with mpi comm
      IF(NSPMD>1) THEN
        CALL SPMD_ALLREDUCE(LOCAL_VALUE,GLOBAL_VALUE,COMM_SENS16%NUM_SENS,SPMD_MIN)
      ELSE
        GLOBAL_VALUE(1:COMM_SENS16%NUM_SENS) = LOCAL_VALUE(1:COMM_SENS16%NUM_SENS)
      ENDIF
      DO IJK=1,COMM_SENS16%NUM_SENS
        ISENS = COMM_SENS16%ID_SENS(IJK)
        SENSOR_TAB(ISENS)%VALUE = GLOBAL_VALUE(IJK)
      ENDDO
      ! -----------------

      RETURN
      END SUBROUTINE

